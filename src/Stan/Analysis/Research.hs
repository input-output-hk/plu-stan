-- | Bounded, intramodule detection of the Cardano research patterns.
-- HIE names retain binder identity. Only reachable expressions are expanded;
-- comments, strings and unused let bindings cannot supply validation evidence.
module Stan.Analysis.Research (researchFindings, stripNonCode) where

import qualified Data.Array as Arr
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS
import Stan.Ghc.Compat (Name, RealSrcSpan, nameOccName, occNameString, isExternalName,
    nameModule, mkFastString, srcSpanStartCol, IfaceTyCon (..))
import Stan.Core.ModuleName (fromGhcModule)
import Stan.Hie (slice)
import Stan.NameMeta (NameMeta (..), compareNames)
import Stan.Hie.Compat (HieFile (..), HieAST (..), HieASTs (..), TypeIndex,
    HieType (..), NodeInfo (..), IdentifierDetails (..), ContextInfo (..), nodeInfo,
    mkNodeAnnotation, toNodeAnnotation)

-- Unknown syntax remains explicit: it is never interpreted as validation.
data Expr = Ref Name | Lit ByteString | Call Expr [Expr] | Form String [Expr]
    deriving stock (Eq)
type Ast = HieAST TypeIndex
type Env = Map Name Expr
type Defs = Map Name Ast

ann :: String -> String -> Ast -> Bool
ann a b = Set.member (mkNodeAnnotation (mkFastString a) (mkFastString b))
    . Set.map toNodeAnnotation . nodeAnnotations . nodeInfo

nodes :: Ast -> [Ast]
nodes n = n : concatMap nodes (nodeChildren n)

names :: Ast -> [Name]
names n = [v | (Right v, d) <- Map.toList (nodeIdentifiers (nodeInfo n)),
    any useful (Set.toList (identInfo d))]
  where
    useful Use = True
    useful MatchBind = True
    useful ValBind{} = True
    useful PatternBind{} = True
    useful RecField{} = True
    useful _ = False

bindingName :: Ast -> Maybe Name
bindingName n = do
    c <- viaNonEmpty head (nodeChildren n)
    viaNonEmpty head (names c)

patterns :: Ast -> [Ast]
patterns = filter isPattern . nodeChildren
  where isPattern n = any (\tag -> ann tag "Pat" n)
            ["VarPat", "ConPat", "TuplePat", "WildPat", "ParPat", "BangPat", "AsPat", "ListPat"]

bindPattern :: Ast -> Expr -> Env
bindPattern pat value
    | ann "TuplePat" "Pat" pat, Form "syntax" xs <- value, length xs == length (nodeChildren pat) =
        Map.unions (zipWith bindPattern (nodeChildren pat) xs)
    | ann "VarPat" "Pat" pat = Map.fromList [(n, value) | n <- names pat]
    | otherwise = Map.unions
        [bindPattern p (Form ("field:" <> show i) [value])
        | (i,p) <- zip [0 :: Int ..] (nodeChildren pat)]

-- A known library name, not an unrelated local function with the same spelling.
isName :: String -> Expr -> Bool
isName wanted (Ref n) = occNameString (nameOccName n) == wanted && isExternalName n
    && any (\package -> compareNames (NameMeta { nameMetaPackage = package, nameMetaModuleName = fromGhcModule (nameModule n), nameMetaName = toText wanted }) n)
        ["plutus-ledger-api", "plutus-tx", "base", "ghc-prim", "ghc-internal", "containers", "bytestring"]
isName _ _ = False

isCall :: String -> Expr -> Bool
isCall s (Call f _) = isName s f
isCall _ _ = False

argsOf :: String -> Expr -> [[Expr]]
argsOf s e = [args | Call f args <- expressions e, isName s f]

children :: Expr -> [Expr]
children (Call f xs) = f:xs
children (Form _ xs) = xs
children _ = []

expressions :: Expr -> [Expr]
expressions e = e : concatMap expressions (children e)

has :: String -> Expr -> Bool
has s = any (isName s) . expressions

app :: Expr -> [Expr] -> Expr
app (Call f xs) ys = Call f (xs <> ys)
app f xs = Call f xs

normalise :: HieFile -> Defs -> Env -> Set Name -> Int -> Ast -> Expr
normalise hie defs env seen fuel n
    | fuel <= 0 = Form "budget" []
    | ann "HsPar" "HsExpr" n = oneChild
    | ann "OpApp" "HsExpr" n, [a,op,b] <- cs = apply (go op) [go a, go b]
    | ann "HsApp" "HsExpr" n, [f,x] <- cs = apply (go f) [go x]
    | ann "HsLet" "HsExpr" n = maybe (Form "unknown-let" []) go (viaNonEmpty last cs)
    | ann "HsIf" "HsExpr" n = Form "if" (map go cs)
    | ann "HsCase" "HsExpr" n, scrut:rest <- cs =
        let s = go scrut
            alternatives = concatMap matches rest
        in Form "case" (s : [match [s] m | m <- alternatives])
    | ann "HsLam" "HsExpr" n = Form "lambda" (Form "parameters" (map patExpr (patterns n)) : map go (filter (ann "GRHS" "GRHS") cs))
    | ann "GRHS" "GRHS" n = case map go (filter (not . isBinds) cs) of
        [x] -> x
        xs -> Form "guard" xs
    | ann "HsOverLit" "HsExpr" n || ann "HsLit" "HsExpr" n =
        Lit (fromMaybe "" (slice (nodeSpan n) (hie_hs_src hie)))
    | ann "HsVar" "HsExpr" n || ann "HsRecSel" "HsExpr" n =
        maybe (Form "unknown-name" []) resolve (viaNonEmpty head (names n))
    | otherwise = case map go (filter (not . isBinds) cs) of
        [x] -> x
        xs -> Form "syntax" xs
  where
    patExpr p
        | ann "WildPat" "Pat" p = Form "wildcard" []
        | ann "VarPat" "Pat" p = Form "binder" (map Ref (names p))
        | otherwise = Form "pattern" (map patExpr (nodeChildren p))
    cs = nodeChildren n
    go = normalise hie defs env seen (fuel-1)
    oneChild = maybe (Form "empty" []) go (viaNonEmpty head cs)
    isBinds a = ann "HsValBinds" "HsLocalBindsLR" a || ann "FunBind" "HsBindLR" a
    matches a | ann "Match" "Match" a = [a]
              | otherwise = concatMap matches (nodeChildren a)
    resolve v = case Map.lookup v env of
        Just e -> e
        Nothing -> case Map.lookup v defs of
            Just d | null (patterns d) && Set.notMember v seen -> expand v d []
            _ -> Ref v
    apply f xs
        | isName "$" f, [a,b] <- xs = apply a [b]
        | Call op [a] <- f, isName "$" op, [b] <- xs = apply a [b]
        | otherwise = case app f xs of
            Call (Ref v) ys | Just d <- Map.lookup v defs,
                not (null (patterns d)), length (patterns d) == length ys,
                Set.notMember v seen -> expand v d ys
            result -> result
    expand v d ys =
        let env' = Map.unions (zipWith bindPattern (patterns d) ys) <> env
            bodies = filter (ann "GRHS" "GRHS") (nodeChildren d)
            run = normalise hie defs env' (Set.insert v seen) (fuel-1)
        in case map run bodies of
            [e] -> e
            es -> Form "alternatives" es
    match ys m =
        let env' = Map.unions (zipWith bindPattern (patterns m) ys) <> env
            bodies = filter (ann "GRHS" "GRHS") (nodeChildren m)
            patNames = [Ref v | p <- patterns m, a <- nodes p, v <- names a, isExternalName v]
        in Form "branch" (Form "pattern" patNames :
            map (normalise hie defs env' seen (fuel-1)) bodies)

-- Facts required by a successful boolean result. A fact in only one arm of
-- an OR/conditional is not a global guard; unused bindings never reach here.
required :: Expr -> [Expr]
required e
    | Call f [a,b] <- e, isName "&&" f = required a <> required b
    | Call f [a,b] <- e, isName "||" f = filter (`elem` required b) (required a)
    | Form "if" [c,t,f] <- e, isFalse f = required c <> required t
    | Form "if" [_c,t,f] <- e = filter (`elem` required f) (required t)
    | Form "case" (_:bs) <- e = common (map required (filter (not . rejecting) bs))
    | Form "branch" (_:xs) <- e = concatMap required xs
    | Form "guard" xs <- e = concatMap required xs
    | otherwise = [e]
  where
    common [] = []
    common (xs:xss) = foldl' (\acc ys -> filter (`elem` ys) acc) xs xss
    rejecting = isFalse

-- An expression that can only reject: the constant 'False', a throwing
-- call, or a form whose every reachable arm rejects. Only the root is
-- classified; a throwing call nested inside an otherwise accepting arm (an
-- @else traceError@ guarding a check) does not turn that arm into a
-- rejection, so its facts stay in play and its siblings are not pruned.
isFalse :: Expr -> Bool
isFalse = \case
    e@Ref{} -> isName "False" e
    Call f [a,b]
        | isName "&&" f -> isFalse a || isFalse b
        | isName "||" f -> isFalse a && isFalse b
    e@Call{} -> isCall "traceError" e || isCall "error" e
    Form "if" [_,t,f] -> isFalse t && isFalse f
    Form "case" (_:bs) -> not (null bs) && all isFalse bs
    Form "branch" (_:xs) -> not (null xs) && all isFalse xs
    Form "alternatives" xs -> not (null xs) && all isFalse xs
    Form "guard" xs -> maybe False isFalse (viaNonEmpty last xs)
    _ -> False

comparison :: Expr -> Maybe (String, Expr, Expr)
comparison (Call f [a,b]) = do
    op <- find (`isName` f) ["==", "/=", "<", "<=", ">", ">=", "equalsInteger", "equalsData"]
    pure (op,a,b)
comparison _ = Nothing

comparisons :: Expr -> [(String,Expr,Expr)]
comparisons = mapMaybe comparison . expressions

checks :: Expr -> [(String,Expr,Expr)]
checks = mapMaybe comparison . required

-- Expressions sharing a variable retain the same GHC Name through aliases.
mentions :: Expr -> Expr -> Bool
mentions needle = elem needle . expressions

field :: String -> Expr -> Expr -> Bool
field name output e = [output] `elem` argsOf name e

outputArgs :: Expr -> [Expr]
outputArgs e = unique [x | s <- ["txOutAddress","txOutValue","txOutDatum","txOutReferenceScript"],
    [x] <- argsOf s e]
  where unique = foldr (\x xs -> if x `elem` xs then xs else x:xs) []

-- Access alone does not validate a datum. Projected data must reach a check,
-- or a constructor must be constrained by a case with a rejecting alternative.
fieldChecked :: String -> Expr -> Expr -> Bool
fieldChecked s out e = any (\(_,a,b) -> field s out a || field s out b) (checks e)
    || any caseCheck (expressions e)
  where
    caseCheck (Form "case" (scrut:bs)) = field s out scrut &&
        any rejects bs && any validates bs
    caseCheck _ = False
    rejects (Form "branch" (_:xs)) = any isFalse xs
    rejects _ = False
    validates (Form "branch" (Form "pattern" ps:xs)) =
        (s /= "txOutDatum" && not (null ps) && not (all isFalse xs))
        || any (any (\(_,a,b) -> mentionsOrigin scrutOrigin a || mentionsOrigin scrutOrigin b) . checks) xs
    validates _ = False
    scrutOrigin = [Call (Ref n) [out] | Ref n <- expressions e, isName s (Ref n)]
    mentionsOrigin origins x = any (`mentions` x) origins

missingOutput :: String -> Expr -> Bool
missingOutput s e = any missing (outputArgs e)
  where
    missing out = applicable out && not (fieldChecked s out e)
    applicable out = any (\(_,a,b) -> any (\f -> field f out a || field f out b)
        ["txOutAddress","txOutValue","txOutDatum","txOutReferenceScript"]) (checks e)
        && not (has "txInInfoResolved" out)

missingStake :: Expr -> Bool
missingStake e = any bad (outputArgs e)
  where
    bad out = fieldChecked "txOutAddress" out e
        && not (any (\(_,a,b) -> complete out a || complete out b) (checks e))
        && not (any (explicitNoStake out) (expressions e))
    explicitNoStake out (Form "case" (address:bs)) = field "txOutAddress" out address &&
        any (\case Form "branch" (p:_) -> has "Address" p && has "Nothing" p; _ -> False) bs
    explicitNoStake _ _ = False
    complete out x = field "txOutAddress" out x &&
        (isCall "txOutAddress" x || has "addressStakingCredential" x || any projectedStake (expressions x))
      where
        projectedStake (Form "field:2" [address]) = isCall "txOutAddress" address && field "txOutAddress" out address
        projectedStake _ = False

researchFindings :: HieFile -> [(Text, RealSrcSpan)]
researchFindings hie = precisionFindings hie <> concatMap inspect tops
  where
    allNodes = concatMap nodes (Map.elems (getAsts (hie_asts hie)))
    binds = [n | n <- allNodes, ann "FunBind" "HsBindLR" n,
                 ann "Match" "Match" n]
    defs = Map.fromList [(v,n) | n <- binds, Just v <- [bindingName n]]
    tops = [n | n <- binds, srcSpanStartCol (nodeSpan n) == 1]
    inspect n = case bindingName n of
        Nothing -> []
        Just v ->
            let bodies = filter (ann "GRHS" "GRHS") (nodeChildren n)
                es = map (normalise hie defs Map.empty (one v) 80) bodies
                sp = maybe (nodeSpan n) nodeSpan (viaNonEmpty head (nodeChildren n))
            in [(pid,sp) | (pid,predicate) <- rules, any predicate es]
                <> [(pid,sp) | emptyOutputValidation n es, pid <- ["PLU-STAN-28", "PLU-STAN-30", "PLU-STAN-32"]]
                <> [("PLU-STAN-40",sp) | not (null (patterns n)),
                    any (helperShape . normalise hie Map.empty Map.empty Set.empty 80) bodies]
    -- An explicitly typed output predicate that accepts without inspecting
    -- any output field is still validation; there is no three-field gate.
    -- An output that reaches a call this module cannot expand (an imported
    -- or multi-clause checker) is unknown rather than uninspected, so a
    -- forwarding wrapper is left alone: only a body that never mentions the
    -- output at all is known to accept without looking at it.
    emptyOutputValidation n es =
        any (any (typed "TxOut") . nodes) (patterns n)
        && any (typedResult "Bool") (take 1 (nodeChildren n))
        && any (\e -> null (outputArgs e) && not (isFalse e)
                && not (any (`mentions` e) outputs)) es
      where
        outputs = [Ref v | p <- patterns n, a <- nodes p, typed "TxOut" a, v <- names a]
    typed target n = any (typeIs target) (mapMaybe identType (Map.elems (nodeIdentifiers (nodeInfo n))))
    typedResult target n = any (resultIs target) (mapMaybe identType (Map.elems (nodeIdentifiers (nodeInfo n))))
    typeIs target ix = case hie_types hie Arr.! ix of
        HTyConApp IfaceTyCon{ifaceTyConName = v} _ -> occNameString (nameOccName v) == target
        _ -> False
    resultIs target ix = case hie_types hie Arr.! ix of
        HFunTy _ _ result -> resultIs target result
        HForAllTy _ result -> resultIs target result
        HQualTy _ result -> resultIs target result
        _ -> typeIs target ix
    recordFields =
        [[v | fld <- nodes decl, ann "ConDeclField" "ConDeclField" fld,
              c <- take 1 (nodeChildren fld),
              (Right v,d) <- Map.toList (nodeIdentifiers (nodeInfo c)), isExternalName v, isField d]
        | decl <- allNodes, ann "DataDecl" "TyClDecl" decl]
    isField d = any (\case RecField{} -> True; _ -> False) (identInfo d)
    rules =
        [ ("PLU-STAN-26", zipUnguarded)
        , ("PLU-STAN-28", missingOutput "txOutAddress")
        , ("PLU-STAN-29", missingStake)
        , ("PLU-STAN-30", missingOutput "txOutReferenceScript")
        , ("PLU-STAN-31", missingOutput "txOutDatum")
        , ("PLU-STAN-32", trashTokens)
        , ("PLU-STAN-33", uncheckedRedeemer)
        , ("PLU-STAN-34", readOnlySpend recordFields)
        , ("PLU-STAN-35", validityRange)
        , ("PLU-STAN-36", datumComparison)
        , ("PLU-STAN-37", incompleteToken)
        , ("PLU-STAN-38", strictValue)
        , ("PLU-STAN-39", unvalidatedIndex)
        , ("PLU-STAN-40", const False) -- definition-shape rule added below
        , ("PLU-STAN-41", fixedMap)
        ]

trashTokens :: Expr -> Bool
trashTokens e = missingOutput "txOutValue" e ||
    any (\x -> (isCall "leq" x || isCall "geq" x) && has "txOutValue" x && not (bounded x)) (expressions e) ||
    any (\(op,a,b) -> op `elem` [">=","<="] &&
        (has "assetClassValueOf" a || has "assetClassValueOf" b) && not (bounded a || bounded b)) (checks e)
  where
    bounded value = any (\(op,a,_) -> op `elem` ["<=","=="] && has "length" a &&
        any (`elem` argsOf "txOutValue" value) (argsOf "txOutValue" a)) (checks e)

-- Check the redeemer of the actual script input, not a token elsewhere in
-- the definition. Lambda bodies and case alternatives are separate contexts.
uncheckedRedeemer :: Expr -> Bool
uncheckedRedeemer e = has "txInfoInputs" e && any unchecked contexts
  where
    contexts = e : [b | Form "lambda" (_:bs) <- expressions e, b <- bs]
    unchecked body = any (missing body) scriptInputs
      where
        scriptInputs = [i | Form "case" (scrut:branches) <- expressions body,
            any (has "ScriptCredential") branches, [i] <- argsOf "txInInfoResolved" scrut]
    missing body input = not (any (valid input) (contextFacts body))
    valid input fact = any (lookupFor input) (expressions fact)
        && (isJust (comparison fact) || case fact of Form "case" _ -> True; _ -> False)
    lookupFor input (Call f [purpose,redeemers]) = isName "lookup" f
        && any (`elem` argsOf "txInfoInputs" e) (argsOf "txInfoRedeemers" redeemers)
        && [input] `elem` argsOf "txInInfoOutRef" purpose && has "Spending" purpose
    lookupFor _ _ = False
    contextFacts body = required body <>
        [fact | Form "branch" (_:bs) <- expressions body, b <- bs, fact <- required b]

readOnlySpend :: [[Name]] -> Expr -> Bool
readOnlySpend schemas e = any direct equalities || any allFields schemas
  where
    equalities = [(a,b) | ("==",a,b) <- checks e]
    opposite a b =
        (has "txInInfoResolved" a && not (has "txInInfoResolved" b)) ||
        (has "txInInfoResolved" b && not (has "txInInfoResolved" a))
    direct (a,b) = isCall "txOutDatum" a && isCall "txOutDatum" b && opposite a b
    fieldPairs fieldName = [(a,b) | (Call (Ref f) [a],Call (Ref g) [b]) <- equalities,
        f == fieldName, g == fieldName, has "txOutDatum" a, has "txOutDatum" b, opposite a b]
    allFields fields = not (null fields) && any (any (complete fields) . fieldPairs) fields
    complete fields pair = all (\f -> pair `elem` fieldPairs f || swap pair `elem` fieldPairs f) fields
    swap (a,b) = (b,a)

validityRange :: Expr -> Bool
validityRange e = has "txInfoValidRange" e && not (any bounded (checks e))
  where
    bounded (op,a,b) = (op `elem` ["<=","<"] && duration a && not (has "txInfoValidRange" b))
        || (op `elem` [">=",">"] && duration b && not (has "txInfoValidRange" a))
    duration x = any delta (argsOf "-" x <> argsOf "subtract" x)
    delta [hi,lo] = has "ivTo" hi && has "ivFrom" lo && sameRange hi lo
    delta _ = False
    sameRange hi lo = any (`elem` argsOf "ivFrom" lo) (argsOf "ivTo" hi)

datumComparison :: Expr -> Bool
datumComparison e = any decodedComparison (comparisons e)
  where
    decodedComparison ("==",a,b) = decoded a || decoded b
    decodedComparison _ = False
    decoded x = has "fromBuiltinData" x || has "unsafeFromBuiltinData" x

incompleteToken :: Expr -> Bool
incompleteToken e = any incomplete (expressions e)
  where
    incomplete (Call f [predicate,values]) = any (`isName` f) ["all","any","filter"]
        && ignoresComponent predicate values
    incomplete (Call f [predicate,_,values]) = any (`isName` f) ["foldr","foldl"]
        && ignoresComponent predicate values
    incomplete _ = False
    ignoresComponent predicate values = has "flattenValue" values &&
        any (\case Form "parameters" ps -> any tupleWildcard ps; _ -> False) (expressions predicate)
    tupleWildcard (Form "pattern" ps) = length ps == 3 && Form "wildcard" [] `elem` ps
    tupleWildcard _ = False

strictValue :: Expr -> Bool
strictValue e = any (\(op,a,b) -> op == "==" &&
    ((has "lovelaceValueOf" a && has "txOutValue" a) ||
     (has "lovelaceValueOf" b && has "txOutValue" b))) (comparisons e)

unvalidatedIndex :: Expr -> Bool
unvalidatedIndex e = any unvalidated selected
  where
    selected = [x | x@(Call f [inputs,index]) <- expressions e, isName "!!" f,
        has "txInfoInputs" inputs || has "txInfoReferenceInputs" inputs,
        any (\case Ref v -> not (isExternalName v); _ -> False) (expressions index)]
    unvalidated x = not (any (identifies x) (checks e))
    identifies x (op,a,b) = op `elem` ["==",">="] && b == Lit "1" &&
        (has "valueOf" a || has "assetClassValueOf" a) && mentions x a && has "txOutValue" a

fixedMap :: Expr -> Bool
fixedMap e = any fixed (expressions e)
  where
    fixed (Call f [Lit key,Call (Ref getter) [_]]) = isName "member" f &&
        BS.isPrefixOf "\"" key && isExternalName getter
    fixed _ = False

-- A suggestion for trivial wrappers only; no claim about optimizer behavior.
helperShape :: Expr -> Bool
helperShape (Call (Ref _) xs) = not (null xs) && all atom xs
  where atom Ref{} = True
        atom Lit{} = True
        atom _ = False
helperShape (Form "case" (_:branches)) = not (null branches) && all simple branches
  where simple (Form "branch" (_:xs)) = all atom xs
        simple _ = False
        atom Ref{} = True
        atom Lit{} = True
        atom (Form tag [_]) = "field:" `isPrefixOf` tag
        atom _ = False
helperShape _ = False

zipUnguarded :: Expr -> Bool
zipUnguarded root = visit (required root) root
  where
    visit facts e
        | Form "if" [c,t,f] <- e = visit facts c || visit (required c <> facts) t || visit facts f
        | Call f xs <- e, Just lists <- zipped f xs =
            not (all (connected facts (viaNonEmpty head lists)) lists)
                || any (visit facts) xs
        | otherwise = any (visit facts) (children e)
    zipped f xs
        | isName "zip" f, length xs == 2 = Just xs
        | isName "zip3" f, length xs == 3 = Just xs
        | isName "zipWith" f, length xs == 3 = Just (drop 1 xs)
        | otherwise = Nothing
    connected facts start target = maybe False (walk []) start
      where
        edges = [(a,b) | fact <- facts, Just ("==",l,r) <- [comparison fact],
            [a] <- argsOf "length" l, [b] <- argsOf "length" r]
        walk seen x = x == target || (x `notElem` seen &&
            any (walk (x:seen)) ([b | (a,b) <- edges,a == x] <> [a | (a,b) <- edges,b == x]))

-- Preserve precise arithmetic spans while resolving aliases by GHC Name.
-- Normalising a multiplication node cannot taint same-spelled binders in
-- unrelated functions, unlike a file-wide textual identifier search.
precisionFindings :: HieFile -> [(Text, RealSrcSpan)]
precisionFindings hie =
    [("PLU-STAN-16",nodeSpan n) | n <- forest,
      ann "OpApp" "HsExpr" n || ann "HsApp" "HsExpr" n,
      let raw = normalise hie Map.empty Map.empty Set.empty 60 n,
      syntacticMultiply raw,
      let expanded = normalise hie defs Map.empty Set.empty 60 n,
      divisionOperand expanded]
  where
    forest = concatMap nodes (Map.elems (getAsts (hie_asts hie)))
    defs = Map.fromList [(v,n) | n <- forest, ann "FunBind" "HsBindLR" n,
        ann "Match" "Match" n, Just v <- [bindingName n]]
    syntacticMultiply (Call f [_,_]) = any (`isName` f) ["*","mul","multiplyInteger"]
    syntacticMultiply _ = False
    divisionOperand (Call _ xs) = any (\x -> any (`has` x) ["div","quot","/","divideInteger","quotientInteger"]) xs
    divisionOperand _ = False

-- Template Haskell declaration splices are expanded out of HIE. For that
-- one syntactic check, mask comments/string literals without moving spans.
-- This is not used as semantic evidence by the expression-based rules.
stripNonCode :: ByteString -> ByteString
stripNonCode = BS.pack . code . BS.unpack
  where
    blank c = if c == '\n' then '\n' else ' '
    code ('-':'-':xs) = ' ':' ':lineComment xs
    code ('{':'-':xs) = ' ':' ':blockComment (1 :: Int) xs
    code ('"':xs) = ' ':stringLiteral xs
    code (x:xs) = x:code xs
    code [] = []
    lineComment ('\n':xs) = '\n':code xs
    lineComment (x:xs) = blank x:lineComment xs
    lineComment [] = []
    blockComment depth ('{':'-':xs) = ' ':' ':blockComment (depth+1) xs
    blockComment 1 ('-':'}':xs) = ' ':' ':code xs
    blockComment depth ('-':'}':xs) = ' ':' ':blockComment (depth-1) xs
    blockComment depth (x:xs) = blank x:blockComment depth xs
    blockComment _ [] = []
    stringLiteral ('\\':x:xs) = ' ':blank x:stringLiteral xs
    stringLiteral ('"':xs) = ' ':code xs
    stringLiteral (x:xs) = blank x:stringLiteral xs
    stringLiteral [] = []
