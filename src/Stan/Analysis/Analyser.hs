{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Analysing functions by 'InspectionAnalysis' for the corresponding
'Inspection'.
-}

module Stan.Analysis.Analyser
    ( analyseAst
    ) where

import Extensions (ExtensionsResult)
import GHC.LanguageExtensions.Type (Extension (Strict, StrictData))
import Slist (Slist)

import Stan.Analysis.Visitor (Visitor (..), VisitorState (..), addFixity, addObservation,
                              addObservations, addOpDecl, getFinalObservations)
import Stan.Core.Id (Id)
import Stan.Core.List (nonRepeatingPairs)
import Stan.Core.ModuleName (ModuleName (..))
import Stan.FileInfo (isExtensionDisabled)
import Stan.Ghc.Compat (Name, RealSrcSpan, isSymOcc, nameOccName, occNameString, srcSpanEndCol,
                        srcSpanEndLine, srcSpanStartCol, srcSpanStartLine, isExternalName,
                        IfaceTyCon (..))
import Stan.Hie (eqAst, slice)
import Stan.Hie.Compat (ContextInfo (..), HieAST (..), HieASTs (..), HieFile (..),
                        HieArgs (..), HieType (..), HieTypeFlat, Identifier, IdentifierDetails (..),
                        NodeAnnotation, NodeInfo (..), TypeIndex, mkNodeAnnotation, nodeInfo,
                        toNodeAnnotation)
import Stan.Hie.MatchAst (hieMatchPatternAst)
import Stan.Hie.MatchType (hieMatchPatternType)
import Stan.Inspection (Inspection (..), InspectionAnalysis (..))
import Stan.NameMeta (NameMeta (..), baseNameFrom, ghcPrimNameFrom, hieMatchNameMeta,
                      plutusTxNameFrom)
import Stan.Observation (Observations, mkObservation)
import Stan.Pattern.Ast (Literal (..), PatternAst (..), anyNamesToPatternAst, app, case',
                         constructor, constructorNameIdentifier, dataDecl, fixity, fun,
                         guardBranch, lambdaCase, lazyField, literalPat, opApp, patternMatchArrow,
                         patternMatchBranch, patternMatch_, rhs, tuple, typeSig)
import Stan.Pattern.Edsl (PatternBool (..))
import Stan.Pattern.Type (PatternType, (|::))

import Data.Char (isAlphaNum, isLower, isSpace, toLower)
import Numeric (readHex, readOct)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Array as Arr
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Slist as S

{- | Analyses the whole AST starting from the very top.
-}
analyseAst
    :: HieFile
    -> ExtensionsResult
    -> [Inspection]
    -> Observations
analyseAst hie exts = getFinalObservations hie . createVisitor hie exts

{- | Create a sinble 'Visitor' value from a list of 'Inspection's and
additional read-only context. This 'Visitor' can be used to traverse
HIE AST in a single pass.
-}
createVisitor
    :: HieFile
    -> ExtensionsResult
    -> [Inspection]
    -> Visitor
createVisitor hie exts inspections = Visitor $ \node ->
    forM_ inspections $ \Inspection{..} -> case inspectionAnalysis of
        FindAst patAst -> matchAst inspectionId patAst hie node
        Infix -> analyseInfix hie node
        LazyField -> when
            (isExtensionDisabled StrictData exts && isExtensionDisabled Strict exts)
            (analyseLazyFields inspectionId hie node)
        BigTuples -> analyseBigTuples inspectionId hie node
        PatternMatchOn_ -> analysePatternMatch_ inspectionId hie node
        UseCompare -> analyseCompare inspectionId hie node
        NonStrictLetMultiUse -> analyseNonStrictLetMultiUse inspectionId hie node
        ValueOfInComparison -> analyseValueOfInComparison inspectionId hie node
        UnsafeFromBuiltinDataInHashComparison -> analyseUnsafeFromBuiltinDataInHashComparison inspectionId hie node
        CurrencySymbolValueOfOnMintedValue -> analyseCurrencySymbolValueOfOnMintedValue inspectionId hie node
        ValidityIntervalMisuse -> analyseValidityIntervalMisuse inspectionId hie node
        PrecisionLossDivisionBeforeMultiply -> analysePrecisionLossDivisionBeforeMultiply inspectionId hie node
        RedeemerSuppliedIndicesUniqueness -> analyseRedeemerSuppliedIndicesUniqueness inspectionId hie node
        LazyAndInOnChainCode -> analyseLazyAndInOnChainCode inspectionId hie node
        MissingTxOutReferenceScriptCheck -> analyseMissingTxOutReferenceScriptCheck inspectionId hie node
        MissingTxOutStakingCredentialCheck -> analyseMissingTxOutStakingCredentialCheck inspectionId hie node
        MissingTxOutValueCheck -> analyseMissingTxOutValueCheck inspectionId hie node
        MissingTxOutDatumCheck -> analyseMissingTxOutDatumCheck inspectionId hie node
        MissingBurningLogic -> analyseMissingBurningLogic inspectionId hie node

{- | Check for big tuples (size >= 4) in the following places:

* Type signatures: foo :: (Int, Int, Int, Int)
* Literals: (True, 0, [], Nothing)
-}
analyseBigTuples
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseBigTuples insId = matchAstWith isBigTuple insId tuple
  where
    isBigTuple :: HieAST TypeIndex -> Bool
    isBigTuple Node{..} = case nodeChildren of
        _:_:_:_:_  -> True
        _lessThan4 -> False

{- | Find usages of multiple comparison operators and suggest using
'compare'. Currently, handles the following cases:

* Guards

The algorithm is to find all guards, filter them by usage of
comparison operators and find matches.
-}
analyseCompare
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseCompare insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchComparisonGuards curNode
  where
    matchComparisonGuards :: HieAST TypeIndex -> Slist RealSrcSpan
    matchComparisonGuards node = memptyIfFalse
        (hieMatchPatternAst hie node fun)
        $ let guards = mapMaybe extractComparisonGuard (nodeChildren node)
          in memptyIfFalse (hasManyCompares guards) (S.one $ nodeSpan node)

    {- Extract left argument, name of a comparison operator and right
    argument from a guard.
    -}
    extractComparisonGuard
        :: HieAST TypeIndex
        -> Maybe (HieAST TypeIndex, HieAST TypeIndex)
    extractComparisonGuard node = do
        -- guard starts with GRHS annotation
        guard $ hieMatchPatternAst hie node rhs
        -- guard predicate is a first son
        stmt:_ <- Just $ nodeChildren node
        -- check if it's a guard
        guard $ hieMatchPatternAst hie stmt guardBranch
        -- check if it's an operator
        guard $ hieMatchPatternAst hie stmt $ opApp (?) opsPat (?)
        -- extract comparison
        x:_opAst:y:_ <- Just $ nodeChildren stmt
        pure (x, y)

    -- pattern for any comparison operator
    opsPat :: PatternAst
    opsPat = anyNamesToPatternAst $ le :| [leq, eq, ge, geq]

    le, leq, eq, ge, geq :: NameMeta
    le  = opName "<"
    leq = opName "<="
    eq  = opName "=="
    ge  = opName ">"
    geq = opName ">="

    opName :: Text -> NameMeta
    opName = (`ghcPrimNameFrom` "GHC.Classes")

    -- return True if any two pairs perform comparison of similar things
    hasManyCompares :: [(HieAST TypeIndex, HieAST TypeIndex)] -> Bool
    hasManyCompares = any (uncurry matchingComparions) . nonRepeatingPairs

    matchingComparions
        :: (HieAST TypeIndex, HieAST TypeIndex)
        -> (HieAST TypeIndex, HieAST TypeIndex)
        -> Bool
    matchingComparions (a, b) (x, y) =
        (eqAst hie a x && eqAst hie b y) || (eqAst hie a y && eqAst hie b x)

analyseNonStrictLetMultiUse
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseNonStrictLetMultiUse insId hie curNode =
    addObservations $ mkObservation insId hie <$> (matchLetMultiUse curNode <> matchLocalFunArgMultiUse curNode)
  where
    matchLetMultiUse :: HieAST TypeIndex -> Slist RealSrcSpan
    matchLetMultiUse node = memptyIfFalse (isLetNode node) $
        case extractLetParts node of
            Nothing -> mempty
            Just (binds, body) ->
                let bindings = collectBindings binds
                    letUses name = countNameUses name body + countNameUses name binds
                    badBindings = filter (\(n, _span, isStrict) ->
                        not isStrict && letUses n > 1) bindings
                in S.slist $ map (\(_n, bindSpan, _isStrict) -> bindSpan) badBindings

    extractLetParts :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex)
    extractLetParts Node{nodeChildren = binds:body:_} = Just (binds, body)
    extractLetParts _ = Nothing

    isLetNode :: HieAST TypeIndex -> Bool
    isLetNode = nodeHasAnnotation letAnnotation

    letAnnotation :: NodeAnnotation
    letAnnotation = mkNodeAnnotation "HsLet" "HsExpr"

    nodeHasAnnotation :: NodeAnnotation -> HieAST TypeIndex -> Bool
    nodeHasAnnotation ann node =
        let NodeInfo{nodeAnnotations = nodeAnnotations'} = nodeInfo node
        in ann `Set.member` Set.map toNodeAnnotation nodeAnnotations'

    collectBindings :: HieAST TypeIndex -> [(Name, RealSrcSpan, Bool)]
    collectBindings node =
        map (\(name, (bindSpan, isStrict)) -> (name, bindSpan, isStrict))
            (Map.toList $ go False Map.empty node)
      where
        go :: Bool -> Map Name (RealSrcSpan, Bool) -> HieAST TypeIndex -> Map Name (RealSrcSpan, Bool)
        go underBang acc n@Node{nodeSpan = bindSpan, nodeChildren = children} =
            let info = nodeInfo n
                underBang' = underBang || nodeHasAnnotation bangPatAnnotation n
                acc' = foldl' (insertBinding underBang' bindSpan) acc
                    (Map.assocs $ nodeIdentifiers info)
            in foldl' (go underBang') acc' children

        insertBinding
            :: Bool
            -> RealSrcSpan
            -> Map Name (RealSrcSpan, Bool)
            -> (Identifier, IdentifierDetails TypeIndex)
            -> Map Name (RealSrcSpan, Bool)
        insertBinding isStrict fallbackSpan acc (ident, details) = case ident of
            Right name | Just bindSpan <- bindingSpan details ->
                Map.insertWith
                    (\(s1, b1) (_s2, b2) -> (s1, b1 || b2))
                    name
                    (bindSpan, isStrict || strictByPrefixSpan bindSpan fallbackSpan)
                    acc
            Right name | isBinding details ->
                Map.insertWith
                    (\(s1, b1) (_s2, b2) -> (s1, b1 || b2))
                    name
                    (fallbackSpan, isStrict)
                    acc
            _ -> acc

        bindingSpan :: IdentifierDetails TypeIndex -> Maybe RealSrcSpan
        bindingSpan IdentifierDetails{identInfo = identInfo'} =
            listToMaybe $ mapMaybe bindingSpanFromInfo (toList identInfo')

        bindingSpanFromInfo :: ContextInfo -> Maybe RealSrcSpan
        bindingSpanFromInfo (ValBind _ _ (Just bindSpan)) = Just bindSpan
        bindingSpanFromInfo (PatternBind _ _ (Just bindSpan)) = Just bindSpan
        bindingSpanFromInfo _ = Nothing


        isBinding :: IdentifierDetails TypeIndex -> Bool
        isBinding IdentifierDetails{identInfo = identInfo'} =
            any isBindingCtx identInfo'

        isBindingCtx :: ContextInfo -> Bool
        isBindingCtx (ValBind _ _ _) = True
        isBindingCtx (PatternBind _ _ _) = True
        isBindingCtx _ = False

    bangPatAnnotation :: NodeAnnotation
    bangPatAnnotation = mkNodeAnnotation "BangPat" "Pat"

    strictByPrefixSpan :: RealSrcSpan -> RealSrcSpan -> Bool
    strictByPrefixSpan bindSpan nameSpan =
        srcSpanStartLine bindSpan == srcSpanStartLine nameSpan
            && srcSpanStartCol bindSpan + 1 == srcSpanStartCol nameSpan
            && hasBangPrefix bindSpan

    hasBangPrefix :: RealSrcSpan -> Bool
    hasBangPrefix span' = case lineAt (srcSpanStartLine span') of
        Nothing -> False
        Just line ->
            let col = srcSpanStartCol span'
            in col > 0 && (line BS8.!? (col - 1) == Just '!')

    lineAt :: Int -> Maybe BS8.ByteString
    lineAt n = (BS8.lines $ hie_hs_src hie) !!? (n - 1)

    matchLocalFunArgMultiUse :: HieAST TypeIndex -> Slist RealSrcSpan
    matchLocalFunArgMultiUse funNode = memptyIfFalse (hieMatchPatternAst hie funNode fun) $
        let (patNodes, rhsNodes) = splitAtFirstRhs (nodeChildren funNode)
        in memptyIfFalse (isLocalFun patNodes) $
            let allBindings = collectMatchBindings patNodes
                headNames = collectValBindNames patNodes
                args = filter (\(n, _span, _isStrict) -> Set.notMember n headNames) allBindings
                argUses name = sum (map (countNameUses name) rhsNodes)
                badArgs = filter (\(n, _span, isStrict) -> not isStrict && argUses n > 1) args
            in S.slist $ map (\(_n, bindSpan, _isStrict) -> bindSpan) badArgs

    splitAtFirstRhs :: [HieAST TypeIndex] -> ([HieAST TypeIndex], [HieAST TypeIndex])
    splitAtFirstRhs = go []
      where
        go acc = \case
            [] -> (reverse acc, [])
            x:xs
                | hieMatchPatternAst hie x rhs -> (reverse acc, x:xs)
                | otherwise -> go (x:acc) xs

    isLocalFun :: [HieAST TypeIndex] -> Bool
    isLocalFun patNodes = any (not . isExternalName) (collectValBindNames patNodes)

    collectValBindNames :: [HieAST TypeIndex] -> Set Name
    collectValBindNames = foldMap go
      where
        go :: HieAST TypeIndex -> Set Name
        go n@Node{nodeChildren = children} =
            let info = nodeInfo n
                bindsHere = Set.fromList
                    [ name
                    | (Right name, IdentifierDetails{identInfo = identInfo'}) <- Map.assocs (nodeIdentifiers info)
                    , any isValBindCtx identInfo'
                    ]
            in bindsHere <> foldMap go children

        isValBindCtx :: ContextInfo -> Bool
        isValBindCtx = \case
            ValBind _ _ _ -> True
            MatchBind -> True
            _ -> False

    collectMatchBindings :: [HieAST TypeIndex] -> [(Name, RealSrcSpan, Bool)]
    collectMatchBindings nodes =
        map (\(name, (bindSpan, isStrict)) -> (name, bindSpan, isStrict))
            (Map.toList $ foldl' (go False) Map.empty nodes)
      where
        go :: Bool -> Map Name (RealSrcSpan, Bool) -> HieAST TypeIndex -> Map Name (RealSrcSpan, Bool)
        go underBang acc n@Node{nodeSpan = bindSpan, nodeChildren = children} =
            let info = nodeInfo n
                underBang' = underBang || nodeHasAnnotation bangPatAnnotation n
                acc' = foldl' (insertMatchBinding underBang' bindSpan) acc
                    (Map.assocs $ nodeIdentifiers info)
            in foldl' (go underBang') acc' children

        insertMatchBinding
            :: Bool
            -> RealSrcSpan
            -> Map Name (RealSrcSpan, Bool)
            -> (Identifier, IdentifierDetails TypeIndex)
            -> Map Name (RealSrcSpan, Bool)
        insertMatchBinding isStrict fallbackSpan acc (ident, IdentifierDetails{identInfo = identInfo'}) = case ident of
            Right name | any isArgBindingCtx identInfo' ->
                Map.insertWith
                    (\(s1, b1) (_s2, b2) -> (s1, b1 || b2))
                    name
                    (fallbackSpan, isStrict)
                    acc
            _ -> acc

        isArgBindingCtx :: ContextInfo -> Bool
        isArgBindingCtx = \case
            MatchBind -> True
            PatternBind _ _ _ -> True
            _ -> False

    countNameUses :: Name -> HieAST TypeIndex -> Int
    countNameUses name = go
      where
        go :: HieAST TypeIndex -> Int
        go n@Node{nodeChildren = children} =
            let info = nodeInfo n
                useHere = any (isNameUse name) (Map.assocs $ nodeIdentifiers info)
            in (if useHere then 1 else 0) + sum (map go children)

        isNameUse :: Name -> (Identifier, IdentifierDetails TypeIndex) -> Bool
        isNameUse target (ident, IdentifierDetails{identInfo = identInfo'}) = case ident of
            Right identName -> identName == target && Set.member Use identInfo'
            _ -> False

analyseValueOfInComparison
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseValueOfInComparison insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchValueOfInComparison Set.empty curNode
  where
    matchValueOfInComparison :: Set Name -> HieAST TypeIndex -> Slist RealSrcSpan
    matchValueOfInComparison valueOfBindings node =
        let valueOfBindings' = case extractLetParts node of
                Just (binds, _body) ->
                    valueOfBindings <> collectValueOfBindings (hie_hs_src hie) binds
                Nothing -> valueOfBindings
            here = case comparisonOperands node of
                Just (lhs, rhsNode)
                    | operandHasValueOf valueOfBindings lhs
                      || operandHasValueOf valueOfBindings rhsNode ->
                        S.one $ nodeSpan node
                _ -> mempty
        in here <> foldMap (matchValueOfInComparison valueOfBindings') (nodeChildren node)

    extractLetParts :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex)
    extractLetParts Node{nodeChildren = binds:body:_} = Just (binds, body)
    extractLetParts _ = Nothing

    comparisonOperands :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex)
    comparisonOperands node =
        opAppOperands node <|> appOperands node

    opAppOperands :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex)
    opAppOperands node = do
        guard $ nodeHasAnnotation opAppAnnotation node
        lhsNode:op:rhsNode:_ <- Just $ nodeChildren node
        guard $ hieMatchPatternAst hie op opsPat
        pure (lhsNode, rhsNode)

    appOperands :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex)
    appOperands node = do
        guard $ nodeHasAnnotation hsAppAnnotation node
        let (headNode, args) = appSpine node
        case args of
            arg1:arg2:_ -> do
                guard $ nodeHasEqName headNode
                Just (arg1, arg2)
            [arg1] -> do
                guard $ isSectionNode headNode || isEqSectionBySource (hie_hs_src hie) headNode
                guard $ nodeHasEqName headNode || isEqSectionBySource (hie_hs_src hie) headNode
                let fixed = fromMaybe headNode (sectionOperand headNode)
                Just (fixed, arg1)
            _ -> Nothing

    opAppAnnotation :: NodeAnnotation
    opAppAnnotation = mkNodeAnnotation "OpApp" "HsExpr"

    hsAppAnnotation :: NodeAnnotation
    hsAppAnnotation = mkNodeAnnotation "HsApp" "HsExpr"

    nodeHasAnnotation :: NodeAnnotation -> HieAST TypeIndex -> Bool
    nodeHasAnnotation ann node =
        let NodeInfo{nodeAnnotations = nodeAnnotations'} = nodeInfo node
        in ann `Set.member` Set.map toNodeAnnotation nodeAnnotations'

    appSpine :: HieAST TypeIndex -> (HieAST TypeIndex, [HieAST TypeIndex])
    appSpine node = case node of
        n@Node{nodeChildren = appFun:arg:_}
            | nodeHasAnnotation hsAppAnnotation n ->
                let (f, args) = appSpine appFun
                in (f, args <> [arg])
        _ -> (node, [])

    opsPat :: PatternAst
    opsPat = anyNamesToPatternAst (eq :| [])

    eq :: NameMeta
    eq = opName "=="

    opName :: Text -> NameMeta
    opName = (`ghcPrimNameFrom` "GHC.Classes")

    operandHasValueOf :: Set Name -> HieAST TypeIndex -> Bool
    operandHasValueOf valueOfBindings ast =
        containsValueOf ast || usesValueOfBinding valueOfBindings ast

    containsValueOf :: HieAST TypeIndex -> Bool
    containsValueOf node =
        nodeHasValueOf node || any containsValueOf (nodeChildren node)

    nodeHasValueOf :: HieAST TypeIndex -> Bool
    nodeHasValueOf node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
        in any (\pair -> any (`hieMatchNameMeta` pair) valueOfNameMetas) idents

    nodeHasEqName :: HieAST TypeIndex -> Bool
    nodeHasEqName node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
            eqNameMeta = opName "=="
            matchesEq = any (hieMatchNameMeta eqNameMeta) idents
        in matchesEq || any nodeHasEqName (nodeChildren node)

    isSectionNode :: HieAST TypeIndex -> Bool
    isSectionNode node =
        nodeHasAnnotation sectionLAnnotation node || nodeHasAnnotation sectionRAnnotation node

    sectionLAnnotation :: NodeAnnotation
    sectionLAnnotation = mkNodeAnnotation "SectionL" "HsExpr"

    sectionRAnnotation :: NodeAnnotation
    sectionRAnnotation = mkNodeAnnotation "SectionR" "HsExpr"

    isEqSectionBySource :: ByteString -> HieAST TypeIndex -> Bool
    isEqSectionBySource srcBytes node = fromMaybe False $ do
        src <- slice (nodeSpan node) srcBytes
        let cleaned = BS8.filter (\c -> not (isSpace c) && c /= '(' && c /= ')') src
            hasEqPrefix = "==" `BS8.isPrefixOf` cleaned && BS8.length cleaned > 2
            hasEqSuffix = "==" `BS8.isSuffixOf` cleaned && BS8.length cleaned > 2
        pure (hasEqPrefix || hasEqSuffix)

    sectionOperand :: HieAST TypeIndex -> Maybe (HieAST TypeIndex)
    sectionOperand node =
        let nonEqChildren = filter (not . nodeHasEqName) (nodeChildren node)
        in listToMaybe nonEqChildren

    valueOfNameMetas :: [NameMeta]
    valueOfNameMetas =
        [ NameMeta
            { nameMetaName = "valueOf"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V1.Value"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , "valueOf" `plutusTxNameFrom` "PlutusTx.Value"
        ]

    usesValueOfBinding :: Set Name -> HieAST TypeIndex -> Bool
    usesValueOfBinding valueOfBindings = go
      where
        go n@Node{nodeChildren = children} =
            let info = nodeInfo n
                useHere = any (isNameUse valueOfBindings)
                    (Map.assocs $ nodeIdentifiers info)
            in useHere || any go children

        isNameUse
            :: Set Name
            -> (Identifier, IdentifierDetails TypeIndex)
            -> Bool
        isNameUse bindings (ident, IdentifierDetails{identInfo = identInfo'}) =
            case ident of
                Right identName ->
                    Set.member Use identInfo' && Set.member identName bindings
                _ -> False

    collectValueOfBindings :: ByteString -> HieAST TypeIndex -> Set Name
    collectValueOfBindings hsSrc = go Set.empty
      where
        go acc n@Node{nodeSpan = bindSpan, nodeChildren = children} =
            let info = nodeInfo n
                acc' = foldl' (insertBinding bindSpan) acc
                    (Map.assocs $ nodeIdentifiers info)
            in foldl' go acc' children

        insertBinding
            :: RealSrcSpan
            -> Set Name
            -> (Identifier, IdentifierDetails TypeIndex)
            -> Set Name
        insertBinding fallbackSpan acc (ident, details) = case ident of
            Right name | Just bindSpan <- bindingSpan details
                , bindingHasValueOf hsSrc bindSpan ->
                    Set.insert name acc
            Right name | isBinding details
                , bindingHasValueOf hsSrc fallbackSpan ->
                    Set.insert name acc
            _ -> acc

        bindingSpan :: IdentifierDetails TypeIndex -> Maybe RealSrcSpan
        bindingSpan IdentifierDetails{identInfo = identInfo'} =
            listToMaybe $ mapMaybe bindingSpanFromInfo (toList identInfo')

        bindingSpanFromInfo :: ContextInfo -> Maybe RealSrcSpan
        bindingSpanFromInfo (ValBind _ _ (Just bindSpan)) = Just bindSpan
        bindingSpanFromInfo (PatternBind _ _ (Just bindSpan)) = Just bindSpan
        bindingSpanFromInfo _ = Nothing

        bindingHasValueOf :: ByteString -> RealSrcSpan -> Bool
        bindingHasValueOf srcBytes bindSpan = fromMaybe False $ do
            src <- slice bindSpan srcBytes
            pure $ "valueOf" `BS8.isInfixOf` src

        isBinding :: IdentifierDetails TypeIndex -> Bool
        isBinding IdentifierDetails{identInfo = identInfo'} =
            any isBindingCtx identInfo'

        isBindingCtx :: ContextInfo -> Bool
        isBindingCtx (ValBind _ _ _) = True
        isBindingCtx (PatternBind _ _ _) = True
        isBindingCtx _ = False


analyseCurrencySymbolValueOfOnMintedValue
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseCurrencySymbolValueOfOnMintedValue insId hie curNode = do
    addObservations $ mkObservation insId hie <$> matchCurrencySymbolValueOf curNode
  where
    allHieAsts :: [HieAST TypeIndex]
    allHieAsts = Map.elems $ getAsts $ hie_asts hie

    mintedBindings :: Set Name
    mintedBindings =
        foldMap (collectMintedBindings (hie_hs_src hie)) allHieAsts

    mintedBindingOccs :: Set ByteString
    mintedBindingOccs =
        Set.map (BS8.pack . occNameString . nameOccName) mintedBindings

    functionParamMap :: Map ByteString [ByteString]
    functionParamMap =
        collectFunctionParamMap (hie_hs_src hie) (collectBindingOccs allHieAsts)

    functionSpans :: [(ByteString, RealSrcSpan)]
    functionSpans =
        collectFunctionSpans allHieAsts functionParamMap

    taintedFunctionParams :: Map ByteString (Set ByteString)
    taintedFunctionParams =
        collectTaintedFunctionParams allHieAsts functionParamMap mintedBindings mintedBindingOccs

    matchCurrencySymbolValueOf :: HieAST TypeIndex -> Slist RealSrcSpan
    matchCurrencySymbolValueOf node =
        let here = case currencySymbolValueOfCall node of
                Just arg1
                    | argIsMinted arg1 -> S.one $ nodeSpan node
                    | argIsTaintedParam node arg1 -> S.one $ nodeSpan node
                _ -> mempty
        in here <> foldMap matchCurrencySymbolValueOf (nodeChildren node)

    currencySymbolValueOfCall :: HieAST TypeIndex -> Maybe (HieAST TypeIndex)
    currencySymbolValueOfCall node = do
        guard $ nodeHasAnnotation hsAppAnnotation node
        let (headNode, args) = appSpine node
        guard $ nodeHasCurrencySymbolValueOf headNode
        listToMaybe args

    nodeHasCurrencySymbolValueOf :: HieAST TypeIndex -> Bool
    nodeHasCurrencySymbolValueOf node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
            matchesName =
                any (\pair -> any (`hieMatchNameMeta` pair) currencySymbolValueOfNameMetas) idents
            matchesOcc =
                any
                    (\(ident, _) -> case ident of
                        Right identName ->
                            occNameString (nameOccName identName) == "currencySymbolValueOf"
                        _ -> False
                    )
                    idents
            matchesSource = fromMaybe False $ do
                src <- slice (nodeSpan node) (hie_hs_src hie)
                pure $ "currencySymbolValueOf" `BS8.isInfixOf` src
        in matchesName || matchesOcc || matchesSource

    hsAppAnnotation :: NodeAnnotation
    hsAppAnnotation = mkNodeAnnotation "HsApp" "HsExpr"

    appSpine :: HieAST TypeIndex -> (HieAST TypeIndex, [HieAST TypeIndex])
    appSpine node = case node of
        n@Node{nodeChildren = appFun:arg:_}
            | nodeHasAnnotation hsAppAnnotation n ->
                let (f, args) = appSpine appFun
                in (f, args <> [arg])
        _ -> (node, [])

    nodeHasAnnotation :: NodeAnnotation -> HieAST TypeIndex -> Bool
    nodeHasAnnotation ann node =
        let NodeInfo{nodeAnnotations = nodeAnnotations'} = nodeInfo node
        in ann `Set.member` Set.map toNodeAnnotation nodeAnnotations'

    argIsMinted :: HieAST TypeIndex -> Bool
    argIsMinted arg =
        containsTxInfoMint arg
            || usesMintedBinding mintedBindings arg

    argIsTaintedParam :: HieAST TypeIndex -> HieAST TypeIndex -> Bool
    argIsTaintedParam node arg =
        case (enclosingFunctionName (nodeSpan node), argVariableOcc arg) of
            (Just fnName, Just argName) ->
                case Map.lookup fnName taintedFunctionParams of
                    Just tainted -> Set.member argName tainted
                    Nothing -> False
            _ -> False

    enclosingFunctionName :: RealSrcSpan -> Maybe ByteString
    enclosingFunctionName spanToCheck =
        let candidates =
                [ (fn, span')
                | (fn, span') <- functionSpans
                , spanContains span' spanToCheck
                ]
        in fmap fst (selectSmallestSpan candidates)

    selectSmallestSpan :: [(ByteString, RealSrcSpan)] -> Maybe (ByteString, RealSrcSpan)
    selectSmallestSpan = foldl' pickSmaller Nothing
      where
        pickSmaller Nothing candidate = Just candidate
        pickSmaller (Just best@(_, bestSpan)) candidate@(_, candSpan)
            | spanIsSmaller candSpan bestSpan = Just candidate
            | otherwise = Just best

    spanIsSmaller :: RealSrcSpan -> RealSrcSpan -> Bool
    spanIsSmaller a b =
        let sizeA = spanSize a
            sizeB = spanSize b
        in sizeA < sizeB

    spanSize :: RealSrcSpan -> (Int, Int)
    spanSize span' =
        if srcSpanEndLine span' == srcSpanStartLine span'
            then (0, srcSpanEndCol span' - srcSpanStartCol span')
            else (srcSpanEndLine span' - srcSpanStartLine span', srcSpanEndCol span')

    spanContains :: RealSrcSpan -> RealSrcSpan -> Bool
    spanContains outer inner =
        let startsAfter =
                srcSpanStartLine inner > srcSpanStartLine outer
                || (srcSpanStartLine inner == srcSpanStartLine outer
                    && srcSpanStartCol inner >= srcSpanStartCol outer)
            endsBefore =
                srcSpanEndLine inner < srcSpanEndLine outer
                || (srcSpanEndLine inner == srcSpanEndLine outer
                    && srcSpanEndCol inner <= srcSpanEndCol outer)
        in startsAfter && endsBefore

    argVariableOcc :: HieAST TypeIndex -> Maybe ByteString
    argVariableOcc node@Node{nodeChildren = []} =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
        in listToMaybe
            [ BS8.pack (occNameString (nameOccName identName))
            | (Right identName, _) <- idents
            ]
    argVariableOcc _ = Nothing

    containsTxInfoMint :: HieAST TypeIndex -> Bool
    containsTxInfoMint node =
        nodeHasTxInfoMint node || any containsTxInfoMint (nodeChildren node)

    nodeHasTxInfoMint :: HieAST TypeIndex -> Bool
    nodeHasTxInfoMint node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
            matchesName = any (\pair -> any (`hieMatchNameMeta` pair) txInfoMintNameMetas) idents
            matchesOcc =
                any
                    (\(ident, _) -> case ident of
                        Right identName -> occNameString (nameOccName identName) == "txInfoMint"
                        _ -> False
                    )
                    idents
        in matchesName || matchesOcc

    usesMintedBinding :: Set Name -> HieAST TypeIndex -> Bool
    usesMintedBinding minted = go
      where
        go n@Node{nodeChildren = children} =
            let info = nodeInfo n
                useHere = any (isNameUse minted) (Map.assocs $ nodeIdentifiers info)
            in useHere || any go children

        isNameUse :: Set Name -> (Identifier, IdentifierDetails TypeIndex) -> Bool
        isNameUse bindings (ident, IdentifierDetails{identInfo = identInfo'}) =
            case ident of
                Right identName ->
                    Set.member Use identInfo' && Set.member identName bindings
                _ -> False

    collectMintedBindings :: ByteString -> HieAST TypeIndex -> Set Name
    collectMintedBindings hsSrc rootNode =
        let bindings = collectAllBindingsWithSpans rootNode
            bindingDeps = Map.fromList
                [ (name, depsInSpan rhsSpan)
                | (name, rhsSpan) <- bindings
                ]
            directlyTainted =
                Set.fromList
                    [ name
                    | (name, rhsSpan) <- bindings
                    , spanHasTxInfoMint rhsSpan
                    ]
                    <> Set.fromList
                        [ name
                        | (name, _rhsSpan) <- bindings
                        , bindingRhsContainsMint name
                        ]
        in expandTransitively bindingDeps directlyTainted
      where
        collectAllBindingsWithSpans :: HieAST TypeIndex -> [(Name, RealSrcSpan)]
        collectAllBindingsWithSpans = go
          where
            go n@Node{nodeChildren = children} =
                let info = nodeInfo n
                    bindings = mapMaybe extractBinding
                        (Map.assocs $ nodeIdentifiers info)
                in bindings ++ concatMap go children

            extractBinding
                :: (Identifier, IdentifierDetails TypeIndex)
                -> Maybe (Name, RealSrcSpan)
            extractBinding (ident, details) = case ident of
                Right name | Just rhsSpan <- getBindingSpan details ->
                    Just (name, rhsSpan)
                _ -> Nothing

        expandTransitively :: Map Name (Set Name) -> Set Name -> Set Name
        expandTransitively depsMap = go
          where
            go tainted =
                let newTainted =
                        Set.fromList
                            [ name
                            | (name, deps) <- Map.assocs depsMap
                            , not (Set.member name tainted)
                            , not (Set.null (deps `Set.intersection` tainted))
                            ]
                in if Set.null newTainted
                   then tainted
                   else go (tainted `Set.union` newTainted)

        bindingRhsContainsMint :: Name -> Bool
        bindingRhsContainsMint name =
            let nameBS = BS8.pack $ occNameString $ nameOccName name
                srcLines = BS8.lines hsSrc
                hasMintBinding line =
                    ((nameBS <> " = ") `BS8.isInfixOf` line || (nameBS <> " =") `BS8.isInfixOf` line)
                    && ("txInfoMint" `BS8.isInfixOf` line)
            in any hasMintBinding srcLines

        depsInSpan :: RealSrcSpan -> Set Name
        depsInSpan rhsSpan =
            go Set.empty rootNode
          where
            go acc n@Node{nodeSpan = nodeSpan', nodeChildren = children} =
                if not (spanOverlaps rhsSpan nodeSpan')
                    then acc
                    else
                        let acc' =
                                if rhsSpan `spanContainsOrEq` nodeSpan'
                                    then acc <> usedNamesInNode n
                                    else acc
                        in foldl' go acc' children

        usedNamesInNode :: HieAST TypeIndex -> Set Name
        usedNamesInNode node =
            let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
            in Set.fromList
                [ identName
                | (Right identName, IdentifierDetails{identInfo = identInfo'}) <- idents
                , Set.member Use identInfo'
                ]

        spanHasTxInfoMint :: RealSrcSpan -> Bool
        spanHasTxInfoMint rhsSpan =
            go rootNode
          where
            go n@Node{nodeSpan = nodeSpan', nodeChildren = children} =
                if not (spanOverlaps rhsSpan nodeSpan')
                    then False
                    else
                        (rhsSpan `spanContainsOrEq` nodeSpan' && nodeHasTxInfoMint n)
                            || any go children

        spanContainsOrEq :: RealSrcSpan -> RealSrcSpan -> Bool
        spanContainsOrEq outer inner =
            let startsAfter =
                    srcSpanStartLine inner > srcSpanStartLine outer
                    || (srcSpanStartLine inner == srcSpanStartLine outer
                        && srcSpanStartCol inner >= srcSpanStartCol outer)
                startsEqual =
                    srcSpanStartLine inner == srcSpanStartLine outer
                    && srcSpanStartCol inner == srcSpanStartCol outer
                endsBefore =
                    srcSpanEndLine inner < srcSpanEndLine outer
                    || (srcSpanEndLine inner == srcSpanEndLine outer
                        && srcSpanEndCol inner <= srcSpanEndCol outer)
                endsEqual =
                    srcSpanEndLine inner == srcSpanEndLine outer
                    && srcSpanEndCol inner == srcSpanEndCol outer
            in (startsAfter || startsEqual) && (endsBefore || endsEqual)

        spanOverlaps :: RealSrcSpan -> RealSrcSpan -> Bool
        spanOverlaps a b =
            let aStartsBeforeBEnds =
                    srcSpanStartLine a < srcSpanEndLine b
                    || (srcSpanStartLine a == srcSpanEndLine b
                        && srcSpanStartCol a <= srcSpanEndCol b)
                bStartsBeforeAEnds =
                    srcSpanStartLine b < srcSpanEndLine a
                    || (srcSpanStartLine b == srcSpanEndLine a
                        && srcSpanStartCol b <= srcSpanEndCol a)
            in aStartsBeforeBEnds && bStartsBeforeAEnds

        getBindingSpan :: IdentifierDetails TypeIndex -> Maybe RealSrcSpan
        getBindingSpan IdentifierDetails{identInfo = identInfo'} =
            listToMaybe $ mapMaybe spanFromCtx (toList identInfo')
          where
            spanFromCtx (ValBind _ _ (Just s)) = Just s
            spanFromCtx (PatternBind _ _ (Just s)) = Just s
            spanFromCtx _ = Nothing

    collectBindingOccs :: [HieAST TypeIndex] -> Set ByteString
    collectBindingOccs =
        foldMap collectBindingOccsFromAst
      where
        collectBindingOccsFromAst :: HieAST TypeIndex -> Set ByteString
        collectBindingOccsFromAst = go Set.empty
          where
            go acc n@Node{nodeChildren = children} =
                let info = nodeInfo n
                    acc' = foldl' insertBinding acc (Map.assocs $ nodeIdentifiers info)
                in foldl' go acc' children

            insertBinding
                :: Set ByteString
                -> (Identifier, IdentifierDetails TypeIndex)
                -> Set ByteString
            insertBinding acc (ident, IdentifierDetails{identInfo = identInfo'}) =
                case ident of
                    Right name
                        | any isBindingCtx identInfo' ->
                            Set.insert (BS8.pack $ occNameString $ nameOccName name) acc
                    _ -> acc

            isBindingCtx :: ContextInfo -> Bool
            isBindingCtx (ValBind _ _ _) = True
            isBindingCtx (PatternBind _ _ _) = True
            isBindingCtx _ = False

    collectFunctionParamMap :: ByteString -> Set ByteString -> Map ByteString [ByteString]
    collectFunctionParamMap srcBytes bindingNames =
        let lines' = BS8.lines srcBytes
            entries = mapMaybe (parseLine bindingNames) lines'
        in Map.fromList entries
      where
        parseLine :: Set ByteString -> ByteString -> Maybe (ByteString, [ByteString])
        parseLine names line = do
            let stripped = BS8.dropWhile isSpace line
            guard (not (BS8.null stripped))
            let noComment = stripLineComment stripped
            guard $ "=" `BS8.isInfixOf` noComment
            let (lhs, _rhs) = BS8.break (== '=') noComment
                tokens = BS8.words lhs
            fname <- listToMaybe tokens
            guard $ Set.member fname names
            let params = mapMaybe tokenToIdent (drop 1 tokens)
            guard (not (null params))
            pure (fname, params)

        tokenToIdent :: ByteString -> Maybe ByteString
        tokenToIdent tok =
            let trimmed = BS8.dropWhile (not . isIdentChar) tok
                ident = BS8.takeWhile isIdentChar trimmed
            in if BS8.null ident then Nothing else Just ident

        isIdentChar :: Char -> Bool
        isIdentChar c = isAlphaNum c || c == '_' || c == '\''

        stripLineComment :: ByteString -> ByteString
        stripLineComment line =
            case BS8.breakSubstring "--" line of
                (before, after)
                    | BS8.null after -> line
                    | otherwise -> before

    collectFunctionSpans
        :: [HieAST TypeIndex]
        -> Map ByteString [ByteString]
        -> [(ByteString, RealSrcSpan)]
    collectFunctionSpans asts paramMap =
        let bindings = foldMap collectBindingsWithSpans asts
        in
        [ (occ, span')
        | (name, span') <- bindings
        , let occ = BS8.pack $ occNameString $ nameOccName name
        , Map.member occ paramMap
        ]
      where
        collectBindingsWithSpans :: HieAST TypeIndex -> [(Name, RealSrcSpan)]
        collectBindingsWithSpans = go
          where
            go n@Node{nodeSpan = nodeSpan', nodeChildren = children} =
                let info = nodeInfo n
                    bindings = mapMaybe (extractBinding nodeSpan')
                        (Map.assocs $ nodeIdentifiers info)
                in bindings ++ concatMap go children

            extractBinding
                :: RealSrcSpan
                -> (Identifier, IdentifierDetails TypeIndex)
                -> Maybe (Name, RealSrcSpan)
            extractBinding _fallbackSpan (ident, details) = case ident of
                Right name | Just rhsSpan <- getBindingSpan details ->
                    Just (name, rhsSpan)
                _ -> Nothing

            getBindingSpan :: IdentifierDetails TypeIndex -> Maybe RealSrcSpan
            getBindingSpan IdentifierDetails{identInfo = identInfo'} =
                listToMaybe $ mapMaybe spanFromCtx (toList identInfo')
              where
                spanFromCtx (ValBind _ _ (Just s)) = Just s
                spanFromCtx (PatternBind _ _ (Just s)) = Just s
                spanFromCtx _ = Nothing

    collectTaintedFunctionParams
        :: [HieAST TypeIndex]
        -> Map ByteString [ByteString]
        -> Set Name
        -> Set ByteString
        -> Map ByteString (Set ByteString)
    collectTaintedFunctionParams asts paramMap minted mintedOccs =
        foldl' mergeMaps Map.empty (map (collectFromAst paramMap minted mintedOccs) asts)
      where
        mergeMaps :: Map ByteString (Set ByteString) -> Map ByteString (Set ByteString)
                  -> Map ByteString (Set ByteString)
        mergeMaps = Map.unionWith Set.union

        collectFromAst
            :: Map ByteString [ByteString]
            -> Set Name
            -> Set ByteString
            -> HieAST TypeIndex
            -> Map ByteString (Set ByteString)
        collectFromAst params mintedNames mintedOccs' =
            go Map.empty
          where
            go acc n@Node{nodeChildren = children} =
                let acc' = case callInfo n of
                        Just (fnName, args) ->
                            case Map.lookup fnName params of
                                Just paramNames ->
                                    let tainted =
                                            Set.fromList
                                                [ param
                                                | (arg, param) <- zip args paramNames
                                                , argMentionsMinted mintedNames mintedOccs' arg
                                                ]
                                    in if Set.null tainted
                                       then acc
                                       else Map.insertWith Set.union fnName tainted acc
                                Nothing -> acc
                        Nothing -> acc
                in foldl' go acc' children

            callInfo :: HieAST TypeIndex -> Maybe (ByteString, [HieAST TypeIndex])
            callInfo node = do
                guard $ nodeHasAnnotation hsAppAnnotation node
                let (headNode, args) = appSpine node
                fnName <- headFunctionOcc headNode
                pure (fnName, args)

            headFunctionOcc :: HieAST TypeIndex -> Maybe ByteString
            headFunctionOcc headNode =
                listToMaybe
                    [ BS8.pack $ occNameString $ nameOccName identName
                    | (Right identName, _) <- Map.assocs $ nodeIdentifiers $ nodeInfo headNode
                    ]

        argMentionsMinted :: Set Name -> Set ByteString -> HieAST TypeIndex -> Bool
        argMentionsMinted mintedNames mintedOccs' arg =
            containsTxInfoMint arg
                || usesMintedBinding mintedNames arg
                || spanMentionsOccs mintedOccs' (nodeSpan arg)

        spanMentionsOccs :: Set ByteString -> RealSrcSpan -> Bool
        spanMentionsOccs occs spanToCheck = fromMaybe False $ do
            src <- slice spanToCheck (hie_hs_src hie)
            pure $ any (\occ -> occ `isWordInBS` src) (Set.toList occs)

    isWordInBS :: ByteString -> ByteString -> Bool
    isWordInBS word src = case BS8.breakSubstring word src of
        (before, after)
            | BS8.null after -> False
            | otherwise ->
                let afterWord = BS8.drop (BS8.length word) after
                    beforeOk = BS8.null before || not (isIdentCharBS (BS8.last before))
                    afterOk = BS8.null afterWord || not (isIdentCharBS (BS8.head afterWord))
                in (beforeOk && afterOk) || (word `isWordInBS` BS8.tail after)

    isIdentCharBS :: Char -> Bool
    isIdentCharBS c = isAlphaNum c || c == '_' || c == '\''

    currencySymbolValueOfNameMetas :: [NameMeta]
    currencySymbolValueOfNameMetas =
        [ NameMeta
            { nameMetaName = "currencySymbolValueOf"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V1.Value"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = "currencySymbolValueOf"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V2.Value"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = "currencySymbolValueOf"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V3.Value"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , "currencySymbolValueOf" `plutusTxNameFrom` "PlutusTx.Value"
        ]

    txInfoMintNameMetas :: [NameMeta]
    txInfoMintNameMetas =
        [ NameMeta
            { nameMetaName = "txInfoMint"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V1.Contexts"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = "txInfoMint"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V2.Contexts"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = "txInfoMint"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V3.Contexts"
            , nameMetaPackage = "plutus-ledger-api"
            }
        ]


analyseValidityIntervalMisuse
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseValidityIntervalMisuse insId hie curNode = do
    addObservations $ mkObservation insId hie <$> matchNode curNode
  where
    allHieAsts :: [HieAST TypeIndex]
    allHieAsts = Map.elems $ getAsts $ hie_asts hie

    finiteCheckSpanSet :: Set RealSrcSpan
    finiteCheckSpanSet =
        Set.fromList $ concatMap collectFiniteCheckSpans allHieAsts

    checkedRangeSpanSet :: Set RealSrcSpan
    checkedRangeSpanSet =
        Set.fromList $ concatMap collectCheckedRangeArgSpans allHieAsts

    invalidCheckedRangeSpanSet :: Set RealSrcSpan
    invalidCheckedRangeSpanSet =
        let isCoveredBy spanSet spanToCheck =
                any (`spanContains` spanToCheck) (Set.toList spanSet)
        in Set.filter
            (\spanToCheck ->
                not (isCoveredBy finiteCheckSpanSet spanToCheck)
            )
            checkedRangeSpanSet

    matchNode :: HieAST TypeIndex -> Slist RealSrcSpan
    matchNode node
        | nodeSpan node `Set.member` invalidCheckedRangeSpanSet = S.one $ nodeSpan node
        | otherwise = mempty

    collectCheckedRangeArgSpans :: HieAST TypeIndex -> [RealSrcSpan]
    collectCheckedRangeArgSpans node =
        let here = case checkCall node of
                Just (call, args) -> map nodeSpan (rangeArgs call args)
                Nothing -> []
        in here <> concatMap collectCheckedRangeArgSpans (nodeChildren node)

    checkCall :: HieAST TypeIndex -> Maybe (Text, [HieAST TypeIndex])
    checkCall node =
        appCheckCall node <|> opCheckCall node

    appCheckCall :: HieAST TypeIndex -> Maybe (Text, [HieAST TypeIndex])
    appCheckCall node = do
        guard $ nodeHasAnnotation hsAppAnnotation node
        let (headNode, args) = appSpine node
        contains <- pure $ nodeHasAnyNameMeta containsNameMetas headNode
        member <- pure $ nodeHasAnyNameMeta memberNameMetas headNode
        before <- pure $ nodeHasAnyNameMeta beforeNameMetas headNode
        after <- pure $ nodeHasAnyNameMeta afterNameMetas headNode
        if contains then pure ("contains", args)
        else if member then pure ("member", args)
        else if before then pure ("before", args)
        else if after then pure ("after", args)
        else empty

    opCheckCall :: HieAST TypeIndex -> Maybe (Text, [HieAST TypeIndex])
    opCheckCall node = do
        guard $ nodeHasAnnotation opAppAnnotation node
        lhsNode:opNode:rhsNode:_ <- Just $ nodeChildren node
        contains <- pure $ nodeHasAnyNameMeta containsNameMetas opNode
        member <- pure $ nodeHasAnyNameMeta memberNameMetas opNode
        before <- pure $ nodeHasAnyNameMeta beforeNameMetas opNode
        after <- pure $ nodeHasAnyNameMeta afterNameMetas opNode
        if contains then pure ("contains", [lhsNode, rhsNode])
        else if member then pure ("member", [lhsNode, rhsNode])
        else if before then pure ("before", [lhsNode, rhsNode])
        else if after then pure ("after", [lhsNode, rhsNode])
        else empty

    rangeArgs :: Text -> [HieAST TypeIndex] -> [HieAST TypeIndex]
    rangeArgs = \case
        "contains" -> take 2
        "member" -> take 1 . drop 1
        "before" -> take 1 . drop 1
        "after" -> take 1 . drop 1
        _ -> const []

    collectFiniteCheckSpans :: HieAST TypeIndex -> [RealSrcSpan]
    collectFiniteCheckSpans = go
      where
        go node =
            let here =
                    if isFiniteCheckNode node
                        then [nodeSpan node]
                        else []
            in here <> concatMap go (nodeChildren node)

        isFiniteCheckNode :: HieAST TypeIndex -> Bool
        isFiniteCheckNode node =
            (nodeHasAnnotation hsCaseAnnotation node || nodeHasAnnotation hsLetAnnotation node)
            && subtreeHasNameMeta boundFunctionNameMetas node
            && subtreeHasNameMeta boundConstructorNameMetas node
            && subtreeHasNameMeta finiteConstructorNameMetas node

    subtreeHasNameMeta :: [NameMeta] -> HieAST TypeIndex -> Bool
    subtreeHasNameMeta metas node =
        nodeHasAnyNameMeta metas node || any (subtreeHasNameMeta metas) (nodeChildren node)

    nodeHasAnyNameMeta :: [NameMeta] -> HieAST TypeIndex -> Bool
    nodeHasAnyNameMeta metas node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
        in any (\pair -> any (`hieMatchNameMeta` pair) metas) idents

    nodeHasAnnotation :: NodeAnnotation -> HieAST TypeIndex -> Bool
    nodeHasAnnotation ann node =
        let NodeInfo{nodeAnnotations = nodeAnnotations'} = nodeInfo node
        in ann `Set.member` Set.map toNodeAnnotation nodeAnnotations'

    appSpine :: HieAST TypeIndex -> (HieAST TypeIndex, [HieAST TypeIndex])
    appSpine node = case node of
        n@Node{nodeChildren = appFun:arg:_}
            | nodeHasAnnotation hsAppAnnotation n ->
                let (f, args) = appSpine appFun
                in (f, args <> [arg])
        _ -> (node, [])

    hsAppAnnotation :: NodeAnnotation
    hsAppAnnotation = mkNodeAnnotation "HsApp" "HsExpr"

    opAppAnnotation :: NodeAnnotation
    opAppAnnotation = mkNodeAnnotation "OpApp" "HsExpr"

    hsCaseAnnotation :: NodeAnnotation
    hsCaseAnnotation = mkNodeAnnotation "HsCase" "HsExpr"

    hsLetAnnotation :: NodeAnnotation
    hsLetAnnotation = mkNodeAnnotation "HsLet" "HsExpr"

    containsNameMetas :: [NameMeta]
    containsNameMetas = intervalNameMetas "contains"

    memberNameMetas :: [NameMeta]
    memberNameMetas = intervalNameMetas "member"

    beforeNameMetas :: [NameMeta]
    beforeNameMetas = intervalNameMetas "before"

    afterNameMetas :: [NameMeta]
    afterNameMetas = intervalNameMetas "after"

    boundFunctionNameMetas :: [NameMeta]
    boundFunctionNameMetas = concatMap intervalNameMetas
        [ "lowerBound"
        , "upperBound"
        ]

    boundConstructorNameMetas :: [NameMeta]
    boundConstructorNameMetas = concatMap intervalNameMetas
        [ "LowerBound"
        , "UpperBound"
        ]

    finiteConstructorNameMetas :: [NameMeta]
    finiteConstructorNameMetas =
        concatMap intervalNameMetas ["Finite"]

    intervalNameMetas :: Text -> [NameMeta]
    intervalNameMetas name =
        [ NameMeta
            { nameMetaName = name
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V1.Interval"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = name
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V2.Interval"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = name
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V3.Interval"
            , nameMetaPackage = "plutus-ledger-api"
            }
        ]

    spanContains :: RealSrcSpan -> RealSrcSpan -> Bool
    spanContains outer inner =
        let startsAfter =
                srcSpanStartLine inner > srcSpanStartLine outer
                || (srcSpanStartLine inner == srcSpanStartLine outer
                    && srcSpanStartCol inner >= srcSpanStartCol outer)
            endsBefore =
                srcSpanEndLine inner < srcSpanEndLine outer
                || (srcSpanEndLine inner == srcSpanEndLine outer
                    && srcSpanEndCol inner <= srcSpanEndCol outer)
        in startsAfter && endsBefore


analysePrecisionLossDivisionBeforeMultiply
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analysePrecisionLossDivisionBeforeMultiply insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchNode curNode
  where
    allHieAsts :: [HieAST TypeIndex]
    allHieAsts = Map.elems $ getAsts $ hie_asts hie

    divisionBindings :: Set Name
    divisionBindings =
        foldMap (collectDivisionBindings (hie_hs_src hie)) allHieAsts

    divisionBindingOccs :: Set ByteString
    divisionBindingOccs =
        Set.map (BS8.pack . occNameString . nameOccName) divisionBindings

    matchNode :: HieAST TypeIndex -> Slist RealSrcSpan
    matchNode node =
        let direct = createMatch precisionLossPattern hie node
            tainted =
                memptyIfFalse
                    (hieMatchPatternAst hie node multiplyPattern
                        && spanMentionsDivisionBinding (nodeSpan node))
                    (S.one $ nodeSpan node)
        in direct <> tainted

    spanMentionsDivisionBinding :: RealSrcSpan -> Bool
    spanMentionsDivisionBinding spanToCheck = fromMaybe False $ do
        src <- slice spanToCheck (hie_hs_src hie)
        pure $ any (`isWordInBS` src) (Set.toList divisionBindingOccs)

    collectDivisionBindings :: ByteString -> HieAST TypeIndex -> Set Name
    collectDivisionBindings hsSrc rootNode =
        let directlyTainted = collectDirectBindings rootNode
            allBindings = collectAllBindingsWithSpans rootNode
        in expandTransitively allBindings directlyTainted
      where
        collectDirectBindings :: HieAST TypeIndex -> Set Name
        collectDirectBindings = go Set.empty
          where
            go acc n@Node{nodeSpan = nodeSpan', nodeChildren = children} =
                let info = nodeInfo n
                    acc' = foldl' (insertBinding nodeSpan') acc
                        (Map.assocs $ nodeIdentifiers info)
                in foldl' go acc' children

            insertBinding
                :: RealSrcSpan
                -> Set Name
                -> (Identifier, IdentifierDetails TypeIndex)
                -> Set Name
            insertBinding fallbackSpan acc (ident, details) = case ident of
                Right name ->
                    let fromBindingSpan = case getBindingSpan details of
                            Just rhsSpan -> spanContainsDivision rhsSpan
                            Nothing -> False
                        fromFallbackSpan =
                            isBindingDetails details && spanContainsDivision fallbackSpan
                        fromSourceSearch = bindingRhsContainsDivision name
                    in if fromBindingSpan || fromFallbackSpan || fromSourceSearch
                       then Set.insert name acc
                       else acc
                _ -> acc

            bindingRhsContainsDivision :: Name -> Bool
            bindingRhsContainsDivision name =
                let nameBS = BS8.pack $ occNameString $ nameOccName name
                    srcLines = BS8.lines hsSrc
                    hasDivBinding line =
                        ((nameBS <> " = ") `BS8.isInfixOf` line || (nameBS <> " =") `BS8.isInfixOf` line)
                        && lineHasDivision line
                in any hasDivBinding srcLines

        collectAllBindingsWithSpans :: HieAST TypeIndex -> [(Name, RealSrcSpan)]
        collectAllBindingsWithSpans = go
          where
            go n@Node{nodeSpan = nodeSpan', nodeChildren = children} =
                let info = nodeInfo n
                    bindings = mapMaybe (extractBinding nodeSpan')
                        (Map.assocs $ nodeIdentifiers info)
                in bindings ++ concatMap go children

            extractBinding
                :: RealSrcSpan
                -> (Identifier, IdentifierDetails TypeIndex)
                -> Maybe (Name, RealSrcSpan)
            extractBinding fallbackSpan (ident, details) = case ident of
                Right name | Just rhsSpan <- getBindingSpan details ->
                    Just (name, rhsSpan)
                Right name | isBindingDetails details ->
                    Just (name, fallbackSpan)
                _ -> Nothing

        expandTransitively :: [(Name, RealSrcSpan)] -> Set Name -> Set Name
        expandTransitively allBindings = go
          where
            go tainted =
                let newTainted = Set.fromList
                        [ name
                        | (name, rhsSpan) <- allBindings
                        , not (Set.member name tainted)
                        , spanUsesTaintedName rhsSpan tainted
                        ]
                in if Set.null newTainted
                   then tainted
                   else go (tainted `Set.union` newTainted)

            spanUsesTaintedName :: RealSrcSpan -> Set Name -> Bool
            spanUsesTaintedName spanToCheck taintedNames = fromMaybe False $ do
                src <- slice spanToCheck hsSrc
                pure $ any (\n -> nameAsBS n `isWordIn` src) (Set.toList taintedNames)

            nameAsBS :: Name -> ByteString
            nameAsBS = BS8.pack . occNameString . nameOccName

            isWordIn :: ByteString -> ByteString -> Bool
            isWordIn word src = case BS8.breakSubstring word src of
                (before, after)
                    | BS8.null after -> False
                    | otherwise ->
                        let afterWord = BS8.drop (BS8.length word) after
                            beforeOk = BS8.null before || not (isIdentCharLocal (BS8.last before))
                            afterOk = BS8.null afterWord || not (isIdentCharLocal (BS8.head afterWord))
                        in (beforeOk && afterOk) || (word `isWordIn` BS8.tail after)

            isIdentCharLocal :: Char -> Bool
            isIdentCharLocal c = isAlphaNum c || c == '_' || c == '\''

        spanContainsDivision :: RealSrcSpan -> Bool
        spanContainsDivision spanToCheck = fromMaybe False $ do
            src <- slice spanToCheck hsSrc
            pure $ lineHasDivision src

        lineHasDivision :: ByteString -> Bool
        lineHasDivision src =
            isWordInBS "div" src
                || isWordInBS "quot" src
                || ("/" `BS8.isInfixOf` src)

        getBindingSpan :: IdentifierDetails TypeIndex -> Maybe RealSrcSpan
        getBindingSpan IdentifierDetails{identInfo = identInfo'} =
            listToMaybe $ mapMaybe spanFromCtx (toList identInfo')
          where
            spanFromCtx (ValBind _ _ (Just s)) = Just s
            spanFromCtx (PatternBind _ _ (Just s)) = Just s
            spanFromCtx _ = Nothing

        isBindingDetails :: IdentifierDetails TypeIndex -> Bool
        isBindingDetails IdentifierDetails{identInfo = identInfo'} =
            any isBindingCtx identInfo'
          where
            isBindingCtx (ValBind _ _ _) = True
            isBindingCtx (PatternBind _ _ _) = True
            isBindingCtx _ = False

    precisionLossPattern :: PatternAst
    precisionLossPattern =
        opApp divisionExpr mulOp (?)
        ||| app (app mulFun divisionExpr) (?)

    multiplyPattern :: PatternAst
    multiplyPattern =
        opApp (?) mulOp (?) ||| app (app mulFun (?)) (?)

    divisionExpr :: PatternAst
    divisionExpr =
        opApp (?) divOp (?) ||| app (app divFun (?)) (?)

    divOp :: PatternAst
    divOp = anyNamesToPatternAst divOpNames

    divFun :: PatternAst
    divFun = anyNamesToPatternAst divFunNames

    mulOp :: PatternAst
    mulOp = anyNamesToPatternAst mulOpNames

    mulFun :: PatternAst
    mulFun = anyNamesToPatternAst mulFunNames

    divOpNames :: NonEmpty NameMeta
    divOpNames =
        "div" `plutusTxNameFrom` "PlutusTx.Prelude" :|
            [ "quot" `plutusTxNameFrom` "PlutusTx.Prelude"
            , "/" `plutusTxNameFrom` "PlutusTx.Prelude"
            , "div" `baseNameFrom` "GHC.Real"
            , "quot" `baseNameFrom` "GHC.Real"
            , "/" `baseNameFrom` "GHC.Real"
            ]

    divFunNames :: NonEmpty NameMeta
    divFunNames =
        "div" `plutusTxNameFrom` "PlutusTx.Prelude" :|
            [ "quot" `plutusTxNameFrom` "PlutusTx.Prelude"
            , "div" `baseNameFrom` "GHC.Real"
            , "quot" `baseNameFrom` "GHC.Real"
            ]

    mulOpNames :: NonEmpty NameMeta
    mulOpNames =
        "*" `plutusTxNameFrom` "PlutusTx.Prelude" :|
            [ "mul" `plutusTxNameFrom` "PlutusTx.Prelude"
            , "*" `baseNameFrom` "GHC.Num"
            ]

    mulFunNames :: NonEmpty NameMeta
    mulFunNames =
        "*" `plutusTxNameFrom` "PlutusTx.Prelude" :|
            [ "mul" `plutusTxNameFrom` "PlutusTx.Prelude"
            , "*" `baseNameFrom` "GHC.Num"
            ]

    isWordInBS :: ByteString -> ByteString -> Bool
    isWordInBS word src = case BS8.breakSubstring word src of
        (before, after)
            | BS8.null after -> False
            | otherwise ->
                let afterWord = BS8.drop (BS8.length word) after
                    beforeOk = BS8.null before || not (isIdentChar (BS8.last before))
                    afterOk = BS8.null afterWord || not (isIdentChar (BS8.head afterWord))
                in (beforeOk && afterOk) || (word `isWordInBS` BS8.tail after)

    isIdentChar :: Char -> Bool
    isIdentChar c = isAlphaNum c || c == '_' || c == '\''

analyseUnsafeFromBuiltinDataInHashComparison
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseUnsafeFromBuiltinDataInHashComparison insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchNode curNode
  where
    -- Collect all variables bound from expressions containing unsafeFromBuiltinData.
    -- We must scan the entire file so let/where/case bindings are visible regardless
    -- of the current node being visited.
    unsafeBindings :: Set Name
    unsafeBindings =
        let allHieAsts = Map.elems $ getAsts $ hie_asts hie
        in foldMap (collectUnsafeFromBuiltinDataBindings (hie_hs_src hie)) allHieAsts
    unsafeBindingOccs :: Set ByteString
    unsafeBindingOccs = collectUnsafeBindingOccsFromSource (hie_hs_src hie)
    unsafeComparisonLines :: Set Int
    unsafeComparisonLines =
        collectUnsafeComparisonLines (hie_hs_src hie) unsafeBindingOccs

    matchNode :: HieAST TypeIndex -> Slist RealSrcSpan
    matchNode node = case hashEqOperands node of
        Just (lhs, rhsNode)
            | (operandFromUnsafe lhs && typeMatchesHash rhsNode)
              || (operandFromUnsafe rhsNode && typeMatchesHash lhs)
              || (comparisonMentionsUnsafeBinding node && (typeMatchesHash lhs || typeMatchesHash rhsNode))
              || (comparisonLineMentionsUnsafeBinding node && (typeMatchesHash lhs || typeMatchesHash rhsNode)) ->
                S.one $ nodeSpan node
        _ -> mempty

    -- Check if operand either:
    -- 1. Directly contains unsafeFromBuiltinData, OR
    -- 2. Uses a variable that was bound from unsafeFromBuiltinData
    operandFromUnsafe :: HieAST TypeIndex -> Bool
    operandFromUnsafe node =
        containsUnsafeFromBuiltinData node
        || usesUnsafeBinding unsafeBindings unsafeBindingOccs node
        || nodeMentionsUnsafeBindingOcc node
        || subtreeSpanMentionsUnsafeBinding node

    hashEqOperands :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex)
    hashEqOperands node =
        opAppOperands node <|> appOperands node

    opAppOperands :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex)
    opAppOperands node = do
        guard $ nodeHasAnnotation opAppAnnotation node
        lhsNode:op:rhsNode:_ <- Just $ nodeChildren node
        guard $ isEqOpNode op
        pure (lhsNode, rhsNode)

    appOperands :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex)
    appOperands node = do
        guard $ nodeHasAnnotation hsAppAnnotation node
        let (headNode, args) = appSpine node
        case args of
            arg1:arg2:_ -> do
                guard $ nodeHasEqOpName headNode
                Just (arg1, arg2)
            [arg1] -> do
                guard $ isSectionNode headNode || isEqSectionBySource (hie_hs_src hie) headNode
                guard $ nodeHasEqOpName headNode || isEqSectionBySource (hie_hs_src hie) headNode
                let fixed = fromMaybe headNode (sectionOperand headNode)
                Just (fixed, arg1)
            _ -> Nothing

    opAppAnnotation :: NodeAnnotation
    opAppAnnotation = mkNodeAnnotation "OpApp" "HsExpr"

    hsAppAnnotation :: NodeAnnotation
    hsAppAnnotation = mkNodeAnnotation "HsApp" "HsExpr"

    nodeHasAnnotation :: NodeAnnotation -> HieAST TypeIndex -> Bool
    nodeHasAnnotation ann node =
        let NodeInfo{nodeAnnotations = nodeAnnotations'} = nodeInfo node
        in ann `Set.member` Set.map toNodeAnnotation nodeAnnotations'

    appSpine :: HieAST TypeIndex -> (HieAST TypeIndex, [HieAST TypeIndex])
    appSpine node = case node of
        n@Node{nodeChildren = appFun:arg:_}
            | nodeHasAnnotation hsAppAnnotation n ->
                let (f, args) = appSpine appFun
                in (f, args <> [arg])
        _ -> (node, [])

    -- Only trigger on equality comparisons, not ordering
    eq :: NameMeta
    eq  = "==" `ghcPrimNameFrom` "GHC.Classes"

    -- Types that can contain unvalidated hashes from BuiltinData
    pubKeyHashType, scriptHashType, credentialType, addressType :: PatternType
    pubKeyHashType = ledgerApiTypePattern "PubKeyHash" "Crypto"
    scriptHashType = ledgerApiTypePattern "ScriptHash" "Scripts"
    credentialType = ledgerApiTypePattern "Credential" "Credential"
    addressType    = ledgerApiTypePattern "Address" "Address"

    ledgerApiTypePattern :: Text -> Text -> PatternType
    ledgerApiTypePattern name moduleSuffix = NameMeta
        { nameMetaName       = name
        , nameMetaModuleName = ModuleName $ "PlutusLedgerApi.V1." <> moduleSuffix
        , nameMetaPackage    = "plutus-ledger-api"
        } |:: []

    typeMatchesHash :: HieAST TypeIndex -> Bool
    typeMatchesHash = go
      where
        go n =
            nodeTypeMatchesHash n || any go (nodeChildren n)

        nodeTypeMatchesHash :: HieAST TypeIndex -> Bool
        nodeTypeMatchesHash n =
            let NodeInfo{nodeType = tys} = nodeInfo n
                matches tix =
                    hieMatchPatternType (hie_types hie) pubKeyHashType tix
                    || hieMatchPatternType (hie_types hie) scriptHashType tix
                    || hieMatchPatternType (hie_types hie) credentialType tix
                    || hieMatchPatternType (hie_types hie) addressType tix
            in any matches tys

    containsUnsafeFromBuiltinData :: HieAST TypeIndex -> Bool
    containsUnsafeFromBuiltinData node =
        nodeHasUnsafeFromBuiltinData node || any containsUnsafeFromBuiltinData (nodeChildren node)

    nodeHasUnsafeFromBuiltinData :: HieAST TypeIndex -> Bool
    nodeHasUnsafeFromBuiltinData node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
            unsafeNm = "unsafeFromBuiltinData" `plutusTxNameFrom` "PlutusTx.IsData.Class"
        in any (hieMatchNameMeta unsafeNm) idents

    -- Check if a node uses any of the variables bound from unsafeFromBuiltinData
    usesUnsafeBinding :: Set Name -> Set ByteString -> HieAST TypeIndex -> Bool
    usesUnsafeBinding bindings bindingOccs = go
      where
        go n@Node{nodeChildren = children} =
            let info = nodeInfo n
                useHere = any (isNameUse bindings bindingOccs)
                    (Map.assocs $ nodeIdentifiers info)
            in useHere || any go children

        isNameUse
            :: Set Name
            -> Set ByteString
            -> (Identifier, IdentifierDetails TypeIndex)
            -> Bool
        isNameUse bs occs (ident, IdentifierDetails{identInfo = identInfo'}) =
            case ident of
                Right identName ->
                    let occ = BS8.pack $ occNameString $ nameOccName identName
                    in Set.member Use identInfo'
                        && (Set.member identName bs || Set.member occ occs)
                        || Set.member occ occs
                _ -> False

    nodeMentionsUnsafeBindingOcc :: HieAST TypeIndex -> Bool
    nodeMentionsUnsafeBindingOcc = go
      where
        go n@Node{nodeChildren = children} =
            let info = nodeInfo n
                mentionsHere =
                    any (mentionsOcc unsafeBindingOccs)
                        (Map.assocs $ nodeIdentifiers info)
            in mentionsHere || any go children

        mentionsOcc
            :: Set ByteString
            -> (Identifier, IdentifierDetails TypeIndex)
            -> Bool
        mentionsOcc occs (ident, _) = case ident of
            Right identName ->
                let occ = BS8.pack $ occNameString $ nameOccName identName
                in Set.member occ occs
            _ -> False

    -- Fallback for cases where identifiers don't show up in the HIE tree:
    -- check if the operand span text mentions any unsafe binding occurrences.
    comparisonMentionsUnsafeBinding :: HieAST TypeIndex -> Bool
    comparisonMentionsUnsafeBinding = subtreeSpanMentionsUnsafeBinding

    comparisonLineMentionsUnsafeBinding :: HieAST TypeIndex -> Bool
    comparisonLineMentionsUnsafeBinding node =
        let line = srcSpanStartLine (nodeSpan node)
        in Set.member line unsafeComparisonLines

    subtreeSpanMentionsUnsafeBinding :: HieAST TypeIndex -> Bool
    subtreeSpanMentionsUnsafeBinding node =
        let here = spanMentionsUnsafeBinding (nodeSpan node)
        in here || any subtreeSpanMentionsUnsafeBinding (nodeChildren node)

    spanMentionsUnsafeBinding :: RealSrcSpan -> Bool
    spanMentionsUnsafeBinding spanToCheck = fromMaybe False $ do
        src <- slice spanToCheck (hie_hs_src hie)
        pure $ any (\occ -> occ `isWordInBS` src) (Set.toList unsafeBindingOccs)

    isWordInBS :: ByteString -> ByteString -> Bool
    isWordInBS word src = case BS8.breakSubstring word src of
        (before, after)
            | BS8.null after -> False
            | otherwise ->
                let afterWord = BS8.drop (BS8.length word) after
                    beforeOk = BS8.null before || not (isIdentCharBS (BS8.last before))
                    afterOk = BS8.null afterWord || not (isIdentCharBS (BS8.head afterWord))
                in (beforeOk && afterOk) || (word `isWordInBS` BS8.tail after)

    isIdentCharBS :: Char -> Bool
    isIdentCharBS c = isAlphaNum c || c == '_' || c == '\''

    collectUnsafeComparisonLines :: ByteString -> Set ByteString -> Set Int
    collectUnsafeComparisonLines srcBytes occs =
        let lines' = BS8.lines srcBytes
            hasUnsafeOcc line = any (\occ -> occ `isWordInBS` line) (Set.toList occs)
            isEqLine line =
                "==" `BS8.isInfixOf` line && not ("/=" `BS8.isInfixOf` line)
        in Set.fromList
            [ idx
            | (idx, line) <- zip [1..] lines'
            , isEqLine line
            , hasUnsafeOcc line
            ]

    -- Collect all variables that are bound from expressions containing unsafeFromBuiltinData
    -- This uses transitive tracking: if datum = unsafeFromBuiltinData x, and
    -- Foo { bar } = datum, then bar is also tainted.
    collectUnsafeFromBuiltinDataBindings :: ByteString -> HieAST TypeIndex -> Set Name
    collectUnsafeFromBuiltinDataBindings hsSrc rootNode =
        let -- First pass: collect bindings where RHS contains "unsafeFromBuiltinData"
            directlyTainted = collectDirectBindings rootNode
            -- Also collect case scrutinee taints and propagate to pattern bindings
            caseTaints = collectCaseTaints rootNode
            initialTainted = directlyTainted `Set.union` caseTaints
            -- Collect all bindings for transitive expansion
            allBindings = collectAllBindingsWithSpans rootNode
            -- Expand transitively until fixpoint, then combine
            expanded = expandTransitively allBindings initialTainted
        in expanded
      where
        -- Collect bindings where RHS contains "unsafeFromBuiltinData"
        -- Uses multiple strategies: binding span from HIE, or searching source for binding patterns
        collectDirectBindings :: HieAST TypeIndex -> Set Name
        collectDirectBindings = go Set.empty
          where
            go acc n@Node{nodeSpan = nodeSpan', nodeChildren = children} =
                let info = nodeInfo n
                    acc' = foldl' (insertBinding nodeSpan') acc
                        (Map.assocs $ nodeIdentifiers info)
                in foldl' go acc' children

            insertBinding
                :: RealSrcSpan
                -> Set Name
                -> (Identifier, IdentifierDetails TypeIndex)
                -> Set Name
            insertBinding fallbackSpan acc (ident, details) = case ident of
                Right name ->
                    -- Try all strategies to check if this binding involves unsafeFromBuiltinData
                    let -- Strategy 1: Use binding span from HIE (works well for PatternBind)
                        fromBindingSpan = case getBindingSpan details of
                            Just rhsSpan -> spanContainsUnsafe rhsSpan
                            Nothing -> False
                        -- Strategy 2: Check if node span contains unsafeFromBuiltinData
                        fromFallbackSpan = isBindingDetails details && spanContainsUnsafe fallbackSpan
                        -- Strategy 3: Search source for "name = ... unsafeFromBuiltinData" pattern
                        -- This catches ValBind cases where HIE doesn't give us the RHS span
                        fromSourceSearch = bindingRhsContainsUnsafe name
                    in if fromBindingSpan || fromFallbackSpan || fromSourceSearch
                       then Set.insert name acc
                       else acc
                _ -> acc

            -- Search the source text for a binding like "name = ... unsafeFromBuiltinData ..."
            -- on the same line (for simple bindings that don't span multiple lines)
            bindingRhsContainsUnsafe :: Name -> Bool
            bindingRhsContainsUnsafe name =
                let nameBS = BS8.pack $ occNameString $ nameOccName name
                    srcLines = BS8.lines hsSrc
                    -- Simply check if "name = " and "unsafeFromBuiltinData" appear on the same line
                    hasUnsafeBinding line =
                        ((nameBS <> " = ") `BS8.isInfixOf` line || (nameBS <> " =") `BS8.isInfixOf` line)
                        && ("unsafeFromBuiltinData" `BS8.isInfixOf` line)
                in any hasUnsafeBinding srcLines

        -- Collect all bindings as (Name, RHS span) pairs for transitive expansion
        collectAllBindingsWithSpans :: HieAST TypeIndex -> [(Name, RealSrcSpan)]
        collectAllBindingsWithSpans = go
          where
            go n@Node{nodeSpan = nodeSpan', nodeChildren = children} =
                let info = nodeInfo n
                    bindings = mapMaybe (extractBinding nodeSpan')
                        (Map.assocs $ nodeIdentifiers info)
                in bindings ++ concatMap go children

            extractBinding
                :: RealSrcSpan
                -> (Identifier, IdentifierDetails TypeIndex)
                -> Maybe (Name, RealSrcSpan)
            extractBinding fallbackSpan (ident, details) = case ident of
                Right name | Just rhsSpan <- getBindingSpan details ->
                    Just (name, rhsSpan)
                Right name | isBindingDetails details ->
                    Just (name, fallbackSpan)
                _ -> Nothing

        -- Collect tainted names from case expressions
        -- If case scrutinee contains unsafeFromBuiltinData, all pattern bindings are tainted
        collectCaseTaints :: HieAST TypeIndex -> Set Name
        collectCaseTaints = go
          where
            go n@Node{nodeChildren = children} =
                let hereTaints =
                        if nodeHasAnnotation hsCaseAnnotation n
                            && any containsUnsafeFromBuiltinData children
                        then Set.fromList (collectMatchBindings n)
                        else Set.empty
                in hereTaints `Set.union` foldMap go children

            hsCaseAnnotation :: NodeAnnotation
            hsCaseAnnotation = mkNodeAnnotation "HsCase" "HsExpr"

            collectMatchBindings :: HieAST TypeIndex -> [Name]
            collectMatchBindings n@Node{nodeChildren = children} =
                let info = nodeInfo n
                    matchBinds =
                        [ name
                        | (Right name, IdentifierDetails{identInfo = identInfo'}) <-
                            Map.assocs $ nodeIdentifiers info
                        , MatchBind `Set.member` identInfo'
                        ]
                in matchBinds ++ concatMap collectMatchBindings children

        -- Expand tainted set transitively
        expandTransitively :: [(Name, RealSrcSpan)] -> Set Name -> Set Name
        expandTransitively allBindings = go
          where
            go tainted =
                let newTainted = Set.fromList
                        [ name
                        | (name, rhsSpan) <- allBindings
                        , not (Set.member name tainted)
                        , spanUsesTaintedName rhsSpan tainted
                        ]
                in if Set.null newTainted
                   then tainted
                   else go (tainted `Set.union` newTainted)

            spanUsesTaintedName :: RealSrcSpan -> Set Name -> Bool
            spanUsesTaintedName spanToCheck taintedNames = fromMaybe False $ do
                src <- slice spanToCheck hsSrc
                pure $ any (\n -> nameAsBS n `isWordIn` src) (Set.toList taintedNames)

            nameAsBS :: Name -> ByteString
            nameAsBS = BS8.pack . occNameString . nameOccName

            isWordIn :: ByteString -> ByteString -> Bool
            isWordIn word src = case BS8.breakSubstring word src of
                (before, after)
                    | BS8.null after -> False
                    | otherwise ->
                        let afterWord = BS8.drop (BS8.length word) after
                            beforeOk = BS8.null before || not (isIdentChar (BS8.last before))
                            afterOk = BS8.null afterWord || not (isIdentChar (BS8.head afterWord))
                        in (beforeOk && afterOk) || (word `isWordIn` BS8.tail after)

            isIdentChar :: Char -> Bool
            isIdentChar c = isAlphaNum c || c == '_' || c == '\''

        -- Helper functions shared across the above
        getBindingSpan :: IdentifierDetails TypeIndex -> Maybe RealSrcSpan
        getBindingSpan IdentifierDetails{identInfo = identInfo'} =
            listToMaybe $ mapMaybe spanFromCtx (toList identInfo')
          where
            spanFromCtx (ValBind _ _ (Just s)) = Just s
            spanFromCtx (PatternBind _ _ (Just s)) = Just s
            spanFromCtx _ = Nothing

        isBindingDetails :: IdentifierDetails TypeIndex -> Bool
        isBindingDetails IdentifierDetails{identInfo = identInfo'} =
            any isBindingCtx identInfo'
          where
            isBindingCtx (ValBind _ _ _) = True
            isBindingCtx (PatternBind _ _ _) = True
            isBindingCtx MatchBind = True
            isBindingCtx _ = False

        spanContainsUnsafe :: RealSrcSpan -> Bool
        spanContainsUnsafe spanToCheck = fromMaybe False $ do
            src <- slice spanToCheck hsSrc
            pure $ "unsafeFromBuiltinData" `BS8.isInfixOf` src

    -- Fallback: collect binding names from source for lines like
    --   datum = unsafeFromBuiltinData ...
    -- Only captures lower-case identifiers to avoid constructor names.
    collectUnsafeBindingOccsFromSource :: ByteString -> Set ByteString
    collectUnsafeBindingOccsFromSource srcBytes =
        let lines' = BS8.lines srcBytes
            prevs = "" : lines'
            nexts = drop 1 lines' <> [""]
            extractNames line =
                let trimmed = BS8.dropWhile isSpace line
                    lhsName = BS8.takeWhile isIdentChar trimmed
                    rhsNames = extractNamesAfterEquals trimmed
                    candidates = lhsName : rhsNames
                in filter isLowerIdent candidates
            isLowerIdent bs = not (BS8.null bs) && isLower (BS8.head bs)
            extractNamesAfterEquals bs =
                case BS8.breakSubstring "=" bs of
                    (_, rest) | BS8.null rest -> []
                    (_, rest) ->
                        let afterEq = BS8.drop 1 rest
                            name = BS8.takeWhile isIdentChar $ BS8.dropWhile isSpace afterEq
                        in (if BS8.null name then [] else [name]) <> extractNamesAfterEquals (BS8.drop 1 rest)
            isIdentChar c = isAlphaNum c || c == '_' || c == '\''
            addLine acc (prevLine, line, nextLine) =
                let unsafeNearby = any (BS8.isInfixOf "unsafeFromBuiltinData") [prevLine, line, nextLine]
                in if unsafeNearby && "=" `BS8.isInfixOf` line
                   then acc <> extractNames line
                   else acc
        in Set.fromList $ foldl' addLine [] (zip3 prevs lines' nexts)

    nodeHasEqOpName :: HieAST TypeIndex -> Bool
    nodeHasEqOpName node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
            matches = any (hieMatchNameMeta eq) idents
        in matches || any nodeHasEqOpName (nodeChildren node)

    isEqOpNode :: HieAST TypeIndex -> Bool
    isEqOpNode node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
        in any (hieMatchNameMeta eq) idents

    isSectionNode :: HieAST TypeIndex -> Bool
    isSectionNode node =
        nodeHasAnnotation sectionLAnnotation node || nodeHasAnnotation sectionRAnnotation node

    sectionLAnnotation :: NodeAnnotation
    sectionLAnnotation = mkNodeAnnotation "SectionL" "HsExpr"

    sectionRAnnotation :: NodeAnnotation
    sectionRAnnotation = mkNodeAnnotation "SectionR" "HsExpr"

    isEqSectionBySource :: ByteString -> HieAST TypeIndex -> Bool
    isEqSectionBySource srcBytes node = fromMaybe False $ do
        src <- slice (nodeSpan node) srcBytes
        let cleaned = BS8.filter (\c -> not (isSpace c) && c /= '(' && c /= ')') src
            -- Only check for equality operator, not ordering operators
            matchesEq =
                ("==" `BS8.isPrefixOf` cleaned && BS8.length cleaned > 2)
                || ("==" `BS8.isSuffixOf` cleaned && BS8.length cleaned > 2)
        pure matchesEq

    sectionOperand :: HieAST TypeIndex -> Maybe (HieAST TypeIndex)
    sectionOperand node =
        let nonOpChildren = filter (not . nodeHasEqOpName) (nodeChildren node)
        in listToMaybe nonOpChildren


analyseRedeemerSuppliedIndicesUniqueness
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseRedeemerSuppliedIndicesUniqueness insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchNode curNode
  where
    matchNode :: HieAST TypeIndex -> Slist RealSrcSpan
    matchNode node =
        let direct = case indexingCall node of
                Nothing -> mempty
                Just (indexSpan, arg1, arg2) ->
                    let triggers =
                            indexArgIsFromMultiIndexRedeemer arg1
                                || indexArgIsFromMultiIndexRedeemer arg2
                    in memptyIfFalse (triggers && not (hasUniquenessMarker indexSpan)) (S.one indexSpan)
            viaMap = case mapCall node of
                Nothing -> mempty
                Just (funArg, indicesArg)
                    | indicesContainerFromRedeemer indicesArg ->
                        let spans = collectIndexingSpans funArg
                        in S.slist $ filter (not . hasUniquenessMarker) spans
                    | otherwise -> mempty
            viaCall = case functionCallWithIndexContainer node of
                Nothing -> mempty
                Just (fnName, _) -> case Map.lookup fnName localFunRhsNodes of
                    Nothing -> mempty
                    Just rhsNodes ->
                        let spans = concatMap collectIndexingSpans rhsNodes
                        in S.slist $ filter (not . hasUniquenessMarker) spans
            viaLetScope = case letScopeIndexing node of
                Nothing -> mempty
                Just spans -> S.slist $ filter (not . hasUniquenessMarker) spans
        in direct <> viaMap <> viaCall <> viaLetScope

    indexingCall :: HieAST TypeIndex -> Maybe (RealSrcSpan, HieAST TypeIndex, HieAST TypeIndex)
    indexingCall node =
        hsAppIndexingCall node <|> opAppIndexingCall node
      where
        hsAppIndexingCall n = do
            guard $ nodeHasAnnotation hsAppAnnotation n
            let (headNode, args) = appSpine n
            a1 <- args !!? 0
            a2 <- args !!? 1
            guard $ nodeIsIndexingFunction headNode || callArgsLookIndexing a1 a2
            pure (nodeSpan n, a1, a2)

        opAppIndexingCall n = do
            guard $ nodeHasAnnotation opAppAnnotation n
            lhsNode:opNode:rhsNode:_ <- Just $ nodeChildren n
            guard $ nodeIsIndexingFunction opNode || callArgsLookIndexing lhsNode rhsNode
            pure (nodeSpan n, lhsNode, rhsNode)

        callArgsLookIndexing :: HieAST TypeIndex -> HieAST TypeIndex -> Bool
        callArgsLookIndexing a b =
            (nodeTypeIsInteger a && nodeTypeIsList b)
                || (nodeTypeIsList a && nodeTypeIsInteger b)

    nodeIsIndexingFunction :: HieAST TypeIndex -> Bool
    nodeIsIndexingFunction node =
        any typeIndexIsIndexingFunction (nodeTypeIndices node)
      where
        typeIndexIsIndexingFunction :: TypeIndex -> Bool
        typeIndexIsIndexingFunction ix = case funTwoArgs ix of
            Just (a1, a2) ->
                (typeIndexRootTyConName a1 == Just "Integer" && typeIndexIsList a2)
                    || (typeIndexIsList a1 && typeIndexRootTyConName a2 == Just "Integer")
            Nothing -> False

        funTwoArgs :: TypeIndex -> Maybe (TypeIndex, TypeIndex)
        funTwoArgs ix = do
            (a1, res1) <- funArgRes ix
            (a2, _res2) <- funArgRes res1
            pure (a1, a2)

        funArgRes :: TypeIndex -> Maybe (TypeIndex, TypeIndex)
        funArgRes ix = case hie_types hie Arr.! peelTypeIndex ix of
            HFunTy _ a b -> Just (a, b)
            _ -> Nothing

        peelTypeIndex :: TypeIndex -> TypeIndex
        peelTypeIndex ix = case hie_types hie Arr.! ix of
            HForAllTy _ inner -> peelTypeIndex inner
            HQualTy _ inner -> peelTypeIndex inner
            _ -> ix

        typeIndexIsList :: TypeIndex -> Bool
        typeIndexIsList ix = case typeIndexRootTyConName ix of
            -- In `.hie` types, the list type constructor can show up as either
            -- `[]` or `List` depending on how GHC renders it.
            Just "[]" -> True
            Just "List" -> True
            Just "BuiltinList" -> True
            _ -> False

        typeIndexRootTyConName :: TypeIndex -> Maybe String
        typeIndexRootTyConName ix = case hie_types hie Arr.! peelTypeIndex ix of
            HTyConApp IfaceTyCon{ifaceTyConName = tyConName} _ ->
                Just $ occNameString $ nameOccName tyConName
            _ -> Nothing

    indexArgIsFromMultiIndexRedeemer :: HieAST TypeIndex -> Bool
    indexArgIsFromMultiIndexRedeemer node =
        subtreeUsesAnyName indexValueBindings node
            || (subtreeHasAnyOccName ["indexByteString"] node
                    && (subtreeHasAnyOccName redeemerDecodeIndicators node || subtreeUsesAnyName indexContainerBindings node))
            || (subtreeHasIndexingCall node
                    && (subtreeHasAnyOccName redeemerDecodeIndicators node || subtreeUsesAnyName indexContainerBindings node))

    redeemerDecodeIndicators :: [String]
    redeemerDecodeIndicators =
        [ "unsafeFromBuiltinData"
        , "fromBuiltinData"
        , "unsafeDataAsB"
        , "unsafeDataAsList"
        , "unsafeDataAsConstr"
        ]

    subtreeHasIndexingCall :: HieAST TypeIndex -> Bool
    subtreeHasIndexingCall = go
      where
        go n@Node{nodeChildren = children} =
            isJust (indexingCall n) || any go children

    mapCall :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex)
    mapCall node = do
        guard $ nodeHasAnnotation hsAppAnnotation node
        let (headNode, args) = appSpine node
        guard $ nodeHasAnyOccName ["map"] headNode
        fn <- args !!? 0
        xs <- args !!? 1
        pure (fn, xs)

    functionCallWithIndexContainer :: HieAST TypeIndex -> Maybe (Name, [HieAST TypeIndex])
    functionCallWithIndexContainer node = do
        guard $ nodeHasAnnotation hsAppAnnotation node
        let (headNode, args) = appSpine node
        fnName <- headName headNode
        guard $ any indicesContainerFromRedeemer args
        pure (fnName, args)

    letScopeIndexing :: HieAST TypeIndex -> Maybe [RealSrcSpan]
    letScopeIndexing node = do
        guard $ nodeHasAnnotation hsLetAnnotation node
        guard $ indicesContainerFromRedeemer node
        let spans =
                [ span'
                | (span', arg1, arg2) <- collectIndexingCalls node
                , not (indexArgIsFromMultiIndexRedeemer arg1 || indexArgIsFromMultiIndexRedeemer arg2)
                ]
        guard $ not (null spans)
        pure spans

    headName :: HieAST TypeIndex -> Maybe Name
    headName node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
        in listToMaybe
            [ name
            | (Right name, IdentifierDetails{identInfo = identInfo'}) <- idents
            , Set.member Use identInfo'
            ]

    localFunRhsNodes :: Map Name [HieAST TypeIndex]
    localFunRhsNodes =
        let allHieAsts = Map.elems $ getAsts $ hie_asts hie
        in foldMap collectFunRhsNodes allHieAsts

    collectFunRhsNodes :: HieAST TypeIndex -> Map Name [HieAST TypeIndex]
    collectFunRhsNodes n@Node{nodeChildren = children}
        | hieMatchPatternAst hie n fun =
            let (patNodes, rhsNodes) = splitAtFirstRhs children
                funNames = collectValBindNames patNodes
            in Map.fromList [ (name, rhsNodes) | name <- Set.toList funNames ]
        | otherwise = foldMap collectFunRhsNodes children

    splitAtFirstRhs :: [HieAST TypeIndex] -> ([HieAST TypeIndex], [HieAST TypeIndex])
    splitAtFirstRhs = go []
      where
        go acc = \case
            [] -> (reverse acc, [])
            x:xs
                | hieMatchPatternAst hie x rhs -> (reverse acc, x:xs)
                | otherwise -> go (x:acc) xs

    collectValBindNames :: [HieAST TypeIndex] -> Set Name
    collectValBindNames = foldMap go
      where
        go :: HieAST TypeIndex -> Set Name
        go n@Node{nodeChildren = children} =
            let info = nodeInfo n
                bindsHere = Set.fromList
                    [ name
                    | (Right name, IdentifierDetails{identInfo = identInfo'}) <- Map.assocs (nodeIdentifiers info)
                    , any isValBindCtx identInfo'
                    ]
            in bindsHere <> foldMap go children

        isValBindCtx :: ContextInfo -> Bool
        isValBindCtx = \case
            ValBind _ _ _ -> True
            MatchBind -> True
            _ -> False

    collectIndexingSpans :: HieAST TypeIndex -> [RealSrcSpan]
    collectIndexingSpans = go
      where
        go n@Node{nodeChildren = children} =
            let here = case indexingCall n of
                    Just (indexSpan, arg1, arg2)
                        | (nodeTypeIsList arg1 && isVariableUse arg2)
                          || (nodeTypeIsList arg2 && isVariableUse arg1) ->
                            [indexSpan]
                    _ -> []
            in here <> concatMap go children

    collectIndexingCalls :: HieAST TypeIndex -> [(RealSrcSpan, HieAST TypeIndex, HieAST TypeIndex)]
    collectIndexingCalls = go
      where
        go n@Node{nodeChildren = children} =
            let here = case indexingCall n of
                    Just (indexSpan, arg1, arg2) -> [(indexSpan, arg1, arg2)]
                    Nothing -> []
            in here <> concatMap go children

    nodeTypeIsList :: HieAST TypeIndex -> Bool
    nodeTypeIsList node =
        any typeIndexIsList (nodeTypeIndices node)
      where
        typeIndexIsList ix = case hie_types hie Arr.! ix of
            HForAllTy _ inner -> typeIndexIsList inner
            HQualTy _ inner -> typeIndexIsList inner
            HTyConApp IfaceTyCon{ifaceTyConName = tyConName} _ ->
                let n = occNameString $ nameOccName tyConName
                in n == "[]" || n == "List" || n == "BuiltinList"
            _ -> False

    nodeTypeIsInteger :: HieAST TypeIndex -> Bool
    nodeTypeIsInteger node =
        any typeIndexIsInteger (nodeTypeIndices node)
      where
        typeIndexIsInteger ix = case hie_types hie Arr.! ix of
            HForAllTy _ inner -> typeIndexIsInteger inner
            HQualTy _ inner -> typeIndexIsInteger inner
            HTyConApp IfaceTyCon{ifaceTyConName = tyConName} _ ->
                occNameString (nameOccName tyConName) == "Integer"
            _ -> False

    nodeTypeIndices :: HieAST TypeIndex -> [TypeIndex]
    nodeTypeIndices node =
        let NodeInfo{nodeType = nodeTypes, nodeIdentifiers = idents} = nodeInfo node
            identTypes =
                [ ty
                | (_ident, IdentifierDetails{identType = Just ty}) <- Map.assocs idents
                ]
        in nodeTypes <> identTypes

    isVariableUse :: HieAST TypeIndex -> Bool
    isVariableUse node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
        in any
            (\(ident, IdentifierDetails{identInfo = identInfo'}) -> case ident of
                Right _name -> Set.member Use identInfo'
                _ -> False
            )
            idents

    indicesContainerFromRedeemer :: HieAST TypeIndex -> Bool
    indicesContainerFromRedeemer node =
        subtreeUsesAnyName indexContainerBindings node
            || (subtreeHasAnyOccName redeemerDecodeIndicators node && nodeTypeLooksLikeIntegerList node)

    nodeTypeLooksLikeIntegerList :: HieAST TypeIndex -> Bool
    nodeTypeLooksLikeIntegerList node =
        let isListTy =
                nodeTypeContainsTyConName "[]" node
                    || nodeTypeContainsTyConName "List" node
                    || nodeTypeContainsTyConName "BuiltinList" node
            hasInteger =
                nodeTypeContainsTyConName "Integer" node
        in isListTy && hasInteger

    hasUniquenessMarker :: RealSrcSpan -> Bool
    hasUniquenessMarker spanToCheck =
        let lineNo = srcSpanStartLine spanToCheck
        in lineHasMarker lineNo || lineHasMarker (lineNo - 1)

    lineHasMarker :: Int -> Bool
    lineHasMarker n
        | n <= 0 = False
        | otherwise =
            case (BS8.lines $ hie_hs_src hie) !!? (n - 1) of
                Nothing -> False
                Just line -> normalizedContainsMarker (normalizeMarkerLine line)

    markerNormalized :: ByteString
    markerNormalized = "plutstan uniqueness enforced"

    normalizeMarkerLine :: ByteString -> ByteString
    normalizeMarkerLine =
        BS8.unwords
            . BS8.words
            . BS8.map (\c -> if isAlphaNum c then toLower c else ' ')

    normalizedContainsMarker :: ByteString -> Bool
    normalizedContainsMarker = BS8.isInfixOf markerNormalized

    subtreeHasAnyOccName :: [String] -> HieAST TypeIndex -> Bool
    subtreeHasAnyOccName targets = go
      where
        go n@Node{nodeChildren = children} =
            nodeHasAnyOccName targets n || any go children

    nodeHasAnyOccName :: [String] -> HieAST TypeIndex -> Bool
    nodeHasAnyOccName targets node =
        any
            (\(ident, _details) -> case ident of
                Right name -> occNameString (nameOccName name) `elem` targets
                _ -> False
            )
            (Map.assocs $ nodeIdentifiers $ nodeInfo node)

    subtreeUsesAnyName :: Set Name -> HieAST TypeIndex -> Bool
    subtreeUsesAnyName targets = go
      where
        go n@Node{nodeChildren = children} =
            nodeUsesAnyName targets n || any go children

    nodeUsesAnyName :: Set Name -> HieAST TypeIndex -> Bool
    nodeUsesAnyName targets node =
        any
            (\(ident, IdentifierDetails{identInfo = identInfo'}) -> case ident of
                Right name -> Set.member Use identInfo' && Set.member name targets
                _ -> False
            )
            (Map.assocs $ nodeIdentifiers $ nodeInfo node)

    nodeHasAnnotation :: NodeAnnotation -> HieAST TypeIndex -> Bool
    nodeHasAnnotation ann node =
        let NodeInfo{nodeAnnotations = nodeAnnotations'} = nodeInfo node
        in ann `Set.member` Set.map toNodeAnnotation nodeAnnotations'

    hsAppAnnotation :: NodeAnnotation
    hsAppAnnotation = mkNodeAnnotation "HsApp" "HsExpr"

    opAppAnnotation :: NodeAnnotation
    opAppAnnotation = mkNodeAnnotation "OpApp" "HsExpr"

    hsLetAnnotation :: NodeAnnotation
    hsLetAnnotation = mkNodeAnnotation "HsLet" "HsExpr"

    appSpine :: HieAST TypeIndex -> (HieAST TypeIndex, [HieAST TypeIndex])
    appSpine node = case node of
        n@Node{nodeChildren = appFun:arg:_}
            | nodeHasAnnotation hsAppAnnotation n ->
                let (f, args) = appSpine appFun
                in (f, args <> [arg])
        _ -> (node, [])

    indexContainerBindings :: Set Name
    indexContainerBindings =
        expandTransitively bindingRhsNodes directIndexContainers

    directIndexContainers :: Set Name
    directIndexContainers = Set.fromList
        [ name
        | (name, rhsNode) <- Map.toList bindingRhsNodes
        , rhsIsIndexContainer rhsNode
        ]

    rhsIsIndexContainer :: HieAST TypeIndex -> Bool
    rhsIsIndexContainer rhsNode =
        subtreeHasAnyOccName redeemerDecodeIndicators rhsNode
            && (nodeTypeLooksLikeIntegerList rhsNode || nodeTypeContainsTyConName "BuiltinByteString" rhsNode)

    indexValueBindings :: Set Name
    indexValueBindings =
        expandTransitively bindingRhsNodes directIndexValues

    directIndexValues :: Set Name
    directIndexValues = Set.fromList
        [ name
        | (name, rhsNode) <- Map.toList bindingRhsNodes
        , nodeTypeContainsTyConName "Integer" rhsNode
        , rhsIsIndexValue rhsNode
        ]

    rhsIsIndexValue :: HieAST TypeIndex -> Bool
    rhsIsIndexValue rhsNode =
        (subtreeHasAnyOccName ["indexByteString"] rhsNode
            && (subtreeHasAnyOccName redeemerDecodeIndicators rhsNode || subtreeUsesAnyName indexContainerBindings rhsNode))
            || (subtreeHasIndexingCall rhsNode
                    && (subtreeHasAnyOccName redeemerDecodeIndicators rhsNode || subtreeUsesAnyName indexContainerBindings rhsNode))

    bindingRhsNodes :: Map Name (HieAST TypeIndex)
    bindingRhsNodes =
        let allHieAsts = Map.elems $ getAsts $ hie_asts hie
            spanIndex = foldMap collectSpanIndex allHieAsts
            bindingSpans = foldMap collectBindingSpans allHieAsts
        in Map.mapMaybe (`Map.lookup` spanIndex) bindingSpans

    collectSpanIndex :: HieAST TypeIndex -> Map RealSrcSpan (HieAST TypeIndex)
    collectSpanIndex n@Node{nodeSpan = span', nodeChildren = children} =
        Map.insertWith (\_old new -> new) span' n (foldMap collectSpanIndex children)

    collectBindingSpans :: HieAST TypeIndex -> Map Name RealSrcSpan
    collectBindingSpans = go mempty
      where
        go acc n@Node{nodeSpan = fallbackSpan, nodeChildren = children} =
            let info = nodeInfo n
                acc' = foldl' (insertBinding fallbackSpan) acc (Map.assocs $ nodeIdentifiers info)
            in foldl' go acc' children

        insertBinding
            :: RealSrcSpan
            -> Map Name RealSrcSpan
            -> (Identifier, IdentifierDetails TypeIndex)
            -> Map Name RealSrcSpan
        insertBinding fallbackSpan acc (ident, details) = case ident of
            Right name | Just bindSpan <- getBindingSpan details ->
                Map.insertWith (\_old new -> new) name bindSpan acc
            Right name | isBindingDetails details ->
                Map.insertWith (\_old new -> new) name fallbackSpan acc
            _ -> acc

        getBindingSpan :: IdentifierDetails TypeIndex -> Maybe RealSrcSpan
        getBindingSpan IdentifierDetails{identInfo = identInfo'} =
            listToMaybe $ mapMaybe spanFromCtx (toList identInfo')
          where
            spanFromCtx (ValBind _ _ (Just s)) = Just s
            spanFromCtx (PatternBind _ _ (Just s)) = Just s
            spanFromCtx _ = Nothing

        isBindingDetails :: IdentifierDetails TypeIndex -> Bool
        isBindingDetails IdentifierDetails{identInfo = identInfo'} =
            any isBindingCtx identInfo'
          where
            isBindingCtx (ValBind _ _ _) = True
            isBindingCtx (PatternBind _ _ _) = True
            isBindingCtx MatchBind = True
            isBindingCtx _ = False

    expandTransitively
        :: Map Name (HieAST TypeIndex)
        -> Set Name
        -> Set Name
    expandTransitively rhsNodes = go
      where
        go tainted =
            let newTainted = Set.fromList
                    [ name
                    | (name, rhsNode) <- Map.toList rhsNodes
                    , not (Set.member name tainted)
                    , subtreeUsesAnyName tainted rhsNode
                    ]
            in if Set.null newTainted
               then tainted
               else go (tainted <> newTainted)

    nodeTypeContainsTyConName :: String -> HieAST TypeIndex -> Bool
    nodeTypeContainsTyConName needle node =
        any (typeIndexContainsTyConName needle) (nodeTypeIndices node)

    typeIndexContainsTyConName :: String -> TypeIndex -> Bool
    typeIndexContainsTyConName needle ix =
        hieTypeContainsTyConName needle (hie_types hie Arr.! ix)

    hieTypeContainsTyConName :: String -> HieTypeFlat -> Bool
    hieTypeContainsTyConName needle ty = case ty of
        HTyConApp IfaceTyCon{ifaceTyConName = tyConName} (HieArgs args) ->
            let here = occNameString (nameOccName tyConName) == needle
            in here || any (hieTypeContainsTyConName needle . (hie_types hie Arr.!) . snd) args
        HFunTy _ a b ->
            hieTypeContainsTyConName needle (hie_types hie Arr.! a)
                || hieTypeContainsTyConName needle (hie_types hie Arr.! b)
        HForAllTy _ inner ->
            hieTypeContainsTyConName needle (hie_types hie Arr.! inner)
        HQualTy _ inner ->
            hieTypeContainsTyConName needle (hie_types hie Arr.! inner)
        _ -> False

analyseLazyAndInOnChainCode
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseLazyAndInOnChainCode insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchNode curNode
  where
    matchNode :: HieAST TypeIndex -> Slist RealSrcSpan
    matchNode node =
        ifCase node <> ifThenElseCase node

    ifCase :: HieAST TypeIndex -> Slist RealSrcSpan
    ifCase node = case ifExpr node of
        Nothing -> mempty
        Just (condNode, thenNode, elseNode)
            | branchHasThrow thenNode || branchHasThrow elseNode ->
                S.slist $ collectOutermostAnd condNode
            | otherwise -> mempty

    ifThenElseCase :: HieAST TypeIndex -> Slist RealSrcSpan
    ifThenElseCase node = case ifThenElseCall node of
        Nothing -> mempty
        Just (condNode, thenNode, elseNode)
            | branchHasThrow thenNode || branchHasThrow elseNode ->
                S.slist $ collectOutermostAnd condNode
            | otherwise -> mempty

    ifExpr :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex, HieAST TypeIndex)
    ifExpr n = do
        guard $ nodeHasAnnotation hsIfAnnotation n
        condNode:thenNode:elseNode:_ <- Just $ nodeChildren n
        pure (condNode, thenNode, elseNode)

    ifThenElseCall :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex, HieAST TypeIndex)
    ifThenElseCall n = do
        guard $ nodeHasAnnotation hsAppAnnotation n
        let (headNode, args) = appSpine n
        guard $ subtreeHasAnyOccName ["ifThenElse"] headNode
        condNode <- args !!? 0
        thenNode <- args !!? 1
        elseNode <- args !!? 2
        pure (condNode, thenNode, elseNode)

    branchHasThrow :: HieAST TypeIndex -> Bool
    branchHasThrow = subtreeHasAnyOccName ["error", "traceError"]

    collectOutermostAnd :: HieAST TypeIndex -> [RealSrcSpan]
    collectOutermostAnd = go Set.empty
      where
        go visited node = case andCall node of
            Just (reportSpan, _lhsNode, _rhsNode) -> [reportSpan]
            Nothing ->
                let direct = concatMap (go visited) (nodeChildren node)
                    viaCall = case callHeadName node of
                        Just name
                            | Set.member name visited -> []
                            | otherwise -> case Map.lookup name localFunRhsNodes of
                                Just rhsNodes -> concatMap (go (Set.insert name visited)) rhsNodes
                                Nothing -> []
                        Nothing -> []
                in direct <> viaCall

    callHeadName :: HieAST TypeIndex -> Maybe Name
    callHeadName node = do
        guard $ nodeHasAnnotation hsAppAnnotation node
        let (headNode, _args) = appSpine node
        headName headNode

    andCall :: HieAST TypeIndex -> Maybe (RealSrcSpan, HieAST TypeIndex, HieAST TypeIndex)
    andCall node =
        opAppAndCall node <|> hsAppAndCall node
      where
        opAppAndCall n = do
            guard $ nodeHasAnnotation opAppAnnotation n
            lhsNode:opNode:rhsNode:_ <- Just $ nodeChildren n
            guard $ subtreeHasAnyOccName ["&&"] opNode
            pure (nodeSpan opNode, lhsNode, rhsNode)

        hsAppAndCall n = do
            guard $ nodeHasAnnotation hsAppAnnotation n
            let (headNode, args) = appSpine n
            guard $ subtreeHasAnyOccName ["&&"] headNode
            lhsNode <- args !!? 0
            rhsNode <- args !!? 1
            pure (nodeSpan headNode, lhsNode, rhsNode)

    subtreeHasAnyOccName :: [String] -> HieAST TypeIndex -> Bool
    subtreeHasAnyOccName targets = go
      where
        go n@Node{nodeChildren = children} =
            nodeHasAnyOccName targets n || any go children

    nodeHasAnyOccName :: [String] -> HieAST TypeIndex -> Bool
    nodeHasAnyOccName targets node =
        any
            (\(ident, _details) -> case ident of
                Right name -> occNameString (nameOccName name) `elem` targets
                _ -> False
            )
            (Map.assocs $ nodeIdentifiers $ nodeInfo node)

    headName :: HieAST TypeIndex -> Maybe Name
    headName node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
        in listToMaybe
            [ name
            | (Right name, IdentifierDetails{identInfo = identInfo'}) <- idents
            , Set.member Use identInfo'
            ]

    nodeHasAnnotation :: NodeAnnotation -> HieAST TypeIndex -> Bool
    nodeHasAnnotation ann node =
        let NodeInfo{nodeAnnotations = nodeAnnotations'} = nodeInfo node
        in ann `Set.member` Set.map toNodeAnnotation nodeAnnotations'

    hsIfAnnotation :: NodeAnnotation
    hsIfAnnotation = mkNodeAnnotation "HsIf" "HsExpr"

    hsAppAnnotation :: NodeAnnotation
    hsAppAnnotation = mkNodeAnnotation "HsApp" "HsExpr"

    opAppAnnotation :: NodeAnnotation
    opAppAnnotation = mkNodeAnnotation "OpApp" "HsExpr"

    appSpine :: HieAST TypeIndex -> (HieAST TypeIndex, [HieAST TypeIndex])
    appSpine node = case node of
        n@Node{nodeChildren = appFun:arg:_}
            | nodeHasAnnotation hsAppAnnotation n ->
                let (f, args) = appSpine appFun
                in (f, args <> [arg])
        _ -> (node, [])

    localFunRhsNodes :: Map Name [HieAST TypeIndex]
    localFunRhsNodes =
        let allHieAsts = Map.elems $ getAsts $ hie_asts hie
        in foldMap collectFunRhsNodes allHieAsts

    collectFunRhsNodes :: HieAST TypeIndex -> Map Name [HieAST TypeIndex]
    collectFunRhsNodes n@Node{nodeChildren = children}
        | hieMatchPatternAst hie n fun =
            let (patNodes, rhsNodes) = splitAtFirstRhs children
                funNames = collectValBindNames patNodes
            in Map.fromList [ (name, rhsNodes) | name <- Set.toList funNames ]
        | otherwise = foldMap collectFunRhsNodes children

    splitAtFirstRhs :: [HieAST TypeIndex] -> ([HieAST TypeIndex], [HieAST TypeIndex])
    splitAtFirstRhs = go []
      where
        go acc = \case
            [] -> (reverse acc, [])
            x:xs
                | hieMatchPatternAst hie x rhs -> (reverse acc, x:xs)
                | otherwise -> go (x:acc) xs

    collectValBindNames :: [HieAST TypeIndex] -> Set Name
    collectValBindNames = foldMap go
      where
        go :: HieAST TypeIndex -> Set Name
        go n@Node{nodeChildren = children} =
            let info = nodeInfo n
                bindsHere = Set.fromList
                    [ name
                    | (Right name, IdentifierDetails{identInfo = identInfo'}) <- Map.assocs (nodeIdentifiers info)
                    , any isValBindCtx identInfo'
                    ]
            in bindsHere <> foldMap go children

        isValBindCtx :: ContextInfo -> Bool
        isValBindCtx = \case
            ValBind _ _ _ -> True
            MatchBind -> True
            _ -> False

{- | Check for occurrences lazy fields in all constructors. Ignores
@newtype@s. Currently HIE Ast doesn't have information whether the
data type is @newtype@ or not. So the algorithm ignores all data types
with a single constructor and single field inside that constructor.
-}
analyseLazyFields
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseLazyFields insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchLazyField curNode
  where
    matchLazyField :: HieAST TypeIndex -> Slist RealSrcSpan
    matchLazyField node = memptyIfFalse
        -- return empty list if it's not a data type
        (hieMatchPatternAst hie node dataDecl)
        -- get list of all constructors
        $ let constructors = filter
                (\n -> hieMatchPatternAst hie n constructor)
                (nodeChildren node)
          in case constructors of
              -- no constructors = not observations
              []  -> mempty
              -- single constructor
              [c] -> S.concatMap matchField $ extractFields False c
              -- multiple constructors = analyse everything
              cs  -> S.concatMap (S.concatMap matchField . extractFields True) cs

    -- Extract fields as AST nodes. Return empty list if only one field
    -- (as a workaround for the @newtype@ problem)
    --
    -- record constructors have the following children:
    --   1. One or many constraints (e.g. forall a . Num a =>)
    --   2. Constructor name.
    --   3. Dummy child with all fields as childrens
    -- plain constructors have constructor name and children in the same list
    extractFields :: Bool -> HieAST TypeIndex -> [HieAST TypeIndex]
    extractFields hasManyCtors ctor = case drop 1 $ dropWhile isConstraint $ nodeChildren ctor of
        [] -> []  -- no fields
        [n] ->  -- single field, maybe dummy record node
            if isDummyRecordNode n
            then case nodeChildren n of
                []      -> []
                [field] -> [field | hasManyCtors]
                fields  -> fields
            else [n | hasManyCtors]
        fields -> fields  -- plain constructor
      where
        -- simple check for the dummy AST node
        isDummyRecordNode :: HieAST TypeIndex -> Bool
        isDummyRecordNode = Set.null . nodeAnnotations . nodeInfo

        -- Not the constructor identifier
        isConstraint :: HieAST TypeIndex -> Bool
        isConstraint n = not $ hieMatchPatternAst hie n constructorNameIdentifier

    -- matches record fields non-recursively
    matchField :: HieAST TypeIndex -> Slist RealSrcSpan
    matchField = createMatch lazyField hie

{- | Check for occurrences of pattern matching on @_@ for sum types (except
literals).
-}
analysePatternMatch_
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analysePatternMatch_ insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchPatternMatch curNode
  where
    matchPatternMatch :: HieAST TypeIndex -> Slist RealSrcSpan
    matchPatternMatch node = memptyIfFalse
        -- return empty list if it's not a case or lambda case
        (hieMatchPatternAst hie node $ lambdaCase ||| case')
        -- get list of all case branches
        $ case nodeChildren node of
              -- no branches = not observations
              []     -> mempty
              -- lambda case, first kid is pattern matching
              [pm]   -> analyseBranches pm
              -- case, first kid is @case exp of@, the second is pattern matching
              _:pm:_ -> analyseBranches pm

    {- Check the pattern matching child on some particular expressions.

    -}
    analyseBranches :: HieAST TypeIndex -> Slist RealSrcSpan
    analyseBranches pm = case nodeChildren pm of
        -- if there is no children = no observations
        [] -> mempty
        -- we need to check first and all other children separately
        -- see 'isFirstPatternMatchBranchOk' comment to understand the first
        -- child's rules.
        c:cs -> memptyIfFalse (isFirstPatternMatchBranchOk c) $
            {- if the first child satisfies rules of the first pattern matching
            branch, then we need to find the child with pattern matching on @_@.
            If there is no such expression = all is good.
            -}
            case find (\x -> hieMatchPatternAst hie x (patternMatch_ (?))) cs of
                Nothing -> mempty
                Just e  -> S.one (nodeSpan e)

    {- The first pattern matching branch should not:
    1. Be empty (makes no sense)
    2. Be a literal pattern matching (e.g. on 'Int's or 'String's)
    In all other cases we can continue our matching checks with other children.
    -}
    isFirstPatternMatchBranchOk :: HieAST TypeIndex -> Bool
    isFirstPatternMatchBranchOk c = hieMatchPatternAst hie c patternMatchBranch &&
        case takeWhile isNotMatchArrow $ nodeChildren c of
            []  -> False
            [x] -> hieMatchPatternAst hie x notLiteral
            _:_ -> True
      where
        isNotMatchArrow :: HieAST TypeIndex -> Bool
        isNotMatchArrow n = hieMatchPatternAst hie n $ neg $ patternMatchArrow (?)

    notLiteral :: PatternAst
    notLiteral = neg
        -- general literal expression
        ( PatternAstConstant AnyLiteral
        -- since GHC-8.10 expression for literal in pattern matching
        ||| literalPat
        )

{- | Analyse HIE AST to find all operators which lack explicit fixity
declaration.

The algorithm is the following:

1. Traverse AST and discover all top-level operators and @infix@
declarations in a single pass.
2. Compare two resulting sets to find out operators without @infix@
declaration.
-}
analyseInfix
    :: HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseInfix hie curNode = do
    matchInfix curNode
    matchOperator curNode
  where
    -- adds to the state list of operator names defined in a single
    -- fixity declaration:
    -- infix 5 ***, +++, ???
    matchInfix :: HieAST TypeIndex -> State VisitorState ()
    matchInfix node@Node{..} = when
        (hieMatchPatternAst hie node fixity)
        (traverse_ addFixity $ concatMap nodeIds nodeChildren)

    -- add to state a singleton or empty list with the top-level
    -- operator definition:
    -- (+++) :: ...
    matchOperator :: HieAST TypeIndex -> State VisitorState ()
    matchOperator node@Node{..} = when
        (hieMatchPatternAst hie node typeSig)
        (whenJust
            -- do nothing when cannot extract name
            -- first child of a parent is a name of a function/operator
            (viaNonEmpty (extractOperatorName . head) nodeChildren)
            -- add each operator decl from a list (should be singleton list)
            (traverse_ (uncurry addOpDecl))
        )

    -- return AST node identifier names as a sized list of texts
    nodeIds :: HieAST TypeIndex -> [Text]
    nodeIds =
        concatMap fixityName
        . Map.keys
        . nodeIdentifiers
        . nodeInfo

    fixityName :: Identifier -> [Text]
    fixityName = \case
        Left _ -> []
        Right name -> [toText $ occNameString $ nameOccName name]

    extractOperatorName :: HieAST TypeIndex -> [(Text, RealSrcSpan)]
    extractOperatorName n@Node{..} =
        concatMap (topLevelOperatorName nodeSpan)
        $ Map.keys
        $ nodeIdentifiers (Stan.Hie.Compat.nodeInfo n)

    topLevelOperatorName :: RealSrcSpan -> Identifier -> [(Text, RealSrcSpan)]
    topLevelOperatorName srcSpan = \case
        Left _ -> []
        Right name ->
            let occName = nameOccName name
            -- return empty list if identifier name is not operator name
            in [(toText $ occNameString occName, srcSpan) | isSymOcc occName]

data MissingTxOutField
    = MissingReferenceScript
    | MissingStakingCredential
    | MissingValue
    | MissingDatum

instance Eq MissingTxOutField where
    MissingReferenceScript == MissingReferenceScript = True
    MissingStakingCredential == MissingStakingCredential = True
    MissingValue == MissingValue = True
    MissingDatum == MissingDatum = True
    _ == _ = False

data TxOutFieldUsage = TxOutFieldUsage
    { txOutFieldAddressChecked :: !Bool
    , txOutFieldStakingChecked :: !Bool
    , txOutFieldValueChecked :: !Bool
    , txOutFieldDatumChecked :: !Bool
    , txOutFieldReferenceChecked :: !Bool
    }

analyseMissingTxOutReferenceScriptCheck
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseMissingTxOutReferenceScriptCheck =
    analyseMissingTxOutFieldCheck MissingReferenceScript

analyseMissingTxOutStakingCredentialCheck
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseMissingTxOutStakingCredentialCheck =
    analyseMissingTxOutFieldCheck MissingStakingCredential

analyseMissingTxOutValueCheck
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseMissingTxOutValueCheck =
    analyseMissingTxOutFieldCheck MissingValue

analyseMissingTxOutDatumCheck
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseMissingTxOutDatumCheck =
    analyseMissingTxOutFieldCheck MissingDatum

analyseMissingTxOutFieldCheck
    :: MissingTxOutField
    -> Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseMissingTxOutFieldCheck missingField insId hie curNode =
    addObservations $ mkObservation insId hie <$> matchNode curNode
  where
    matchNode :: HieAST TypeIndex -> Slist RealSrcSpan
    matchNode node =
        memptyIfFalse
            (hieMatchPatternAst hie node fun && shouldFlagNode node)
            (S.one $ observationSpan node)

    observationSpan :: HieAST TypeIndex -> RealSrcSpan
    observationSpan node =
        fromMaybe (nodeSpan node) (findAnchor node)

    findAnchor :: HieAST TypeIndex -> Maybe RealSrcSpan
    findAnchor n@Node{nodeSpan = span', nodeChildren = children}
        | isOneLineSpan span'
            && nodeHasAnyOccName allFieldTokens n
            && not (spanLooksLikeBinding span') = Just span'
        | otherwise = asum (map findAnchor children)

    isOneLineSpan :: RealSrcSpan -> Bool
    isOneLineSpan span' =
        srcSpanStartLine span' == srcSpanEndLine span'

    spanLooksLikeBinding :: RealSrcSpan -> Bool
    spanLooksLikeBinding span' = case srcLineAt (srcSpanStartLine span') of
        Nothing -> False
        Just line -> case parseBindingLhs line of
            Nothing -> False
            Just lhsOcc -> lhsOcc `elem`
                [ "txOutAddress"
                , "txOutValue"
                , "txOutDatum"
                , "txOutReferenceScript"
                , "referenceScript"
                ]
    shouldFlagNode :: HieAST TypeIndex -> Bool
    shouldFlagNode node =
        let txOutUsageCandidates = usageCandidates node
            txOutEvidence = hasTxOutEvidence node
        in txOutEvidence
            && any (\usage -> checkedFieldsCount usage >= 3 && isMissingField missingField usage) txOutUsageCandidates

    usageCandidates :: HieAST TypeIndex -> [TxOutFieldUsage]
    usageCandidates node = case Set.toList txOutVars of
        [] -> [detectTxOutFieldUsage node]
        _ ->
            let varGroups = txOutAliasGroups txOutVars (nodeSpan node)
                allowGlobalAliasFallback = length varGroups == 1
                groupedUsage =
                    map
                        (\varGroup ->
                            detectTxOutFieldUsageForVars
                                allowGlobalAliasFallback
                                varGroup
                                node
                        )
                        varGroups
                fallbackGeneric =
                    [ detectTxOutFieldUsage node
                    | allowGlobalAliasFallback
                    ]
            in groupedUsage <> fallbackGeneric
      where
        txOutVars = collectTxOutVariableNames node

    detectTxOutFieldUsage :: HieAST TypeIndex -> TxOutFieldUsage
    detectTxOutFieldUsage node =
        let addressEqImpliesStaking =
                subtreeHasAddressEquality node
        in TxOutFieldUsage
        { txOutFieldAddressChecked = subtreeHasAnyTxOutFieldToken addressTokens node
        , txOutFieldStakingChecked =
            subtreeHasAnyTxOutFieldToken stakingTokens node || addressEqImpliesStaking
        , txOutFieldValueChecked = subtreeHasAnyTxOutFieldToken valueTokens node
        , txOutFieldDatumChecked = subtreeHasAnyTxOutFieldToken datumTokens node
        , txOutFieldReferenceChecked = subtreeHasAnyTxOutFieldToken referenceTokens node
        }

    detectTxOutFieldUsageForVars :: Bool -> Set Name -> HieAST TypeIndex -> TxOutFieldUsage
    detectTxOutFieldUsageForVars allowGlobalAliasFallback varGroup node =
        let span' = nodeSpan node
            aliasOccsByField =
                let scopedAliasOccs = spanRecordFieldAliasesForVars varGroup span'
                in if Map.null scopedAliasOccs && allowGlobalAliasFallback
                    then spanRecordFieldAliasesForVars Set.empty span'
                    else scopedAliasOccs
            addressAliasOccs = lookupFieldAliasOccs fieldKeyAddress aliasOccsByField
            valueAliasOccs = lookupFieldAliasOccs fieldKeyValue aliasOccsByField
            datumAliasOccs = lookupFieldAliasOccs fieldKeyDatum aliasOccsByField
            referenceAliasOccs = lookupFieldAliasOccs fieldKeyReference aliasOccsByField
            addressCheckedByAliases =
                spanMentionsAnyFieldTokenForOccs addressTokens addressAliasOccs span'
                    || spanMentionsAddressEqForOccs addressAliasOccs span'
            addressEqImpliesStaking =
                spanMentionsAddressEqForVars varGroup span'
                    || spanMentionsAddressEqForOccs addressAliasOccs span'
        in TxOutFieldUsage
        { txOutFieldAddressChecked =
            spanMentionsAnyFieldTokenForVars addressTokens varGroup span'
                || addressCheckedByAliases
        , txOutFieldStakingChecked =
            spanMentionsAnyFieldTokenForVars stakingTokens varGroup span'
                || spanMentionsAnyFieldTokenForOccs stakingTokens addressAliasOccs span'
                || addressEqImpliesStaking
        , txOutFieldValueChecked =
            spanMentionsAnyFieldTokenForVars valueTokens varGroup span'
                || spanMentionsAnyFieldTokenForOccs valueTokens valueAliasOccs span'
        , txOutFieldDatumChecked =
            spanMentionsAnyFieldTokenForVars datumTokens varGroup span'
                || spanMentionsAnyFieldTokenForOccs datumTokens datumAliasOccs span'
        , txOutFieldReferenceChecked =
            spanMentionsAnyFieldTokenForVars referenceTokens varGroup span'
                || spanMentionsAnyFieldTokenForOccs referenceTokens referenceAliasOccs span'
        }

    collectTxOutVariableNames :: HieAST TypeIndex -> Set Name
    collectTxOutVariableNames = go
      where
        go n@Node{nodeChildren = children} =
            namesHere n <> foldMap go children

        namesHere n =
            Set.fromList
                [ name
                | (Right name, details) <- Map.assocs $ nodeIdentifiers $ nodeInfo n
                , not (isExternalName name)
                , identTypeRootTyConName details == Just "TxOut"
                ]

    identTypeRootTyConName :: IdentifierDetails TypeIndex -> Maybe String
    identTypeRootTyConName IdentifierDetails{identType = identType'} = do
        ix <- identType'
        typeIndexRootTyConName ix

    typeIndexRootTyConName :: TypeIndex -> Maybe String
    typeIndexRootTyConName ix = case hie_types hie Arr.! ix of
        HTyConApp IfaceTyCon{ifaceTyConName = tyConName} _ ->
            Just $ occNameString $ nameOccName tyConName
        _ -> Nothing

    txOutAliasGroups :: Set Name -> RealSrcSpan -> [Set Name]
    txOutAliasGroups vars span' =
        connectedComponents vars aliasEdges
      where
        occToName = Map.fromList
            [ (occNameString $ nameOccName name, name)
            | name <- Set.toList vars
            ]

        aliasEdges :: [(Name, Name)]
        aliasEdges = mapMaybe (aliasEdge occToName) (srcLinesInSpan span')

    aliasEdge :: Map String Name -> ByteString -> Maybe (Name, Name)
    aliasEdge occToName line = do
        (lhsOcc, rhsOcc) <- parseSimpleAliasLine line
        lhsName <- Map.lookup lhsOcc occToName
        rhsName <- Map.lookup rhsOcc occToName
        guard (lhsName /= rhsName)
        pure (lhsName, rhsName)

    parseSimpleAliasLine :: ByteString -> Maybe (String, String)
    parseSimpleAliasLine line = do
        let noComment = fst $ BS8.breakSubstring "--" line
            (lhsRaw, eqAndRhs) = BS8.break (== '=') noComment
        guard (not $ BS8.null eqAndRhs)
        guard (not $ BS8.isPrefixOf "==" eqAndRhs)
        lhsOcc <- lastWordOcc lhsRaw
        (rhsOcc, rhsRest) <- firstWordAndRest $ BS8.drop 1 eqAndRhs
        guard (BS8.all isSpace rhsRest)
        pure (lhsOcc, rhsOcc)

    fieldKeyAddress, fieldKeyValue, fieldKeyDatum, fieldKeyReference :: String
    fieldKeyAddress = "address"
    fieldKeyValue = "value"
    fieldKeyDatum = "datum"
    fieldKeyReference = "reference"

    lookupFieldAliasOccs :: String -> Map String (Set String) -> Set String
    lookupFieldAliasOccs fieldKey aliases =
        Map.findWithDefault Set.empty fieldKey aliases

    mergeFieldAliasMaps :: Map String (Set String) -> Map String (Set String) -> Map String (Set String)
    mergeFieldAliasMaps = Map.unionWith (<>)

    spanRecordFieldAliasesForVars :: Set Name -> RealSrcSpan -> Map String (Set String)
    spanRecordFieldAliasesForVars vars span' =
        let varOccs = Set.fromList $ map (occNameString . nameOccName) $ Set.toList vars
            (_, _, aliasesByField) =
                foldl' (collectRecordFieldAliases varOccs) (False, Nothing, Map.empty) (srcLinesInSpan span')
        in aliasesByField

    collectRecordFieldAliases
        :: Set String
        -> (Bool, Maybe (Map String (Set String), Bool), Map String (Set String))
        -> ByteString
        -> (Bool, Maybe (Map String (Set String), Bool), Map String (Set String))
    collectRecordFieldAliases varOccs (pendingTxOutIntro, activeCapture, aliasesByField) rawLine =
        let line = lineCodePart rawLine
            hasTxOutWord = containsWordBS "TxOut" line
            hasOpenBrace = BS8.elem '{' line
            lineAliases = parseRecordFieldAliasesFromLine line
            commitAliases rhsOcc aliasesToCommit =
                if Set.null varOccs || rhsOcc `Set.member` varOccs
                    then mergeFieldAliasMaps aliasesByField aliasesToCommit
                    else aliasesByField
        in case activeCapture of
            Nothing
                | hasOpenBrace && (hasTxOutWord || pendingTxOutIntro) ->
                    case parseRecordPatternBoundVar line of
                        Just rhsOcc ->
                            ( False
                            , Nothing
                            , commitAliases rhsOcc lineAliases
                            )
                        Nothing ->
                            (False, Just (lineAliases, BS8.elem '}' line), aliasesByField)
                | otherwise ->
                    (hasTxOutWord && not hasOpenBrace, Nothing, aliasesByField)
            Just (capturedAliases, waitingBoundVarAfterClose) ->
                let capturedAliases' = mergeFieldAliasMaps capturedAliases lineAliases
                    rhsOcc = if waitingBoundVarAfterClose
                        then parseRecordPatternBoundVarOnly line
                        else parseRecordPatternBoundVar line
                in case rhsOcc of
                    Just boundOcc ->
                        ( False
                        , Nothing
                        , commitAliases boundOcc capturedAliases'
                        )
                    Nothing ->
                        ( False
                        , Just (capturedAliases', waitingBoundVarAfterClose || BS8.elem '}' line)
                        , aliasesByField
                        )

    parseRecordFieldAliasesFromLine :: ByteString -> Map String (Set String)
    parseRecordFieldAliasesFromLine line =
        foldl'
            (\acc segment -> case parseRecordFieldAliasSegment segment of
                Nothing -> acc
                Just (fieldKey, aliasOcc) ->
                    Map.insertWith (<>) fieldKey (Set.singleton aliasOcc) acc
            )
            Map.empty
            (BS8.split ',' line)

    parseRecordFieldAliasSegment :: ByteString -> Maybe (String, String)
    parseRecordFieldAliasSegment segment = case parseRecordFieldAliasAssignment segment of
        Just parsedAlias -> Just parsedAlias
        Nothing -> parseRecordFieldAliasPun segment

    parseRecordFieldAliasAssignment :: ByteString -> Maybe (String, String)
    parseRecordFieldAliasAssignment segment = do
        let (lhsRaw, eqAndRhs) = BS8.break (== '=') segment
        guard (not $ BS8.null eqAndRhs)
        guard (not $ BS8.isPrefixOf "==" eqAndRhs)
        fieldOcc <- lastWordOcc lhsRaw
        fieldKey <- txOutFieldKeyFromOcc fieldOcc
        aliasOcc <- firstWordOccLoose $ BS8.drop 1 eqAndRhs
        pure (fieldKey, aliasOcc)

    parseRecordFieldAliasPun :: ByteString -> Maybe (String, String)
    parseRecordFieldAliasPun segment = do
        let (_lhsRaw, eqAndRhs) = BS8.break (== '=') segment
        guard (BS8.null eqAndRhs)
        fieldOcc <- lastWordOcc segment
        fieldKey <- txOutFieldKeyFromOcc fieldOcc
        pure (fieldKey, fieldOcc)

    txOutFieldKeyFromOcc :: String -> Maybe String
    txOutFieldKeyFromOcc = \case
        "txOutAddress" -> Just fieldKeyAddress
        "txOutValue" -> Just fieldKeyValue
        "txOutDatum" -> Just fieldKeyDatum
        "txOutReferenceScript" -> Just fieldKeyReference
        "referenceScript" -> Just fieldKeyReference
        _ -> Nothing

    parseRecordPatternBoundVar :: ByteString -> Maybe String
    parseRecordPatternBoundVar line = do
        let (_beforeClose, closeAndRest) = BS8.break (== '}') line
        guard (not $ BS8.null closeAndRest)
        parseRecordPatternBoundVarOnly $ BS8.drop 1 closeAndRest

    parseRecordPatternBoundVarOnly :: ByteString -> Maybe String
    parseRecordPatternBoundVarOnly line = do
        let (_beforeEq, eqAndRhs) = BS8.break (== '=') line
        guard (not $ BS8.null eqAndRhs)
        guard (not $ BS8.isPrefixOf "==" eqAndRhs)
        firstWordOccLoose $ BS8.drop 1 eqAndRhs
    connectedComponents :: Set Name -> [(Name, Name)] -> [Set Name]
    connectedComponents vars edges = go vars []
      where
        adjacency :: Map Name (Set Name)
        adjacency = Map.fromListWith (<>)
            [ (a, Set.singleton b)
            | (a, b) <- edges
            ]
            <> Map.fromListWith (<>)
            [ (b, Set.singleton a)
            | (a, b) <- edges
            ]

        go unvisited acc
            | Set.null unvisited = reverse acc
            | otherwise =
                let start = Set.findMin unvisited
                    component = dfs Set.empty [start]
                    remaining = unvisited Set.\\ component
                in go remaining (component : acc)

        dfs visited = \case
            [] -> visited
            x:xs
                | Set.member x visited -> dfs visited xs
                | otherwise ->
                    let neighbours = Set.toList $ Map.findWithDefault Set.empty x adjacency
                    in dfs (Set.insert x visited) (neighbours <> xs)

    subtreeHasAnyOccName :: [String] -> HieAST TypeIndex -> Bool
    subtreeHasAnyOccName targets = go
      where
        go n@Node{nodeChildren = children} =
            nodeHasAnyOccName targets n || any go children

    nodeHasAnyOccName :: [String] -> HieAST TypeIndex -> Bool
    nodeHasAnyOccName targets node =
        any hasTargetOccName (Map.keys $ nodeIdentifiers $ nodeInfo node)
      where
        hasTargetOccName = \case
            Left _ -> False
            Right name -> occNameString (nameOccName name) `elem` targets

    subtreeHasEqOperator :: HieAST TypeIndex -> Bool
    subtreeHasEqOperator node =
        subtreeHasAnyOccName ["=="] node
            || subtreeHasEqName node
            || spanMentionsEqOperator (nodeSpan node)

    subtreeHasAddressEquality :: HieAST TypeIndex -> Bool
    subtreeHasAddressEquality = go
      where
        go n@Node{nodeChildren = children} =
            nodeLooksLikeAddressEquality n || any go children

        nodeLooksLikeAddressEquality :: HieAST TypeIndex -> Bool
        nodeLooksLikeAddressEquality n =
            (isOneLineSpan (nodeSpan n)
                && nodeHasAnyTxOutFieldToken ["txOutAddress"] n
                && subtreeHasEqOperator n
            )
                || spanLooksLikeAddressEquality (nodeSpan n)

        spanLooksLikeAddressEquality :: RealSrcSpan -> Bool
        spanLooksLikeAddressEquality span' =
            any lineHasAddressEqualityPattern (srcNumberedLinesInSpan span')

        lineHasAddressEqualityPattern :: (Int, ByteString) -> Bool
        lineHasAddressEqualityPattern (lineNumber, line) =
            let prevLine = srcLineAt (lineNumber - 1)
                nextLine = srcLineAt (lineNumber + 1)
                lineIsAddressOperand = lineLooksLikeAddressOperandToken line
                adjacentEq =
                    maybe False lineHasEqOperator prevLine
                        || lineHasEqOperator line
                        || maybe False lineHasEqOperator nextLine
                adjacentAddressOperand =
                    maybe False lineLooksLikeAddressOperandToken prevLine
                        || lineIsAddressOperand
                        || maybe False lineLooksLikeAddressOperandToken nextLine
            in adjacentEq && adjacentAddressOperand

        lineLooksLikeAddressOperandToken :: ByteString -> Bool
        lineLooksLikeAddressOperandToken line =
            parseBindingLhs line == Nothing
                && containsWordBS "txOutAddress" (lineCodePart line)

    subtreeHasEqName :: HieAST TypeIndex -> Bool
    subtreeHasEqName n@Node{nodeChildren = children} =
        nodeHasEqName n || any subtreeHasEqName children

    nodeHasEqName :: HieAST TypeIndex -> Bool
    nodeHasEqName node =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo node
            eqNameMeta = ("==" `ghcPrimNameFrom` "GHC.Classes")
        in any (hieMatchNameMeta eqNameMeta) idents

    spanMentionsEqOperator :: RealSrcSpan -> Bool
    spanMentionsEqOperator span' =
        any lineHasEqOperator (srcLinesInSpan span')

    lineCodePart :: ByteString -> ByteString
    lineCodePart = fst . BS8.breakSubstring "--"

    lineHasEqOperator :: ByteString -> Bool
    lineHasEqOperator line =
        "==" `BS8.isInfixOf` lineCodePart line

    srcLinesInSpan :: RealSrcSpan -> [ByteString]
    srcLinesInSpan span' =
        map snd (srcNumberedLinesInSpan span')

    srcNumberedLinesInSpan :: RealSrcSpan -> [(Int, ByteString)]
    srcNumberedLinesInSpan span' =
        let start = srcSpanStartLine span'
            end = srcSpanEndLine span'
            allLines = BS8.lines $ hie_hs_src hie
        in mapMaybe
            (\lineNumber -> (lineNumber,) <$> (allLines !!? (lineNumber - 1)))
            [start .. end]


    srcLineAt :: Int -> Maybe ByteString
    srcLineAt lineNumber =
        (BS8.lines $ hie_hs_src hie) !!? (lineNumber - 1)

    subtreeHasAnyTxOutFieldToken :: [String] -> HieAST TypeIndex -> Bool
    subtreeHasAnyTxOutFieldToken targets = go
      where
        go n@Node{nodeChildren = children} =
            nodeHasAnyTxOutFieldToken targets n || any go children

    nodeHasAnyTxOutFieldToken :: [String] -> HieAST TypeIndex -> Bool
    nodeHasAnyTxOutFieldToken targets node =
        any hasTargetOccName (Map.assocs $ nodeIdentifiers $ nodeInfo node)
      where
        hasTargetOccName = \case
            (Left _, _details) -> False
            (Right name, details) ->
                occNameString (nameOccName name) `elem` targets
                    && (isExternalName name || identTypeContainsTyConName "TxOut" details)

    spanMentionsAnyFieldTokenForVars :: [String] -> Set Name -> RealSrcSpan -> Bool
    spanMentionsAnyFieldTokenForVars tokens vars span' =
        let varOccs = map (occNameString . nameOccName) $ Set.toList vars
            numberedLines = srcNumberedLinesInSpan span'
            lineByNumber = srcLineAt
        in any
            (\(lineNumber, line) ->
                let prevPrevLine = lineByNumber (lineNumber - 2)
                    prevLine = lineByNumber (lineNumber - 1)
                    nextLine = lineByNumber (lineNumber + 1)
                    nextNextLine = lineByNumber (lineNumber + 2)
                    mentionsField =
                        any
                            (\varOcc ->
                                lineMentionsFieldTokenForVarAdjacent
                                    tokens
                                    varOcc
                                    prevLine
                                    line
                                    nextLine
                                    || (maybe False isSkippableCodeLine nextLine
                                        && lineMentionsFieldTokenForVarAdjacent
                                            tokens
                                            varOcc
                                            prevLine
                                            line
                                            nextNextLine
                                       )
                                    || (maybe False isSkippableCodeLine prevLine
                                        && lineMentionsFieldTokenForVarAdjacent
                                            tokens
                                            varOcc
                                            prevPrevLine
                                            line
                                            nextLine
                                       )
                            )
                            varOccs
                in mentionsField
                    && not (isUnusedBindingLine numberedLines lineNumber line)
            )
            numberedLines
    spanMentionsAddressEqForVars :: Set Name -> RealSrcSpan -> Bool
    spanMentionsAddressEqForVars vars span' =
        let varOccs = map (occNameString . nameOccName) $ Set.toList vars
            numberedLines = srcNumberedLinesInSpan span'
            lineByNumber = srcLineAt
        in any
            (\(lineNumber, line) ->
                let prevLine = lineByNumber (lineNumber - 1)
                    nextLine = lineByNumber (lineNumber + 1)
                    mentionsEq =
                        any
                            (\varOcc ->
                                lineMentionsAddressEqForVarAdjacent
                                    varOcc
                                    prevLine
                                    line
                                    nextLine
                            )
                            varOccs
                in mentionsEq
                    && not (isUnusedBindingLine numberedLines lineNumber line)
            )
            numberedLines

    spanMentionsAnyFieldTokenForOccs :: [String] -> Set String -> RealSrcSpan -> Bool
    spanMentionsAnyFieldTokenForOccs tokens occs span'
        | Set.null occs = False
        | otherwise =
            let occNames = Set.toList occs
                numberedLines = srcNumberedLinesInSpan span'
                lineByNumber = srcLineAt
            in any
                (\(lineNumber, line) ->
                    let prevPrevLine = lineByNumber (lineNumber - 2)
                        prevLine = lineByNumber (lineNumber - 1)
                        nextLine = lineByNumber (lineNumber + 1)
                        nextNextLine = lineByNumber (lineNumber + 2)
                        mentionsField =
                            any
                                (\occ ->
                                    lineMentionsFieldTokenForVarAdjacent
                                        tokens
                                        occ
                                        prevLine
                                        line
                                        nextLine
                                        || (maybe False isSkippableCodeLine nextLine
                                            && lineMentionsFieldTokenForVarAdjacent
                                                tokens
                                                occ
                                                prevLine
                                                line
                                                nextNextLine
                                           )
                                        || (maybe False isSkippableCodeLine prevLine
                                            && lineMentionsFieldTokenForVarAdjacent
                                                tokens
                                                occ
                                                prevPrevLine
                                                line
                                                nextLine
                                           )
                                )
                                occNames
                    in mentionsField
                        && not (isUnusedBindingLine numberedLines lineNumber line)
                )
                numberedLines

    spanMentionsAddressEqForOccs :: Set String -> RealSrcSpan -> Bool
    spanMentionsAddressEqForOccs occs span'
        | Set.null occs = False
        | otherwise =
            let occNames = Set.toList occs
                numberedLines = srcNumberedLinesInSpan span'
                lineByNumber = srcLineAt
            in any
                (\(lineNumber, line) ->
                    let prevLine = lineByNumber (lineNumber - 1)
                        nextLine = lineByNumber (lineNumber + 1)
                        mentionsEq =
                            any
                                (\occ ->
                                    lineMentionsAddressEqForOccAdjacent
                                        occ
                                        prevLine
                                        line
                                        nextLine
                                )
                                occNames
                    in mentionsEq
                        && not (isUnusedBindingLine numberedLines lineNumber line)
                )
                numberedLines

    lineMentionsAddressEqForVarAdjacent :: String -> Maybe ByteString -> ByteString -> Maybe ByteString -> Bool
    lineMentionsAddressEqForVarAdjacent varOcc prevLine line nextLine =
        lineMentionsAddressEqForVar varOcc line
            || (lineHasEqOperator line
                && maybe False (lineLooksLikeAddressOperand varOcc) prevLine
               )
            || (lineHasEqOperator line
                && maybe False (lineLooksLikeAddressOperand varOcc) nextLine
               )
            || (lineLooksLikeAddressOperand varOcc line
                && maybe False lineHasEqOperator prevLine
               )
            || (lineLooksLikeAddressOperand varOcc line
                && maybe False lineHasEqOperator nextLine
               )

    lineMentionsAddressEqForVar :: String -> ByteString -> Bool
    lineMentionsAddressEqForVar varOcc line =
        lineHasEqOperator line
            && lineMentionsFieldTokenForVar ["txOutAddress"] varOcc line

    lineLooksLikeAddressOperand :: String -> ByteString -> Bool
    lineLooksLikeAddressOperand varOcc line =
        parseBindingLhs line == Nothing
            && lineMentionsFieldTokenForVar ["txOutAddress"] varOcc line

    lineMentionsAddressEqForOccAdjacent :: String -> Maybe ByteString -> ByteString -> Maybe ByteString -> Bool
    lineMentionsAddressEqForOccAdjacent occ prevLine line nextLine =
        lineMentionsAddressEqForOcc occ line
            || maybe False (lineMentionsAddressEqForOcc occ) prevLine
            || maybe False (lineMentionsAddressEqForOcc occ) nextLine
            || (lineHasEqOperator line
                && maybe False (lineLooksLikeAddressAliasOperand occ) prevLine
               )
            || (lineHasEqOperator line
                && maybe False (lineLooksLikeAddressAliasOperand occ) nextLine
               )
            || (lineLooksLikeAddressAliasOperand occ line
                && maybe False lineHasEqOperator prevLine
               )
            || (lineLooksLikeAddressAliasOperand occ line
                && maybe False lineHasEqOperator nextLine
               )
    lineMentionsAddressEqForOcc :: String -> ByteString -> Bool
    lineMentionsAddressEqForOcc occ line =
        lineHasEqOperator line
            && lineMentionsVarOcc occ line

    lineLooksLikeAddressAliasOperand :: String -> ByteString -> Bool
    lineLooksLikeAddressAliasOperand occ line =
        parseBindingLhs line == Nothing
            && lineMentionsVarOcc occ line

    lineMentionsFieldTokenForVarAdjacent :: [String] -> String -> Maybe ByteString -> ByteString -> Maybe ByteString -> Bool
    lineMentionsFieldTokenForVarAdjacent tokens varOcc prevLine line nextLine =
        lineMentionsFieldTokenForVar tokens varOcc line
            || (lineHasAnyFieldToken tokens line
                && parseBindingLhs line == Nothing
                && maybe False (lineMentionsVarOcc varOcc) nextLine
               )
            || (lineMentionsVarOcc varOcc line
                && maybe False
                    (\prev -> parseBindingLhs prev == Nothing && lineHasAnyFieldToken tokens prev)
                    prevLine
               )
            || (lineLooksLikeCaseScrutinee varOcc line
                && maybe False
                    (\next -> parseBindingLhs next == Nothing && lineHasAnyFieldToken tokens next)
                    nextLine
               )

    lineMentionsVarOcc :: String -> ByteString -> Bool
    lineMentionsVarOcc varOcc line =
        containsWordBS (BS8.pack varOcc) (lineCodePart line)

    lineLooksLikeCaseScrutinee :: String -> ByteString -> Bool
    lineLooksLikeCaseScrutinee varOcc line =
        let code = lineCodePart line
        in parseBindingLhs line == Nothing
            && containsWordBS "case" code
            && containsWordBS "of" code
            && containsWordBS (BS8.pack varOcc) code
    isSkippableCodeLine :: ByteString -> Bool
    isSkippableCodeLine line =
        BS8.null $ BS8.dropWhile isSpace $ lineCodePart line

    lineMentionsFieldTokenForVar :: [String] -> String -> ByteString -> Bool
    lineMentionsFieldTokenForVar tokens varOcc line =
        lineMentionsVarOcc varOcc line
            && lineHasAnyFieldToken tokens line

    lineHasAnyFieldToken :: [String] -> ByteString -> Bool
    lineHasAnyFieldToken tokens line =
        let code = lineCodePart line
        in any (\tok -> containsWordBS (BS8.pack tok) code) tokens

    isUnusedBindingLine :: [(Int, ByteString)] -> Int -> ByteString -> Bool
    isUnusedBindingLine numberedLines lineNumber line = case parseBindingLhs line of
        Nothing -> False
        Just lhsOcc ->
            not $ any
                (\(n, candidateLine) ->
                    n > lineNumber && containsWordBS (BS8.pack lhsOcc) candidateLine
                )
                numberedLines

    parseBindingLhs :: ByteString -> Maybe String
    parseBindingLhs line = do
        let noComment = fst $ BS8.breakSubstring "--" line
            (lhsRaw, eqAndRhs) = BS8.break (== '=') noComment
            lhsTrimmed = trimRight lhsRaw
            beforeEq = if BS8.null lhsTrimmed then Nothing else Just (BS8.last lhsTrimmed)
        guard (not $ BS8.null eqAndRhs)
        guard (not $ BS8.isPrefixOf "==" eqAndRhs)
        guard $ case beforeEq of
            Just c -> c `notElem` ['<', '>', '!', '/', ':']
            Nothing -> False
        lastWordOcc lhsRaw

    firstWordAndRest :: ByteString -> Maybe (String, ByteString)
    firstWordAndRest bs = do
        let trimmed = BS8.dropWhile isSpace bs
            (word, rest) = BS8.span isIdentifierChar trimmed
        guard (not $ BS8.null word)
        pure (BS8.unpack word, BS8.dropWhile isSpace rest)


    firstWordOccLoose :: ByteString -> Maybe String
    firstWordOccLoose bs = do
        let trimmed = BS8.dropWhile (\c -> isSpace c || c == '{' || c == '}') bs
            word = BS8.takeWhile isIdentifierChar trimmed
        guard (not $ BS8.null word)
        pure (BS8.unpack word)
    lastWordOcc :: ByteString -> Maybe String
    lastWordOcc bs = do
        let trimmed = trimRight bs
            revWord = BS8.takeWhile isIdentifierChar (BS8.reverse trimmed)
            word = BS8.reverse revWord
        guard (not $ BS8.null word)
        pure (BS8.unpack word)

    trimRight :: ByteString -> ByteString
    trimRight = BS8.reverse . BS8.dropWhile isSpace . BS8.reverse

    isIdentifierChar :: Char -> Bool
    isIdentifierChar c = isAlphaNum c || c == '_' || c == '\''

    containsWordBS :: ByteString -> ByteString -> Bool
    containsWordBS needle haystack
        | BS8.null needle = False
        | otherwise = go haystack
      where
        go bs = case BS8.breakSubstring needle bs of
            (_before, after)
                | BS8.null after -> False
                | otherwise ->
                    let idx = BS8.length bs - BS8.length after
                        beforeCh = bs BS8.!? (idx - 1)
                        afterCh = bs BS8.!? (idx + BS8.length needle)
                        beforeOk = maybe True (not . isIdentifierChar) beforeCh
                        afterOk = maybe True (not . isIdentifierChar) afterCh
                    in if beforeOk && afterOk
                        then True
                        else go (BS8.drop 1 after)

    identTypeContainsTyConName :: String -> IdentifierDetails TypeIndex -> Bool
    identTypeContainsTyConName needle IdentifierDetails{identType = identType'} = case identType' of
        Nothing -> False
        Just ix -> typeIndexContainsTyConName needle ix

    checkedFieldsCount :: TxOutFieldUsage -> Int
    checkedFieldsCount TxOutFieldUsage{..} =
        length $ filter id
            [ txOutFieldAddressChecked
            , txOutFieldStakingChecked
            , txOutFieldValueChecked
            , txOutFieldDatumChecked
            , txOutFieldReferenceChecked
            ]

    isMissingField :: MissingTxOutField -> TxOutFieldUsage -> Bool
    isMissingField missing = \case
        TxOutFieldUsage{txOutFieldReferenceChecked = False} | missing == MissingReferenceScript -> True
        TxOutFieldUsage{txOutFieldStakingChecked = False} | missing == MissingStakingCredential -> True
        TxOutFieldUsage{txOutFieldValueChecked = False} | missing == MissingValue -> True
        TxOutFieldUsage{txOutFieldDatumChecked = False} | missing == MissingDatum -> True
        _ -> False

    hasTxOutEvidence :: HieAST TypeIndex -> Bool
    hasTxOutEvidence node =
        nodeTypeContainsTyConName "TxOut" node
            || subtreeHasAnyTxOutFieldToken txOutEvidenceTokens node

    nodeTypeContainsTyConName :: String -> HieAST TypeIndex -> Bool
    nodeTypeContainsTyConName needle node =
        any (typeIndexContainsTyConName needle) (nodeTypeIndices node)

    typeIndexContainsTyConName :: String -> TypeIndex -> Bool
    typeIndexContainsTyConName needle ix =
        hieTypeContainsTyConName needle (hie_types hie Arr.! ix)

    hieTypeContainsTyConName :: String -> HieTypeFlat -> Bool
    hieTypeContainsTyConName needle ty = case ty of
        HTyConApp IfaceTyCon{ifaceTyConName = tyConName} (HieArgs args) ->
            let here = occNameString (nameOccName tyConName) == needle
            in here || any (hieTypeContainsTyConName needle . (hie_types hie Arr.!) . snd) args
        HFunTy _ a b ->
            hieTypeContainsTyConName needle (hie_types hie Arr.! a)
                || hieTypeContainsTyConName needle (hie_types hie Arr.! b)
        HForAllTy _ inner ->
            hieTypeContainsTyConName needle (hie_types hie Arr.! inner)
        HQualTy _ inner ->
            hieTypeContainsTyConName needle (hie_types hie Arr.! inner)
        _ -> False

    nodeTypeIndices :: HieAST TypeIndex -> [TypeIndex]
    nodeTypeIndices node =
        let NodeInfo{nodeType = nodeTypes, nodeIdentifiers = idents} = nodeInfo node
            identTypes =
                [ ty
                | (_ident, IdentifierDetails{identType = Just ty}) <- Map.assocs idents
                ]
        in nodeTypes <> identTypes

    addressTokens, stakingTokens, valueTokens, datumTokens, referenceTokens :: [String]
    addressTokens =
        [ "txOutAddress"
        , "hasOutputAddress"
        , "getOutputPkh"
        , "Address"
        , "ScriptCredential"
        , "PubKeyCredential"
        ]

    stakingTokens =
        [ "hasStakingCredential"
        , "addressStakingCredential"
        , "StakingCredential"
        , "stakingCredential"
        , "StakingHash"
        , "StakingPtr"
        ]

    valueTokens =
        [ "txOutValue"
        , "hasOutputValue"
        , "valueOf"
        , "currencySymbolValueOf"
        ]

    datumTokens =
        [ "txOutDatum"
        , "hasOutputDatum"
        , "OutputDatum"
        , "getDatumData"
        ]

    referenceTokens =
        [ "txOutReferenceScript"
        , "hasReferenceScript"
        , "referenceScript"
        , "ReferenceScript"
        ]

    allFieldTokens :: [String]
    allFieldTokens =
        addressTokens <> stakingTokens <> valueTokens <> datumTokens <> referenceTokens

    txOutEvidenceTokens :: [String]
    txOutEvidenceTokens =
        allFieldTokens
            <> [ "TxOut"
               , "Address"
               , "OutputDatum"
               ]

data BurningCheckKey
    = BurningKeyDirectValueOf !RealSrcSpan !ValueOfTokenKey
    | BurningKeyValueBinding !RealSrcSpan !Name !(Maybe (Set ValueOfArgKey))
    | BurningKeyFlattenAmount !RealSrcSpan !Name
    deriving stock (Eq, Ord)

data ValueOfArgKey
    = ValueOfArgName !Name
    | ValueOfArgSource !ByteString
    deriving stock (Eq, Ord)

data ValueOfTokenKey = ValueOfTokenKey !(Set ValueOfArgKey) !(Set ValueOfArgKey)
    deriving stock (Eq, Ord)

data BurningSign
    = BurningPositive
    | BurningNegative
    deriving stock (Eq)

data BindingRef = BindingRef
    { bindingRefOcc :: !ByteString
    , bindingRefLine :: !Int
    }

analyseMissingBurningLogic
    :: Id Inspection
    -> HieFile
    -> HieAST TypeIndex
    -> State VisitorState ()
analyseMissingBurningLogic insId hie = go
  where
    go :: HieAST TypeIndex -> State VisitorState ()
    go curNode =
        addObservations $ mkObservation insId hie <$> matchNode curNode

    warningSpanSet :: Set RealSrcSpan
    warningSpanSet =
        Set.fromList $ concatMap collectWarningSpans functionNodes

    functionNodes :: [HieAST TypeIndex]
    functionNodes =
        concatMap collectFunctionNodes (Map.elems $ getAsts $ hie_asts hie)

    matchNode :: HieAST TypeIndex -> Slist RealSrcSpan
    matchNode node = case comparisonNode node of
        Just _ | nodeSpan node `Set.member` warningSpanSet ->
            S.one $ nodeSpan node
        _ -> mempty

    collectFunctionNodes :: HieAST TypeIndex -> [HieAST TypeIndex]
    collectFunctionNodes n@Node{nodeChildren = children}
        | hieMatchPatternAst hie n fun = [n]
        | otherwise = concatMap collectFunctionNodes children

    collectWarningSpans :: HieAST TypeIndex -> [RealSrcSpan]
    collectWarningSpans funNode =
        let hasExternalMint = subtreeHasAnyNameMeta txInfoMintPrimitiveNameMetas funNode
        in if not hasExternalMint
            then []
            else
                let span' = nodeSpan funNode
                    bindingNameSpans = collectBindingNameSpans funNode
                    bindingBooleanContexts = collectBindingBooleanContexts funNode
                    (mintCarrierRefs, mintedValueRefs) = collectMintBindingRefs funNode bindingNameSpans span'
                    mintCarrierNames = resolveBindingRefs bindingNameSpans mintCarrierRefs
                    mintedBindingNames = resolveBindingRefs bindingNameSpans mintedValueRefs
                    flattenAmountNames = collectFlattenAmountBindingNames bindingNameSpans funNode span' mintCarrierNames
                    checks = concatMap
                        (comparisonSignedKeys
                            bindingNameSpans
                            bindingBooleanContexts
                            mintCarrierNames
                            mintedBindingNames
                            flattenAmountNames
                        )
                        (collectComparisonNodes funNode)
                    groupedChecks = foldl' insertCheck Map.empty checks
                in concat
                    [ positiveSpans
                    | (_key, (positiveSpans, hasNegative)) <- Map.assocs groupedChecks
                    , not hasNegative
                    ]
      where
        insertCheck
            :: Map BurningCheckKey ([RealSrcSpan], Bool)
            -> (BurningCheckKey, BurningSign, RealSrcSpan)
            -> Map BurningCheckKey ([RealSrcSpan], Bool)
        insertCheck acc (key, sign, span') =
            let current = case sign of
                    BurningPositive -> ([span'], False)
                    BurningNegative -> ([], True)
            in Map.insertWith
                (\(newSpans, newNeg) (oldSpans, oldNeg) ->
                    (newSpans <> oldSpans, newNeg || oldNeg))
                key
                current
                acc

    collectComparisonNodes
        :: HieAST TypeIndex
        -> [(Maybe RealSrcSpan, RealSrcSpan, ByteString, HieAST TypeIndex, HieAST TypeIndex)]
    collectComparisonNodes = go Nothing
      where
        go boolContext n@Node{nodeChildren = children} =
            let here = case comparisonNode n of
                    Just (op, lhs, rhsNode) -> [(boolContext, nodeSpan n, op, lhs, rhsNode)]
                    Nothing -> []
                childContext
                    | isConditionalBranchNode n = boolContext <|> Just (nodeSpan n)
                    | otherwise = boolContext <|> booleanContextSpan n
            in here <> concatMap (go childContext) children

        booleanContextSpan :: HieAST TypeIndex -> Maybe RealSrcSpan
        booleanContextSpan n = nodeSpan n <$ booleanContextOperator n

        booleanContextOperator :: HieAST TypeIndex -> Maybe ByteString
        booleanContextOperator n =
            opBooleanOperator n <|> appBooleanOperator n
          where
            opBooleanOperator node = do
                guard $ nodeHasAnnotation opAppAnnotation node
                _lhs:opNode:_rhs:_ <- Just $ nodeChildren node
                find (`elem` supportedBooleanOperators) (nodeOccNames opNode)

            appBooleanOperator node = do
                guard $ nodeHasAnnotation hsAppAnnotation node
                let (headNode, args) = appSpine node
                guard (length args >= 2)
                find (`elem` supportedBooleanOperators) (nodeOccNames headNode)

            supportedBooleanOperators :: [ByteString]
            supportedBooleanOperators = ["||", "&&"]

        isConditionalBranchNode :: HieAST TypeIndex -> Bool
        isConditionalBranchNode n =
            nodeHasAnnotation hsIfAnnotation n
                || nodeHasAnnotation hsCaseAnnotation n
                || isIfThenElseCall n

        isIfThenElseCall :: HieAST TypeIndex -> Bool
        isIfThenElseCall n
            | not (nodeHasAnnotation hsAppAnnotation n) = False
            | otherwise =
                let (headNode, args) = appSpine n
                in length args >= 3
                    && ("ifThenElse" `elem` nodeOccNames headNode
                        || subtreeHasOccName "ifThenElse" headNode)

        hsIfAnnotation :: NodeAnnotation
        hsIfAnnotation = mkNodeAnnotation "HsIf" "HsExpr"

        hsCaseAnnotation :: NodeAnnotation
        hsCaseAnnotation = mkNodeAnnotation "HsCase" "HsExpr"

    comparisonSignedKeys
        :: [(Name, RealSrcSpan)]
        -> Map Name (Set RealSrcSpan)
        -> Set Name
        -> Set Name
        -> Set Name
        -> (Maybe RealSrcSpan, RealSrcSpan, ByteString, HieAST TypeIndex, HieAST TypeIndex)
        -> [(BurningCheckKey, BurningSign, RealSrcSpan)]
    comparisonSignedKeys
        bindingNameSpans
        bindingBooleanContexts
        mintCarrierNames
        mintedBindingNames
        flattenAmountNames
        (boolContext, cmpSpan, op, lhs, rhsNode) =
        let bindingContexts = fromMaybe Set.empty $ do
                bindingName <- comparisonBindingName bindingNameSpans cmpSpan
                contexts <- Map.lookup bindingName bindingBooleanContexts
                guard (not $ Set.null contexts)
                pure contexts
            guardedDispatchContext = guardDispatchContext bindingNameSpans cmpSpan
            boolContexts = maybe Set.empty Set.singleton boolContext
            inheritedContexts =
                bindingContexts
                    <> maybe Set.empty Set.singleton guardedDispatchContext
            contextCandidates =
                if Set.null bindingContexts
                    then boolContexts <> inheritedContexts
                    else inheritedContexts
            directContexts =
                if Set.null contextCandidates
                    then Set.singleton cmpSpan
                    else contextCandidates
            lhsKeys = Set.toList $ Set.unions
                [ Set.fromList $ operandKeys mintCarrierNames mintedBindingNames flattenAmountNames directContext lhs
                | directContext <- Set.toList directContexts
                ]
            rhsKeys = Set.toList $ Set.unions
                [ Set.fromList $ operandKeys mintCarrierNames mintedBindingNames flattenAmountNames directContext rhsNode
                | directContext <- Set.toList directContexts
                ]
            lhsLiteral = operandIntegerLiteral lhs
            rhsLiteral = operandIntegerLiteral rhsNode
            eqSide keys = \case
                Just n | n > 0 -> map (\key -> (key, BurningPositive, cmpSpan)) keys
                Just n | n < 0 -> map (\key -> (key, BurningNegative, cmpSpan)) keys
                _ -> []
            lhsIneq keys predicate sign =
                [ (key, sign, cmpSpan)
                | key <- keys
                , Just n <- [rhsLiteral]
                , predicate n
                ]
            rhsIneq keys predicate sign =
                [ (key, sign, cmpSpan)
                | key <- keys
                , Just n <- [lhsLiteral]
                , predicate n
                ]
        in case op of
            "==" ->
                eqSide lhsKeys rhsLiteral
                    <> eqSide rhsKeys lhsLiteral
            ">" ->
                lhsIneq lhsKeys (>= 0) BurningPositive
                    <> rhsIneq rhsKeys (<= 0) BurningNegative
            ">=" ->
                lhsIneq lhsKeys (>= 0) BurningPositive
                    <> rhsIneq rhsKeys (<= 0) BurningNegative
            "<" ->
                lhsIneq lhsKeys (<= 0) BurningNegative
                    <> rhsIneq rhsKeys (>= 0) BurningPositive
            "<=" ->
                lhsIneq lhsKeys (<= 0) BurningNegative
                    <> rhsIneq rhsKeys (>= 0) BurningPositive
            _ -> []

    collectBindingBooleanContexts :: HieAST TypeIndex -> Map Name (Set RealSrcSpan)
    collectBindingBooleanContexts = go Map.empty
      where
        go :: Map Name (Set RealSrcSpan) -> HieAST TypeIndex -> Map Name (Set RealSrcSpan)
        go acc n@Node{nodeChildren = children}
            | nodeHasAnnotation hsLetAnnotation n = case children of
                binds:body:_ ->
                    let letBindingNameSpans = collectBindingNameSpans binds
                        letBindingNames = Set.fromList $ map fst letBindingNameSpans
                        directActiveBindingNames = operandUsedNames body `Set.intersection` letBindingNames
                        letBindingDependencies =
                            collectLetBindingDependencies letBindingNameSpans letBindingNames binds
                        activeBindingNames =
                            expandActiveBindingNames letBindingDependencies directActiveBindingNames
                        acc' = goActiveLetBinds letBindingNameSpans activeBindingNames acc binds
                    in go acc' body
                _ ->
                    foldl' go acc children
            | otherwise =
                let acc' =
                        foldl'
                            (\m (contextSpan, usedNames) ->
                                insertBooleanContext contextSpan usedNames m
                            )
                            acc
                            (contextSpanAndUsedNames n)
                in foldl' go acc' children

        goActiveLetBinds
            :: [(Name, RealSrcSpan)]
            -> Set Name
            -> Map Name (Set RealSrcSpan)
            -> HieAST TypeIndex
            -> Map Name (Set RealSrcSpan)
        goActiveLetBinds letBindingNameSpans activeBindingNames acc n@Node{nodeChildren = children}
            | nodeHasAnnotation hsLetAnnotation n =
                go acc n
            | otherwise =
                let acc' =
                        foldl'
                            (insertActiveContext letBindingNameSpans activeBindingNames)
                            acc
                            (contextSpanAndUsedNames n)
                in foldl'
                    (goActiveLetBinds letBindingNameSpans activeBindingNames)
                    acc'
                    children

        insertActiveContext
            :: [(Name, RealSrcSpan)]
            -> Set Name
            -> Map Name (Set RealSrcSpan)
            -> (RealSrcSpan, Set Name)
            -> Map Name (Set RealSrcSpan)
        insertActiveContext letBindingNameSpans activeBindingNames acc (contextSpan, usedNames) =
            let contextOwner = fst <$> pickMostSpecificSpan
                    [ (name, bindSpan)
                    | (name, bindSpan) <- letBindingNameSpans
                    , spanContainsSpan bindSpan contextSpan
                    ]
            in case contextOwner of
                Just owner | owner `Set.member` activeBindingNames ->
                    insertBooleanContext contextSpan usedNames acc
                _ -> acc

        collectLetBindingDependencies
            :: [(Name, RealSrcSpan)]
            -> Set Name
            -> HieAST TypeIndex
            -> Map Name (Set Name)
        collectLetBindingDependencies letBindingNameSpans letBindingNames =
            goDependencies (Map.fromSet (const Set.empty) letBindingNames)
          where
            goDependencies
                :: Map Name (Set Name)
                -> HieAST TypeIndex
                -> Map Name (Set Name)
            goDependencies acc n@Node{nodeChildren = children} =
                let owner = fst <$> pickMostSpecificSpan
                        [ (name, bindSpan)
                        | (name, bindSpan) <- letBindingNameSpans
                        , spanContainsSpan bindSpan (nodeSpan n)
                        ]
                    usedLetBindingNames =
                        nodeUsedNames n `Set.intersection` letBindingNames
                    acc' = case owner of
                        Just ownerName ->
                            Map.insertWith Set.union ownerName usedLetBindingNames acc
                        Nothing ->
                            acc
                in foldl' goDependencies acc' children

        expandActiveBindingNames
            :: Map Name (Set Name)
            -> Set Name
            -> Set Name
        expandActiveBindingNames letBindingDependencies = goExpand
          where
            goExpand activeNames =
                let expandedNames =
                        activeNames
                            <> Set.unions
                                [ Map.findWithDefault Set.empty name letBindingDependencies
                                | name <- Set.toList activeNames
                                ]
                in if expandedNames == activeNames
                    then activeNames
                    else goExpand expandedNames

        insertBooleanContext
            :: RealSrcSpan
            -> Set Name
            -> Map Name (Set RealSrcSpan)
            -> Map Name (Set RealSrcSpan)
        insertBooleanContext contextSpan usedNames acc =
            Set.foldl'
                (\m name ->
                    Map.insertWith
                        Set.union
                        name
                        (Set.singleton contextSpan)
                        m
                )
                acc
                usedNames

        contextSpanAndUsedNames
            :: HieAST TypeIndex
            -> [(RealSrcSpan, Set Name)]
        contextSpanAndUsedNames n =
            booleanContextData <> conditionalContextData
          where
            contextSpan = nodeSpan n
            booleanContextData = case booleanContextOperator n of
                Just _ ->
                    let usedNames = operandUsedNamesExcludingConditionals n
                    in [(contextSpan, usedNames) | not (Set.null usedNames)]
                Nothing -> []
            conditionalContextData
                | isConditionalBranchNode n =
                    let usedNames = conditionalBranchUsedNames n
                    in [(contextSpan, usedNames) | not (Set.null usedNames)]
                | otherwise = []

        conditionalBranchUsedNames :: HieAST TypeIndex -> Set Name
        conditionalBranchUsedNames n@Node{nodeChildren = children}
            | nodeHasAnnotation hsIfAnnotation n =
                usedNamesFromMaybeNodes [children !!? 1, children !!? 2]
            | nodeHasAnnotation hsCaseAnnotation n =
                Set.unions $ map operandUsedNamesExcludingConditionals $ drop 1 children
            | isIfThenElseCall n =
                let (_headNode, args) = appSpine n
                in usedNamesFromMaybeNodes [args !!? 1, args !!? 2]
            | otherwise = Set.empty
          where
            usedNamesFromMaybeNodes maybeNodes =
                Set.unions
                    [ operandUsedNamesExcludingConditionals branchNode
                    | Just branchNode <- maybeNodes
                    ]

        booleanContextOperator :: HieAST TypeIndex -> Maybe ByteString
        booleanContextOperator n =
            opBooleanOperator n <|> appBooleanOperator n
          where
            opBooleanOperator node = do
                guard $ nodeHasAnnotation opAppAnnotation node
                _lhs:opNode:_rhs:_ <- Just $ nodeChildren node
                find (`elem` supportedBooleanOperators) (nodeOccNames opNode)

            appBooleanOperator node = do
                guard $ nodeHasAnnotation hsAppAnnotation node
                let (headNode, args) = appSpine node
                guard (length args >= 2)
                find (`elem` supportedBooleanOperators) (nodeOccNames headNode)

            supportedBooleanOperators :: [ByteString]
            supportedBooleanOperators = ["||", "&&"]

        nodeUsedNames :: HieAST TypeIndex -> Set Name
        nodeUsedNames n =
            Set.fromList
                [ name
                | (Right name, IdentifierDetails{identInfo = identInfo'}) <- Map.assocs $ nodeIdentifiers $ nodeInfo n
                , Set.member Use identInfo'
                ]

        operandUsedNamesExcludingConditionals :: HieAST TypeIndex -> Set Name
        operandUsedNamesExcludingConditionals = goUsed Set.empty
          where
            goUsed :: Set Name -> HieAST TypeIndex -> Set Name
            goUsed acc n@Node{nodeChildren = children}
                | isConditionalBranchNode n = acc
                | otherwise =
                    let info = nodeInfo n
                        occsHere = Set.fromList
                            [ name
                            | (Right name, IdentifierDetails{identInfo = identInfo'}) <- Map.assocs $ nodeIdentifiers info
                            , Set.member Use identInfo'
                            ]
                    in foldl' goUsed (acc <> occsHere) children

        isConditionalBranchNode :: HieAST TypeIndex -> Bool
        isConditionalBranchNode n =
            nodeHasAnnotation hsIfAnnotation n
                || nodeHasAnnotation hsCaseAnnotation n
                || isIfThenElseCall n

        isIfThenElseCall :: HieAST TypeIndex -> Bool
        isIfThenElseCall n
            | not (nodeHasAnnotation hsAppAnnotation n) = False
            | otherwise =
                let (headNode, args) = appSpine n
                in length args >= 3
                    && ("ifThenElse" `elem` nodeOccNames headNode
                        || subtreeHasOccName "ifThenElse" headNode)

        hsIfAnnotation :: NodeAnnotation
        hsIfAnnotation = mkNodeAnnotation "HsIf" "HsExpr"

        hsCaseAnnotation :: NodeAnnotation
        hsCaseAnnotation = mkNodeAnnotation "HsCase" "HsExpr"

        hsLetAnnotation :: NodeAnnotation
        hsLetAnnotation = mkNodeAnnotation "HsLet" "HsExpr"

    comparisonBindingName :: [(Name, RealSrcSpan)] -> RealSrcSpan -> Maybe Name
    comparisonBindingName bindingNameSpans cmpSpan =
        fst <$> (directMatch <|> positionalFallback)
      where
        directMatch = pickMostSpecificSpan
            [ (name, bindSpan)
            | (name, bindSpan) <- bindingNameSpans
            , spanContainsSpan bindSpan cmpSpan
            ]

        positionalFallback =
            listToMaybe $
                sortBy compareByStartDesc
                    [ (name, bindSpan)
                    | (name, bindSpan) <- bindingNameSpans
                    , bindingStartsBeforeComparison bindSpan cmpSpan
                    ]

        compareByStartDesc (_nameA, spanA) (_nameB, spanB) =
            compare
                (srcSpanStartLine spanB, srcSpanStartCol spanB)
                (srcSpanStartLine spanA, srcSpanStartCol spanA)

    bindingStartsBeforeComparison :: RealSrcSpan -> RealSrcSpan -> Bool
    bindingStartsBeforeComparison bindSpan cmpSpan =
        let bindStartLine = srcSpanStartLine bindSpan
            bindStartCol = srcSpanStartCol bindSpan
            cmpStartLine = srcSpanStartLine cmpSpan
            cmpStartCol = srcSpanStartCol cmpSpan
        in bindStartLine < cmpStartLine
            || (bindStartLine == cmpStartLine && bindStartCol <= cmpStartCol)

    spanContainsSpan :: RealSrcSpan -> RealSrcSpan -> Bool
    spanContainsSpan outer inner =
        startsWithin && endsWithin
      where
        startsWithin =
            srcSpanStartLine inner > srcSpanStartLine outer
                || (srcSpanStartLine inner == srcSpanStartLine outer
                    && srcSpanStartCol inner >= srcSpanStartCol outer)
        endsWithin =
            srcSpanEndLine inner < srcSpanEndLine outer
                || (srcSpanEndLine inner == srcSpanEndLine outer
                    && srcSpanEndCol inner <= srcSpanEndCol outer)

    guardDispatchContext :: [(Name, RealSrcSpan)] -> RealSrcSpan -> Maybe RealSrcSpan
    guardDispatchContext bindingNameSpans cmpSpan = do
        (_, bindSpan) <- pickMostSpecificSpan
            [ (name, span')
            | (name, span') <- bindingNameSpans
            , spanContainsSpan span' cmpSpan
            ]
        guard (comparisonFollowsGuardLine bindSpan cmpSpan)
        pure bindSpan

    comparisonFollowsGuardLine :: RealSrcSpan -> RealSrcSpan -> Bool
    comparisonFollowsGuardLine bindSpan span' =
        let allLines = BS8.lines $ hie_hs_src hie
            bindingStartLine = srcSpanStartLine bindSpan
            startLine = srcSpanStartLine span'
            startsGuardPredicate lineText =
                BS8.isPrefixOf "|" lineText
                    && not (BS8.isPrefixOf "||" lineText)
            normalizedCodeLine lineText =
                trimLeft $ lineCodePart lineText
            isStandaloneBlockComment strippedLine =
                BS8.isPrefixOf "{-" strippedLine
                    && BS8.isSuffixOf "-}" strippedLine
            isSkippableLookbackLine strippedLine =
                BS8.null strippedLine || isStandaloneBlockComment strippedLine
            currentLineIsGuard =
                case allLines !!? (startLine - 1) of
                    Just lineText -> startsGuardPredicate (normalizedCodeLine lineText)
                    Nothing -> False
            lookbackLine = listToMaybe
                [ strippedLine
                | lineNo <- reverse [bindingStartLine .. startLine - 1]
                , Just lineText <- [allLines !!? (lineNo - 1)]
                , let strippedLine = normalizedCodeLine lineText
                , not (isSkippableLookbackLine strippedLine)
                ]
        in currentLineIsGuard || maybe False startsGuardPredicate lookbackLine

    comparisonNode
        :: HieAST TypeIndex
        -> Maybe (ByteString, HieAST TypeIndex, HieAST TypeIndex)
    comparisonNode node =
        opComparison node <|> appComparison node
      where
        opComparison n = do
            guard $ nodeHasAnnotation opAppAnnotation n
            lhs:opNode:rhsNode:_ <- Just $ nodeChildren n
            op <- nodeComparisonOperator opNode
            pure (op, lhs, rhsNode)

        appComparison n = do
            guard $ nodeHasAnnotation hsAppAnnotation n
            let (headNode, args) = appSpine n
            op <- nodeComparisonOperator headNode
            lhs <- args !!? 0
            rhsNode <- args !!? 1
            pure (op, lhs, rhsNode)

    nodeComparisonOperator :: HieAST TypeIndex -> Maybe ByteString
    nodeComparisonOperator node =
        find (`elem` supportedComparisonOperators) (nodeOccNames node)
      where
        supportedComparisonOperators :: [ByteString]
        supportedComparisonOperators = ["==", ">", "<", ">=", "<="]

    operandKeys
        :: Set Name
        -> Set Name
        -> Set Name
        -> RealSrcSpan
        -> HieAST TypeIndex
        -> [BurningCheckKey]
    operandKeys mintCarrierNames mintedBindingNames flattenAmountNames directContext operand =
        Set.toList $
            directKeys
                <> bindingKeys
                <> flattenKeys
      where
        operandNames = operandUsedNames operand

        directKeys :: Set BurningCheckKey
        directKeys =
            Set.map
                (BurningKeyDirectValueOf directContext)
                (operandDirectValueOfMintKeys mintCarrierNames operand)

        bindingKeys :: Set BurningCheckKey
        bindingKeys =
            Set.fromList
                [ BurningKeyValueBinding directContext name (bindingCallTokenArgKey name operand)
                | name <- Set.toList (operandNames `Set.intersection` mintedBindingNames)
                ]

        flattenKeys :: Set BurningCheckKey
        flattenKeys =
            Set.map (BurningKeyFlattenAmount directContext)
                (operandNames `Set.intersection` flattenAmountNames)

        bindingCallTokenArgKey :: Name -> HieAST TypeIndex -> Maybe (Set ValueOfArgKey)
        bindingCallTokenArgKey bindingName = go
          where
            go n =
                directCallArgs n
                    <|> infixCallArgs n
                    <|> listToMaybe (mapMaybe go (nodeChildren n))

            directCallArgs n = do
                guard $ nodeHasAnnotation hsAppAnnotation n
                let (headNode, args) = appSpine n
                guard $ bindingName `Set.member` operandUsedNames headNode
                guard (not $ null args)
                pure $
                    Set.fromList
                        [ ValueOfArgSource (bindingCallArgSource idx arg)
                        | (idx, arg) <- zip [0 :: Int ..] args
                        ]

            infixCallArgs n = do
                guard $ nodeHasAnnotation opAppAnnotation n
                _lhs:opNode:rhsNode:_ <- Just $ nodeChildren n
                guard $ bindingName `Set.member` operandUsedNames opNode
                pure $ Set.singleton $ ValueOfArgSource $ bindingCallArgSource 0 rhsNode

            bindingCallArgSource :: Int -> HieAST TypeIndex -> ByteString
            bindingCallArgSource idx argNode =
                BS8.pack (show idx) <> ":" <> bindingArgSource argNode

            bindingArgSource argNode =
                let fallback = BS8.intercalate "." (nodeOccNames argNode)
                in case slice (nodeSpan argNode) (hie_hs_src hie) of
                    Just src -> normalizeBindingArgSource src
                    Nothing -> fallback

            normalizeBindingArgSource =
                stripTypeAnnotations
                    . BS8.filter (\c -> not (isSpace c) && c /= '(' && c /= ')')

            stripTypeAnnotations bs = case BS8.breakSubstring "::" bs of
                (before, afterType)
                    | BS8.null afterType -> before
                    | otherwise ->
                        let rest = BS8.dropWhile isTypeAnnotationChar (BS8.drop 2 afterType)
                        in before <> stripTypeAnnotations rest

            isTypeAnnotationChar c =
                isAlphaNum c || c `elem` ("_'.:[]()," :: String)

    operandDirectValueOfMintKeys :: Set Name -> HieAST TypeIndex -> Set ValueOfTokenKey
    operandDirectValueOfMintKeys mintCarrierNames = go Set.empty
      where
        go :: Set ValueOfTokenKey -> HieAST TypeIndex -> Set ValueOfTokenKey
        go acc n@Node{nodeChildren = children} =
            let acc' = case directValueOfMintTokenKey mintCarrierNames n of
                    Just key -> Set.insert key acc
                    Nothing -> acc
            in foldl' go acc' children

    directValueOfMintTokenKey :: Set Name -> HieAST TypeIndex -> Maybe ValueOfTokenKey
    directValueOfMintTokenKey mintCarrierNames node =
        fullCallKey <|> infixPartialKey
      where
        fullCallKey :: Maybe ValueOfTokenKey
        fullCallKey = do
            (mintArg, csArg, tnArg) <- valueOfArgs node
            guard (mintArgMentionsMint mintArg)
            pure (ValueOfTokenKey (valueOfArgKey csArg) (valueOfArgKey tnArg))

        infixPartialKey :: Maybe ValueOfTokenKey
        infixPartialKey = do
            guard $ nodeHasAnnotation opAppAnnotation node
            mintArg:opNode:csArg:_ <- Just $ nodeChildren node
            guard $ subtreeHasAnyNameMeta valueOfPrimitiveNameMetas opNode
            guard (mintArgMentionsMint mintArg)
            let tnFallbackKey = Set.singleton $ ValueOfArgSource $ nodeSourceKey node
            pure (ValueOfTokenKey (valueOfArgKey csArg) tnFallbackKey)

        mintArgMentionsMint :: HieAST TypeIndex -> Bool
        mintArgMentionsMint mintArg =
            let mintArgNames = operandUsedNames mintArg
                hasTxInfoMintAlias =
                    not (Set.null $ mintArgNames `Set.intersection` mintCarrierNames)
            in subtreeHasAnyNameMeta txInfoMintPrimitiveNameMetas mintArg || hasTxInfoMintAlias

        valueOfArgs :: HieAST TypeIndex -> Maybe (HieAST TypeIndex, HieAST TypeIndex, HieAST TypeIndex)
        valueOfArgs n = do
            guard $ nodeHasAnnotation hsAppAnnotation n
            let (headNode, args) = appSpine n
            directCall headNode args <|> backtickCall headNode args

        directCall
            :: HieAST TypeIndex
            -> [HieAST TypeIndex]
            -> Maybe (HieAST TypeIndex, HieAST TypeIndex, HieAST TypeIndex)
        directCall headNode args = do
            guard $ subtreeHasAnyNameMeta valueOfPrimitiveNameMetas headNode
            mintArg <- args !!? 0
            csArg <- args !!? 1
            tnArg <- args !!? 2
            pure (mintArg, csArg, tnArg)

        backtickCall
            :: HieAST TypeIndex
            -> [HieAST TypeIndex]
            -> Maybe (HieAST TypeIndex, HieAST TypeIndex, HieAST TypeIndex)
        backtickCall headNode args = do
            tnArg <- args !!? 0
            guard $ nodeHasAnnotation opAppAnnotation headNode
            mintArg:opNode:csArg:_ <- Just $ nodeChildren headNode
            guard $ subtreeHasAnyNameMeta valueOfPrimitiveNameMetas opNode
            pure (mintArg, csArg, tnArg)

        valueOfArgKey :: HieAST TypeIndex -> Set ValueOfArgKey
        valueOfArgKey n =
            let source = nodeSourceKey n
                usedNames = Set.filter (nameAppearsInSource source) (operandUsedNames n)
                nameKeys = Set.map ValueOfArgName usedNames
            in if Set.null nameKeys
                then Set.singleton $ ValueOfArgSource source
                else nameKeys
          where
            nameAppearsInSource :: ByteString -> Name -> Bool
            nameAppearsInSource source name =
                let occ = BS8.pack (occNameString $ nameOccName name)
                in containsWordBS occ source

        nodeSourceKey :: HieAST TypeIndex -> ByteString
        nodeSourceKey n =
            let fallback = BS8.intercalate "." (nodeOccNames n)
            in case slice (nodeSpan n) (hie_hs_src hie) of
                Just src -> normalizeSourceKey src
                Nothing -> fallback

        normalizeSourceKey :: ByteString -> ByteString
        normalizeSourceKey =
            stripTypeAnnotations
                . BS8.filter (\c -> not (isSpace c) && c /= '(' && c /= ')')

        stripTypeAnnotations :: ByteString -> ByteString
        stripTypeAnnotations bs = case BS8.breakSubstring "::" bs of
            (before, afterType)
                | BS8.null afterType -> before
                | otherwise ->
                    let rest = BS8.dropWhile isTypeAnnotationChar (BS8.drop 2 afterType)
                    in before <> stripTypeAnnotations rest

        isTypeAnnotationChar :: Char -> Bool
        isTypeAnnotationChar c =
            isAlphaNum c || c `elem` ("_'.:[]()," :: String)

    subtreeHasOccName :: ByteString -> HieAST TypeIndex -> Bool
    subtreeHasOccName needle = go
      where
        go n@Node{nodeChildren = children} =
            needle `elem` nodeOccNames n || any go children

    nodeHasAnyNameMeta :: [NameMeta] -> HieAST TypeIndex -> Bool
    nodeHasAnyNameMeta metas n =
        let idents = Map.assocs $ nodeIdentifiers $ nodeInfo n
        in any (\pair -> any (`hieMatchNameMeta` pair) metas) idents

    subtreeHasAnyNameMeta :: [NameMeta] -> HieAST TypeIndex -> Bool
    subtreeHasAnyNameMeta metas = go
      where
        go n@Node{nodeChildren = children} =
            nodeHasAnyNameMeta metas n || any go children

    valueOfPrimitiveNameMetas :: [NameMeta]
    valueOfPrimitiveNameMetas =
        [ NameMeta
            { nameMetaName = "valueOf"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V1.Value"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = "valueOf"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V2.Value"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = "valueOf"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V3.Value"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , "valueOf" `plutusTxNameFrom` "PlutusTx.Value"
        ]

    flattenValuePrimitiveNameMetas :: [NameMeta]
    flattenValuePrimitiveNameMetas =
        [ NameMeta
            { nameMetaName = "flattenValue"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V1.Value"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = "flattenValue"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V2.Value"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = "flattenValue"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V3.Value"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , "flattenValue" `plutusTxNameFrom` "PlutusTx.Value"
        ]

    txInfoMintPrimitiveNameMetas :: [NameMeta]
    txInfoMintPrimitiveNameMetas =
        [ NameMeta
            { nameMetaName = "txInfoMint"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V1.Contexts"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = "txInfoMint"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V2.Contexts"
            , nameMetaPackage = "plutus-ledger-api"
            }
        , NameMeta
            { nameMetaName = "txInfoMint"
            , nameMetaModuleName = ModuleName "PlutusLedgerApi.V3.Contexts"
            , nameMetaPackage = "plutus-ledger-api"
            }
        ]

    operandUsedNames :: HieAST TypeIndex -> Set Name
    operandUsedNames = go Set.empty
      where
        go acc n@Node{nodeChildren = children} =
            let info = nodeInfo n
                occsHere = Set.fromList
                    [ name
                    | (Right name, IdentifierDetails{identInfo = identInfo'}) <- Map.assocs $ nodeIdentifiers info
                    , Set.member Use identInfo'
                    ]
            in foldl' go (acc <> occsHere) children

    operandIntegerLiteral :: HieAST TypeIndex -> Maybe Integer
    operandIntegerLiteral operand = do
        src <- slice (nodeSpan operand) (hie_hs_src hie)
        parseIntegerLiteral src

    parseIntegerLiteral :: ByteString -> Maybe Integer
    parseIntegerLiteral src = do
        let cleaned =
                BS8.filter (\c -> not (isSpace c) && c /= '(' && c /= ')' && c /= '_') src
        guard (not $ BS8.null cleaned)
        let (literalPart, rest) = splitTypeAnnotation cleaned
        n <- parseLiteral literalPart
        guard (BS8.null rest || isPureTypeAnnotation rest)
        pure n
      where
        splitTypeAnnotation :: ByteString -> (ByteString, ByteString)
        splitTypeAnnotation bs = case BS8.breakSubstring "::" bs of
            (before, after)
                | BS8.null after -> (bs, "")
                | otherwise -> (before, after)

        parseLiteral :: ByteString -> Maybe Integer
        parseLiteral bs = parsePrefixedLiteral bs <|> parseDecimalLiteral bs

        parseDecimalLiteral :: ByteString -> Maybe Integer
        parseDecimalLiteral bs = do
            (n, rest') <- BS8.readInteger bs
            guard (BS8.null rest')
            pure n

        parsePrefixedLiteral :: ByteString -> Maybe Integer
        parsePrefixedLiteral bs = do
            let (sign, body) = case BS8.uncons bs of
                    Just ('-', rest') -> ((-1), rest')
                    Just ('+', rest') -> (1, rest')
                    _ -> (1, bs)
            (base, digits) <- case () of
                _ | Just rest' <- BS8.stripPrefix "0x" body -> Just (16, rest')
                  | Just rest' <- BS8.stripPrefix "0X" body -> Just (16, rest')
                  | Just rest' <- BS8.stripPrefix "0o" body -> Just (8, rest')
                  | Just rest' <- BS8.stripPrefix "0O" body -> Just (8, rest')
                  | otherwise -> Nothing
            guard (not $ BS8.null digits)
            let parsed = case base of
                    16 -> readHex (BS8.unpack digits)
                    8 -> readOct (BS8.unpack digits)
                    _ -> []
            (value, rest') <- listToMaybe parsed
            guard (null rest')
            pure (sign * value)

        isPureTypeAnnotation :: ByteString -> Bool
        isPureTypeAnnotation rest = case BS8.stripPrefix "::" rest of
            Just annotation ->
                not (BS8.null annotation)
                    && BS8.all isTypeAnnotationChar annotation
            Nothing -> False

        isTypeAnnotationChar :: Char -> Bool
        isTypeAnnotationChar c =
            isAlphaNum c || c `elem` ("_'.:[]()," :: String)

    collectMintBindingRefs
        :: HieAST TypeIndex
        -> [(Name, RealSrcSpan)]
        -> RealSrcSpan
        -> ([BindingRef], [BindingRef])
    collectMintBindingRefs funNode bindingNameSpans span' =
        let bindings = collectLocalBindings span'
            mintCarrierOccs = closeMintCarrierOccs bindings
            mintCarrierRefs =
                [ BindingRef{bindingRefOcc = lhsOcc, bindingRefLine = lhsLine}
                | (lhsOcc, lhsLine, _rhsWords) <- bindings
                , lhsOcc `Set.member` mintCarrierOccs
                ]
            mintedValueRefs =
                [ BindingRef{bindingRefOcc = lhsOcc, bindingRefLine = lhsLine}
                | (lhsOcc, lhsLine, rhsWords) <- bindings
                , rhsMentionsMintValueOf mintCarrierOccs lhsOcc lhsLine rhsWords
                ]
        in (mintCarrierRefs, mintedValueRefs)
      where
        collectLocalBindings :: RealSrcSpan -> [(ByteString, Int, Set ByteString)]
        collectLocalBindings localSpan =
            go [] Nothing (srcNumberedLinesInSpan localSpan)

        go
            :: [(ByteString, Int, Set ByteString)]
            -> Maybe (ByteString, Int, Int, [ByteString])
            -> [(Int, ByteString)]
            -> [(ByteString, Int, Set ByteString)]
        go acc activeBinding [] = flushActiveBinding acc activeBinding
        go acc activeBinding ((lineNo, line):rest) =
            let code = lineCodePart line
                strippedCode = trimLeft code
                indent = lineIndent code
                isBlank = BS8.null strippedCode
            in case parseBindingLhs code of
                Just lhsOcc ->
                    let acc' = flushActiveBinding acc activeBinding
                        rhsStart = bindingRhsPart code
                    in go acc' (Just (lhsOcc, lineNo, indent, [rhsStart])) rest
                Nothing ->
                    case activeBinding of
                        Just (lhsOcc, lhsLine, bindIndent, rhsLines)
                            | isBlank || indent > bindIndent ->
                                go acc (Just (lhsOcc, lhsLine, bindIndent, code : rhsLines)) rest
                            | otherwise ->
                                go (flushActiveBinding acc activeBinding) Nothing rest
                        Nothing ->
                            go acc Nothing rest

        flushActiveBinding
            :: [(ByteString, Int, Set ByteString)]
            -> Maybe (ByteString, Int, Int, [ByteString])
            -> [(ByteString, Int, Set ByteString)]
        flushActiveBinding acc = \case
            Nothing -> acc
            Just (lhsOcc, lhsLine, _bindIndent, rhsLines) ->
                let rhsWords = identifierWords $ BS8.intercalate " " rhsLines
                in (lhsOcc, lhsLine, rhsWords) : acc

        rhsMentionsMintValueOf
            :: Set ByteString
            -> ByteString
            -> Int
            -> Set ByteString
            -> Bool
        rhsMentionsMintValueOf mintCarrierOccs lhsOcc lhsLine rhsWords =
            bindingHasPrimitiveValueOf
                && (Set.member "txInfoMint" rhsWords
                    || not (Set.null $ rhsWords `Set.intersection` mintCarrierOccs))
          where
            bindingHasPrimitiveValueOf = maybe False bindingSpanHasPrimitiveValueOf $
                bindingSpanForOccLine bindingNameSpans lhsOcc lhsLine

            bindingSpanHasPrimitiveValueOf bindSpan =
                spanHasAnyNameMetaWithin bindSpan valueOfPrimitiveNameMetas funNode

        closeMintCarrierOccs :: [(ByteString, Int, Set ByteString)] -> Set ByteString
        closeMintCarrierOccs bindings = expandOccs seedOccs
          where
            seedOccs =
                Set.fromList
                    [ lhsOcc
                    | (lhsOcc, _lhsLine, rhsWords) <- bindings
                    , Set.member "txInfoMint" rhsWords
                    ]

            expandOccs knownOccs =
                let knownOccs' = foldl'
                        (\acc (lhsOcc, _lhsLine, rhsWords) ->
                            if Set.null (rhsWords `Set.intersection` acc)
                                then acc
                                else Set.insert lhsOcc acc
                        )
                        knownOccs
                        bindings
                in if knownOccs' == knownOccs
                    then knownOccs
                    else expandOccs knownOccs'

        identifierWords :: ByteString -> Set ByteString
        identifierWords = Set.fromList . collectWords
          where
            collectWords input
                | BS8.null input = []
                | otherwise =
                    let trimmed = BS8.dropWhile (not . isIdentifierChar) input
                    in if BS8.null trimmed
                        then []
                        else
                            let (word, rest) = BS8.span isIdentifierChar trimmed
                            in word : collectWords rest

        bindingRhsPart :: ByteString -> ByteString
        bindingRhsPart line =
            let (_lhsRaw, eqAndRhs) = BS8.break (== '=') line
            in if BS8.null eqAndRhs
                then ""
                else BS8.drop 1 eqAndRhs

    collectFlattenAmountBindingNames
        :: [(Name, RealSrcSpan)]
        -> HieAST TypeIndex
        -> RealSrcSpan
        -> Set Name
        -> Set Name
    collectFlattenAmountBindingNames bindingNameSpans funNode span' mintCarrierNames =
        resolveBindingRefs bindingNameSpans $ collectFlattenAmountBindingRefs funNode span' mintCarrierNames

    collectFlattenAmountBindingRefs :: HieAST TypeIndex -> RealSrcSpan -> Set Name -> [BindingRef]
    collectFlattenAmountBindingRefs funNode span' mintCarrierNames =
        let numberedLines = srcNumberedLinesInSpan span'
            lineMap = Map.fromList numberedLines
            mintCaseStartLines = collectMintFlattenCaseStartLines funNode mintCarrierNames
            mintCaseBranchLines = collectMintCaseBranchLines mintCaseStartLines numberedLines
        in [ BindingRef{bindingRefOcc = amountOcc, bindingRefLine = amountLine}
           | (lineNo, line) <- mintCaseBranchLines
           , let candidate = flattenCandidate lineMap lineNo line
           , Just amountOcc <- [parseFlattenAmountOcc candidate <|> parseFlattenAmountOcc line]
           , let amountLine = findAmountLine lineMap lineNo amountOcc
           ]
      where
        findAmountLine :: Map Int ByteString -> Int -> ByteString -> Int
        findAmountLine lineMap lineNo amountOcc =
            fromMaybe lineNo $
                find
                    (\ln -> amountOccursInPattern amountOcc (Map.findWithDefault "" ln lineMap))
                    (reverse [max 1 (lineNo - 6) .. lineNo])
          where
            amountOccursInPattern :: ByteString -> ByteString -> Bool
            amountOccursInPattern occ lineText =
                let code = lineCodePart lineText
                    (beforeArrow, afterArrow) = BS8.breakSubstring "->" code
                    patternPart =
                        if BS8.null afterArrow
                            then code
                            else beforeArrow
                in containsWordBS occ patternPart

        flattenCandidate :: Map Int ByteString -> Int -> ByteString -> ByteString
        flattenCandidate lineMap lineNo line
            | "[" `BS8.isInfixOf` line = line
            | otherwise =
                let fromLine = max 1 (lineNo - 6)
                    lookback =
                        [ Map.findWithDefault "" ln lineMap
                        | ln <- [fromLine .. lineNo]
                        ]
                in BS8.intercalate " " lookback

        collectMintFlattenCaseStartLines :: HieAST TypeIndex -> Set Name -> Set Int
        collectMintFlattenCaseStartLines = go Set.empty
          where
            go acc n@Node{nodeChildren = children} carrierNames =
                let acc'
                        | isMintFlattenCaseNode carrierNames n =
                            Set.insert (srcSpanStartLine $ nodeSpan n) acc
                        | otherwise = acc
                in foldl' (\acc'' child -> go acc'' child carrierNames) acc' children

            isMintFlattenCaseNode carrierNames n
                | not $ nodeHasAnnotation hsCaseAnnotation n = False
                | otherwise = case nodeChildren n !!? 0 of
                    Just scrutinee ->
                        subtreeHasAnyNameMeta flattenValuePrimitiveNameMetas scrutinee
                            && scrutineeMentionsMintCarrier carrierNames scrutinee
                    Nothing -> False

            scrutineeMentionsMintCarrier carrierNames scrutinee =
                let scrutineeNames = operandUsedNames scrutinee
                in subtreeHasAnyNameMeta txInfoMintPrimitiveNameMetas scrutinee
                    || not (Set.null $ scrutineeNames `Set.intersection` carrierNames)

            hsCaseAnnotation :: NodeAnnotation
            hsCaseAnnotation = mkNodeAnnotation "HsCase" "HsExpr"

        collectMintCaseBranchLines :: Set Int -> [(Int, ByteString)] -> [(Int, ByteString)]
        collectMintCaseBranchLines mintCaseStartLines = goOutside []
          where
            goOutside :: [(Int, ByteString)] -> [(Int, ByteString)] -> [(Int, ByteString)]
            goOutside acc [] = acc
            goOutside acc ((lineNo, line):rest) =
                let code = lineCodePart line
                    strippedCode = trimLeft code
                in if isCaseStart strippedCode && lineNo `Set.member` mintCaseStartLines
                    then startCaseCapture acc (lineIndent code) code rest
                    else goOutside acc rest

            startCaseCapture
                :: [(Int, ByteString)]
                -> Int
                -> ByteString
                -> [(Int, ByteString)]
                -> [(Int, ByteString)]
            startCaseCapture acc caseIndent caseLine rest =
                if containsWordBS "of" (trimLeft caseLine)
                    then goMintCase acc caseIndent Nothing [] rest
                    else goCaseHeader acc caseIndent rest

            goCaseHeader
                :: [(Int, ByteString)]
                -> Int
                -> [(Int, ByteString)]
                -> [(Int, ByteString)]
            goCaseHeader acc _caseIndent [] = acc
            goCaseHeader acc caseIndent ((lineNo, line):rest) =
                let code = lineCodePart line
                    strippedCode = trimLeft code
                    indent = lineIndent code
                    isBlank = BS8.null strippedCode
                in if containsWordBS "of" strippedCode
                    then goMintCase acc caseIndent Nothing [] rest
                    else if not isBlank && indent <= caseIndent
                        then if isCaseStart strippedCode && lineNo `Set.member` mintCaseStartLines
                            then startCaseCapture acc indent code rest
                            else goOutside acc rest
                        else goCaseHeader acc caseIndent rest

            isCaseStart :: ByteString -> Bool
            isCaseStart strippedCode =
                containsWordBS "case" strippedCode

            addBranchLine
                :: Int
                -> ByteString
                -> [(Int, ByteString)]
                -> [(Int, ByteString)]
            addBranchLine lineNo line = ((lineNo, line) :)

            goMintCase
                :: [(Int, ByteString)]
                -> Int
                -> Maybe Int
                -> [(Int, ByteString)]
                -> [(Int, ByteString)]
                -> [(Int, ByteString)]
            goMintCase acc _caseIndent _branchIndent branchLines [] = branchLines <> acc
            goMintCase acc caseIndent branchIndent branchLines ((lineNo, line):rest) =
                let code = lineCodePart line
                    strippedCode = trimLeft code
                    indent = lineIndent code
                    isBlank = BS8.null strippedCode
                    acc' = branchLines <> acc
                in if not isBlank && indent <= caseIndent
                    then if isCaseStart strippedCode && lineNo `Set.member` mintCaseStartLines
                        then startCaseCapture acc' indent code rest
                        else goOutside acc' rest
                    else if "->" `BS8.isInfixOf` code
                        then case branchIndent of
                            Nothing ->
                                goMintCase acc caseIndent (Just indent) (addBranchLine lineNo code branchLines) rest
                            Just topBranchIndent
                                | indent == topBranchIndent ->
                                    goMintCase acc caseIndent branchIndent (addBranchLine lineNo code branchLines) rest
                                | otherwise ->
                                    goMintCase acc caseIndent branchIndent branchLines rest
                        else goMintCase acc caseIndent branchIndent branchLines rest

    parseFlattenAmountOcc :: ByteString -> Maybe ByteString
    parseFlattenAmountOcc line = do
        guard ("->" `BS8.isInfixOf` line)
        inside <- betweenSquareBrackets line
        let parts = map trimRight $ BS8.split ',' inside
        thirdRaw <- case parts of
            _one:_two:three:_rest -> Just three
            _ -> Nothing
        let
            amountOcc = BS8.filter isIdentifierChar thirdRaw
        guard (not $ BS8.null amountOcc)
        guard (amountOcc /= "_")
        pure amountOcc

    betweenSquareBrackets :: ByteString -> Maybe ByteString
    betweenSquareBrackets line = do
        let (_beforeOpen, openAndRest) = BS8.break (== '[') line
        guard (not $ BS8.null openAndRest)
        let afterOpen = BS8.drop 1 openAndRest
            (inside, closeAndRest) = BS8.break (== ']') afterOpen
        guard (not $ BS8.null closeAndRest)
        pure inside

    srcNumberedLinesInSpan :: RealSrcSpan -> [(Int, ByteString)]
    srcNumberedLinesInSpan span' =
        let startLine = srcSpanStartLine span'
            endLine = srcSpanEndLine span'
            allLines = BS8.lines $ hie_hs_src hie
        in mapMaybe
            (\lineNo -> fmap ((,) lineNo) (allLines !!? (lineNo - 1)))
            [startLine .. endLine]

    collectBindingNameSpans :: HieAST TypeIndex -> [(Name, RealSrcSpan)]
    collectBindingNameSpans = go []
      where
        go acc n@Node{nodeChildren = children} =
            let info = nodeInfo n
                bindingsHere = mapMaybe (extractBinding $ nodeSpan n) (Map.assocs $ nodeIdentifiers info)
            in foldl' go (bindingsHere <> acc) children

        extractBinding
            :: RealSrcSpan
            -> (Identifier, IdentifierDetails TypeIndex)
            -> Maybe (Name, RealSrcSpan)
        extractBinding fallbackSpan (ident, details) = case ident of
            Right name | Just bindSpan <- getBindingSpanOrFallback fallbackSpan details ->
                Just (name, bindSpan)
            _ -> Nothing

    getBindingSpanOrFallback
        :: RealSrcSpan
        -> IdentifierDetails TypeIndex
        -> Maybe RealSrcSpan
    getBindingSpanOrFallback fallbackSpan details =
        getBindingSpan details
            <|> if hasBindingCtx details
                then Just fallbackSpan
                else Nothing

    getBindingSpan :: IdentifierDetails TypeIndex -> Maybe RealSrcSpan
    getBindingSpan IdentifierDetails{identInfo = identInfo'} =
        listToMaybe $ mapMaybe spanFromCtx (toList identInfo')
      where
        spanFromCtx (ValBind _ _ (Just s)) = Just s
        spanFromCtx (PatternBind _ _ (Just s)) = Just s
        spanFromCtx _ = Nothing

    hasBindingCtx :: IdentifierDetails TypeIndex -> Bool
    hasBindingCtx IdentifierDetails{identInfo = identInfo'} =
        any isBindingContext identInfo'
      where
        isBindingContext (ValBind _ _ _) = True
        isBindingContext (PatternBind _ _ _) = True
        isBindingContext _ = False

    resolveBindingRefs :: [(Name, RealSrcSpan)] -> [BindingRef] -> Set Name
    resolveBindingRefs bindingNameSpans refs =
        Set.fromList $ mapMaybe (resolveBindingRef bindingNameSpans) refs

    resolveBindingRef :: [(Name, RealSrcSpan)] -> BindingRef -> Maybe Name
    resolveBindingRef bindingNameSpans BindingRef{bindingRefOcc = occ, bindingRefLine = lineNo} =
        fst <$> pickMostSpecificSpan
            [ (name, bindSpan)
            | (name, bindSpan) <- bindingNameSpans
            , BS8.pack (occNameString $ nameOccName name) == occ
            , lineWithinSpan lineNo bindSpan
            ]

    pickMostSpecificSpan :: [(Name, RealSrcSpan)] -> Maybe (Name, RealSrcSpan)
    pickMostSpecificSpan = foldl' pick Nothing
      where
        pick Nothing candidate = Just candidate
        pick (Just best@(_bestName, bestSpan)) candidate@(_candName, candSpan)
            | spanSize candSpan < spanSize bestSpan = Just candidate
            | otherwise = Just best

    spanSize :: RealSrcSpan -> (Int, Int)
    spanSize span' =
        if srcSpanEndLine span' == srcSpanStartLine span'
            then (0, srcSpanEndCol span' - srcSpanStartCol span')
            else (srcSpanEndLine span' - srcSpanStartLine span', srcSpanEndCol span')

    lineWithinSpan :: Int -> RealSrcSpan -> Bool
    lineWithinSpan lineNo span' =
        lineNo >= srcSpanStartLine span'
            && lineNo <= srcSpanEndLine span'

    bindingSpanForOccLine :: [(Name, RealSrcSpan)] -> ByteString -> Int -> Maybe RealSrcSpan
    bindingSpanForOccLine bindingNameSpans occ lineNo =
        snd <$> pickMostSpecificSpan
            [ (name, bindSpan)
            | (name, bindSpan) <- bindingNameSpans
            , BS8.pack (occNameString $ nameOccName name) == occ
            , lineWithinSpan lineNo bindSpan
            ]

    spanHasAnyNameMetaWithin :: RealSrcSpan -> [NameMeta] -> HieAST TypeIndex -> Bool
    spanHasAnyNameMetaWithin targetSpan metas = go
      where
        go n@Node{nodeSpan = currentSpan, nodeChildren = children}
            | not $ spanOverlaps targetSpan currentSpan = False
            | targetSpan `spanContainsOrEq` currentSpan && nodeHasAnyNameMeta metas n = True
            | otherwise = any go children

    spanContainsOrEq :: RealSrcSpan -> RealSrcSpan -> Bool
    spanContainsOrEq outer inner =
        let startsAfter =
                srcSpanStartLine inner > srcSpanStartLine outer
                || (srcSpanStartLine inner == srcSpanStartLine outer
                    && srcSpanStartCol inner >= srcSpanStartCol outer)
            endsBefore =
                srcSpanEndLine inner < srcSpanEndLine outer
                || (srcSpanEndLine inner == srcSpanEndLine outer
                    && srcSpanEndCol inner <= srcSpanEndCol outer)
        in startsAfter && endsBefore

    spanOverlaps :: RealSrcSpan -> RealSrcSpan -> Bool
    spanOverlaps a b =
        let aStartsBeforeBEnds =
                srcSpanStartLine a < srcSpanEndLine b
                || (srcSpanStartLine a == srcSpanEndLine b
                    && srcSpanStartCol a <= srcSpanEndCol b)
            bStartsBeforeAEnds =
                srcSpanStartLine b < srcSpanEndLine a
                || (srcSpanStartLine b == srcSpanEndLine a
                    && srcSpanStartCol b <= srcSpanEndCol a)
        in aStartsBeforeBEnds && bStartsBeforeAEnds

    lineCodePart :: ByteString -> ByteString
    lineCodePart = fst . BS8.breakSubstring "--"

    parseBindingLhs :: ByteString -> Maybe ByteString
    parseBindingLhs line = do
        let (lhsRaw, eqAndRhs) = BS8.break (== '=') line
            lhsTrimmed = trimRight lhsRaw
            beforeEq = if BS8.null lhsTrimmed then Nothing else Just (BS8.last lhsTrimmed)
        guard (not $ BS8.null eqAndRhs)
        guard (not $ BS8.isPrefixOf "==" eqAndRhs)
        guard $ case beforeEq of
            Just c -> c `notElem` ['<', '>', '!', '/', ':']
            Nothing -> False
        lhsBindingOcc lhsRaw

    lhsBindingOcc :: ByteString -> Maybe ByteString
    lhsBindingOcc lhsRaw =
        listToMaybe $
            filter (not . isBindingKeyword) (identifierWordsInOrder lhsRaw)
      where
        isBindingKeyword :: ByteString -> Bool
        isBindingKeyword word =
            word == "let" || word == "where"

        identifierWordsInOrder :: ByteString -> [ByteString]
        identifierWordsInOrder = collectWords
          where
            collectWords input
                | BS8.null input = []
                | otherwise =
                    let trimmed = BS8.dropWhile (not . isIdentifierChar) input
                    in if BS8.null trimmed
                        then []
                        else
                            let (word, rest) = BS8.span isIdentifierChar trimmed
                            in word : collectWords rest

    trimRight :: ByteString -> ByteString
    trimRight = BS8.reverse . BS8.dropWhile isSpace . BS8.reverse

    trimLeft :: ByteString -> ByteString
    trimLeft = BS8.dropWhile isSpace

    lineIndent :: ByteString -> Int
    lineIndent = BS8.length . BS8.takeWhile isSpace

    containsWordBS :: ByteString -> ByteString -> Bool
    containsWordBS needle haystack = go haystack
      where
        go bs = case BS8.breakSubstring needle bs of
            (_before, afterNeedle) | BS8.null afterNeedle -> False
            (before, afterNeedle) ->
                let after = BS8.drop (BS8.length needle) afterNeedle
                    boundaryBefore = BS8.null before || not (isIdentifierChar $ BS8.last before)
                    boundaryAfter = BS8.null after || not (isIdentifierChar $ BS8.head after)
                in if boundaryBefore && boundaryAfter
                    then True
                    else go (BS8.tail afterNeedle)

    isIdentifierChar :: Char -> Bool
    isIdentifierChar c = isAlphaNum c || c == '_' || c == '\''

    nodeHasAnnotation :: NodeAnnotation -> HieAST TypeIndex -> Bool
    nodeHasAnnotation ann node =
        let NodeInfo{nodeAnnotations = nodeAnnotations'} = nodeInfo node
        in ann `Set.member` Set.map toNodeAnnotation nodeAnnotations'

    nodeOccNames :: HieAST TypeIndex -> [ByteString]
    nodeOccNames node =
        [ BS8.pack $ occNameString $ nameOccName name
        | ident <- Map.keys $ nodeIdentifiers $ nodeInfo node
        , Right name <- [ident]
        ]

    hsAppAnnotation :: NodeAnnotation
    hsAppAnnotation = mkNodeAnnotation "HsApp" "HsExpr"

    opAppAnnotation :: NodeAnnotation
    opAppAnnotation = mkNodeAnnotation "OpApp" "HsExpr"

    appSpine :: HieAST TypeIndex -> (HieAST TypeIndex, [HieAST TypeIndex])
    appSpine node = case node of
        n@Node{nodeChildren = appFun:arg:_}
            | nodeHasAnnotation hsAppAnnotation n ->
                let (f, args) = appSpine appFun
                in (f, args <> [arg])
        _ -> (node, [])

-- | Returns source spans of matched AST nodes.
-- | Returns source spans of matched AST nodes.
createMatch
    :: PatternAst
    -> HieFile
    -> HieAST TypeIndex
    -> Slist RealSrcSpan
createMatch patAst hie node =
    memptyIfFalse (hieMatchPatternAst hie node patAst) (S.one $ nodeSpan node)

{- | Specialized version of 'matchAstWith' where custom predicate
always returns 'True'.
-}
matchAst
    :: Id Inspection
    -> PatternAst
    -> HieFile
    -> HieAST TypeIndex  -- ^ Current node
    -> State VisitorState ()
matchAst = matchAstWith (const True)

{- | Add observation to the state if the given node matches the given
'PatternAst' exactly (non-recursively) and if the given custom
predicate returns 'True'..
-}
matchAstWith
    :: (HieAST TypeIndex -> Bool)  -- ^ Custom node check
    -> Id Inspection
    -> PatternAst
    -> HieFile
    -> HieAST TypeIndex  -- ^ Current node
    -> State VisitorState ()
matchAstWith check insId patAst hie node@Node{..} =
    when (hieMatchPatternAst hie node patAst && check node) $
        addObservation $ mkObservation insId hie nodeSpan
