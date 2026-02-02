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
                        srcSpanEndLine, srcSpanStartCol, srcSpanStartLine, isExternalName)
import Stan.Hie (eqAst, slice)
import Stan.Hie.Compat (ContextInfo (..), HieAST (..), HieASTs (..), HieFile (..),
                        Identifier, IdentifierDetails (..), NodeAnnotation, NodeInfo (..),
                        TypeIndex, mkNodeAnnotation, nodeInfo, toNodeAnnotation)
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

import Data.Char (isAlphaNum, isLower, isSpace)
import qualified Data.ByteString.Char8 as BS8
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
                -- If we've matched a comparison expression, don't recurse into it;
                -- the HIE tree can contain nested nodes with the same span.
                S.one $ nodeSpan node
        _ ->
            foldMap matchNode (nodeChildren node)

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
