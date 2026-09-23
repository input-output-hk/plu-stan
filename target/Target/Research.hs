{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
-- Executable adaptations of the pinned Cardano-CWE-Research examples.
-- Each case is independently mapped in test/cwe-conformance.json.
module Target.Research where
import PlutusTx qualified as Tx
import PlutusTx.Prelude qualified as P
import PlutusTx.Builtins qualified as B
import PlutusTx.AssocMap qualified as M
import PlutusLedgerApi.V2
import PlutusLedgerApi.V2.Contexts (txSignedBy)
import PlutusLedgerApi.V1.Value qualified as V
import PlutusLedgerApi.V1.Interval qualified as I
{-# ANN module ("onchain-contract" :: String) #-}

data TestDatum = TestDatum { amount :: Integer, owner :: PubKeyHash }
Tx.makeIsDataIndexed ''TestDatum [('TestDatum, 0)]
data IndexRedeemer = IndexRedeemer { inputIndex :: Integer }
Tx.makeIsDataIndexed ''IndexRedeemer [('IndexRedeemer, 0)]
data MapDatum = MapDatum { extraInfo :: M.Map BuiltinByteString Integer }
Tx.makeIsDataIndexed ''MapDatum [('MapDatum, 0)]

-- PrecisionLoss: invalid
precisionBad :: Integer -> Integer -> Integer -> Integer
precisionBad a b c = (a `div` b) * c

-- PrecisionLoss: invalid
precisionQuot :: Integer -> Integer -> Integer -> Integer
precisionQuot a b c = (a `quot` b) * c

-- PrecisionLoss: invalid
precisionAlias :: Integer -> Integer -> Integer -> Integer
precisionAlias a b c = let ratio = a `div` b in ratio * c

-- PrecisionLoss: valid
precisionGood :: Integer -> Integer -> Integer -> Integer
precisionGood a b c = (a * c) `div` b

-- EmptyStringADACheck: invalid
adaLiteral :: TokenName -> Bool
adaLiteral tn = tn == V.tokenName ""

-- EmptyStringADACheck: invalid
adaEmptyBuiltin :: TokenName -> Bool
adaEmptyBuiltin tn = tn == V.TokenName B.emptyByteString

-- EmptyStringADACheck: invalid
adaReverse :: TokenName -> Bool
adaReverse tn = V.TokenName B.emptyByteString == tn

-- EmptyStringADACheck: invalid
adaCurrency :: CurrencySymbol -> Bool
adaCurrency cs = cs == V.CurrencySymbol B.emptyByteString

-- EmptyStringADACheck: valid
adaGood :: TokenName -> Bool
adaGood tn = tn == V.adaToken

-- UnstableMakeIsData: Stable splice above must not warn; see splice cases below
stableMarker :: Integer -> Integer
stableMarker x = x

data UnstableDatum = UnstableDatum Integer
Tx.unstableMakeIsData ''UnstableDatum

-- ImmutableCredential: Top-level credential reachable from mkValidator
bakedAdmin :: PubKeyHash
bakedAdmin = "abc"

-- ImmutableCredential: Diagnostic belongs to credential definition
mkValidator :: ScriptContext -> Bool
mkValidator ctx = txSignedBy (scriptContextTxInfo ctx) bakedAdmin

-- ImmutableCredential: valid
mutableAdmin :: TestDatum -> ScriptContext -> Bool
mutableAdmin d ctx = txSignedBy (scriptContextTxInfo ctx) (owner d)

-- ZipWithoutLengthCheck: invalid
zipBad :: [Integer] -> [Integer] -> Bool
zipBad xs ys = all (\(a,b) -> a == b) (zip xs ys)

-- ZipWithoutLengthCheck: valid
zipGood :: [Integer] -> [Integer] -> Bool
zipGood xs ys = length xs == length ys && all (\(a,b) -> a == b) (zip xs ys)

-- ZipWithoutLengthCheck: invalid
zipUnrelated :: [Integer] -> [Integer] -> [Integer] -> Bool
zipUnrelated xs ys zs = length xs == length zs && all (\(a,b) -> a == b) (zip xs ys)

-- ZipWithoutLengthCheck: invalid
zipComment :: [Integer] -> [Integer] -> Bool
zipComment xs ys =
  -- length xs == length ys
  all (\(a,b) -> a == b) (zip xs ys)

-- ZipWithoutLengthCheck: invalid
zipDead :: [Integer] -> [Integer] -> Bool
zipDead xs ys = let unused = length xs == length ys in all (\(a,b) -> a == b) (zip xs ys)

-- MissingAddressValidation: invalid
addressBad :: TxOut -> Datum -> Bool
addressBad out d = txOutDatum out == OutputDatum d

-- MissingAddressValidation: valid
addressGood :: TxOut -> Datum -> Address -> Bool
addressGood out d addr = txOutDatum out == OutputDatum d && txOutAddress out == addr

-- MissingAddressValidation: invalid
addressOther :: TxOut -> TxOut -> Datum -> Address -> Bool
addressOther out other d addr = txOutDatum out == OutputDatum d && txOutAddress other == addr

-- MissingAddressValidation: valid
addressAliasGood :: TxOut -> Address -> Bool
addressAliasGood out addr = let alias = out in txOutAddress alias == addr

-- MissingStakingValidation: invalid
stakeBad :: TxOut -> Credential -> Bool
stakeBad out cred = addressCredential (txOutAddress out) == cred

-- MissingStakingValidation: valid
stakeGood :: TxOut -> Credential -> Bool
stakeGood out cred = addressCredential (txOutAddress out) == cred && addressStakingCredential (txOutAddress out) == Nothing

-- MissingStakingValidation: valid
stakeFullAddress :: TxOut -> Address -> Bool
stakeFullAddress out addr = txOutAddress out == addr

-- UnvalidatedReferenceScript: invalid
referenceBad :: TxOut -> Address -> Bool
referenceBad out addr = txOutAddress out == addr

-- UnvalidatedReferenceScript: valid
referenceGood :: TxOut -> Address -> Bool
referenceGood out addr = txOutAddress out == addr && txOutReferenceScript out == Nothing

-- UnvalidatedReferenceScript: invalid
referenceDead :: TxOut -> Address -> Bool
referenceDead out addr = let unused = txOutReferenceScript out == Nothing in txOutAddress out == addr

-- UnvalidatedDatum: invalid
datumBad :: TxOut -> Address -> Bool
datumBad out addr = txOutAddress out == addr

-- UnvalidatedDatum: valid
datumGood :: TxOut -> Address -> Datum -> Bool
datumGood out addr d = txOutAddress out == addr && txOutDatum out == OutputDatum d

-- UnvalidatedDatum: invalid
datumWildcard :: TxOut -> Address -> Bool
datumWildcard out addr = txOutAddress out == addr && case txOutDatum out of
  OutputDatum _ -> True
  _ -> False

-- UnvalidatedDatum: valid
datumFieldsGood :: TxOut -> Address -> Bool
datumFieldsGood out addr = txOutAddress out == addr && case txOutDatum out of
  OutputDatum (Datum d) -> case Tx.fromBuiltinData d of
    Just (TestDatum n _) -> n > 0
    Nothing -> False
  _ -> False

-- TrashTokens: invalid
trashSubset :: TxOut -> Value -> Bool
trashSubset out expected = txOutValue out `V.geq` expected

-- TrashTokens: invalid
trashAsset :: TxOut -> V.AssetClass -> Bool
trashAsset out asset = V.assetClassValueOf (txOutValue out) asset >= 1

-- TrashTokens: invalid
trashMissing :: TxOut -> Address -> Bool
trashMissing out addr = txOutAddress out == addr

-- TrashTokens: valid
trashGood :: TxOut -> Value -> Bool
trashGood out expected = txOutValue out == expected

-- TrashTokens: Supported bounded-token-set alternative
trashBounded :: TxOut -> Value -> Bool
trashBounded out expected = txOutValue out `V.geq` expected && length (V.flattenValue (txOutValue out)) <= 2

-- UncheckedRedeemer: invalid
redeemerBad :: TxInfo -> Bool
redeemerBad info = any (\i -> case addressCredential (txOutAddress (txInInfoResolved i)) of
  ScriptCredential _ -> True
  _ -> False) (txInfoInputs info)

-- UncheckedRedeemer: valid
redeemerGood :: TxInfo -> TxOutRef -> Redeemer -> Bool
redeemerGood info ref expected = redeemerBad info && M.lookup (Spending ref) (txInfoRedeemers info) == Just expected

-- UncheckedRedeemer: invalid
redeemerDead :: TxInfo -> TxOutRef -> Bool
redeemerDead info ref = let unused = M.lookup (Spending ref) (txInfoRedeemers info) in redeemerBad info

-- UncheckedRedeemer: invalid
redeemerComment :: TxInfo -> Bool
redeemerComment info =
  -- lookup Spending txInfoRedeemers
  redeemerBad info

-- UncheckedRedeemer: valid
redeemerReferences :: TxInfo -> Bool
redeemerReferences info = any (\i -> case addressCredential (txOutAddress (txInInfoResolved i)) of
  ScriptCredential _ -> True
  _ -> False) (txInfoReferenceInputs info)

-- ReadOnlySpend: invalid
readOnlyFull :: TxInInfo -> TxOut -> Bool
readOnlyFull i out = txOutAddress (txInInfoResolved i) == txOutAddress out && txOutValue (txInInfoResolved i) == txOutValue out && txOutDatum (txInInfoResolved i) == txOutDatum out && txOutReferenceScript (txInInfoResolved i) == txOutReferenceScript out

-- ReadOnlySpend: invalid
readOnlyDatum :: TxInInfo -> TxOut -> Bool
readOnlyDatum i out = txOutDatum (txInInfoResolved i) == txOutDatum out

-- ReadOnlySpend: valid
readOnlyChanged :: TxInInfo -> TxOut -> Datum -> Bool
readOnlyChanged i out next = txOutDatum out == OutputDatum next && txOutValue out == txOutValue (txInInfoResolved i)

-- ReadOnlySpend: invalid
readOnlyUnrelatedReference :: TxInfo -> TxInInfo -> TxOut -> Bool
readOnlyUnrelatedReference info i out = length (txInfoReferenceInputs info) > 0 && txOutDatum (txInInfoResolved i) == txOutDatum out

-- ValidityRangeBound: invalid
timeFiniteBad :: TxInfo -> POSIXTime -> Bool
timeFiniteBad info deadline = case I.ivFrom (txInfoValidRange info) of
  I.LowerBound (I.Finite lo) _ -> lo >= deadline
  _ -> False

-- ValidityRangeBound: valid
timeGood :: TxInfo -> POSIXTime -> Bool
timeGood info maxDuration = case (I.ivFrom (txInfoValidRange info), I.ivTo (txInfoValidRange info)) of
  (I.LowerBound (I.Finite lo) _, I.UpperBound (I.Finite hi) _) -> hi - lo <= maxDuration
  _ -> False

-- ValidityRangeBound: invalid
timeContainsBad :: TxInfo -> POSIXTime -> Bool
timeContainsBad info deadline = I.contains (I.from deadline) (txInfoValidRange info)

-- ValidityRangeBound: valid
timeNoUse :: Integer -> Bool
timeNoUse n = n > 0

-- DatumComparisonOptimization: invalid
decodeCompare :: BuiltinData -> TestDatum -> Bool
decodeCompare d expected = case Tx.fromBuiltinData d of
  Just (TestDatum n pkh) -> n == amount expected && pkh == owner expected
  Nothing -> False

-- DatumComparisonOptimization: invalid
decodeUnsafeCompare :: BuiltinData -> TestDatum -> Bool
decodeUnsafeCompare d expected = let actual = Tx.unsafeFromBuiltinData d in amount actual == amount expected && owner actual == owner expected

-- DatumComparisonOptimization: valid
encodeCompare :: BuiltinData -> TestDatum -> Bool
encodeCompare d expected = d == Tx.toBuiltinData expected

-- DatumComparisonOptimization: valid
decodeValidate :: BuiltinData -> Bool
decodeValidate d = case Tx.fromBuiltinData d of
  Just (TestDatum n _) -> n > 0
  Nothing -> False

-- IncompleteTokenValidation: invalid
tokenWildcard0 :: TxInfo -> CurrencySymbol -> TokenName -> Bool
tokenWildcard0 info symbol name = all (\(_, tn, q) -> tn == name && q == 1) (V.flattenValue (txInfoMint info))

-- IncompleteTokenValidation: invalid
tokenWildcard1 :: TxInfo -> CurrencySymbol -> TokenName -> Bool
tokenWildcard1 info symbol name = all (\(cs, _, q) -> cs == symbol && q == 1) (V.flattenValue (txInfoMint info))

-- IncompleteTokenValidation: invalid
tokenWildcard2 :: TxInfo -> CurrencySymbol -> TokenName -> Bool
tokenWildcard2 info symbol name = all (\(cs, tn, _) -> cs == symbol && tn == name) (V.flattenValue (txInfoMint info))

-- IncompleteTokenValidation: valid
tokenGood :: TxInfo -> CurrencySymbol -> TokenName -> Bool
tokenGood info symbol name = all (\(cs, tn, q) -> cs == symbol && tn == name && q == 1) (V.flattenValue (txInfoMint info))

-- IncompleteTokenValidation: invalid
tokenOutput :: TxOut -> CurrencySymbol -> Bool
tokenOutput out symbol = all (\(cs, _, _) -> cs == symbol) (V.flattenValue (txOutValue out))

-- StrictValueEquality: invalid
strictAda :: TxOut -> Lovelace -> Bool
strictAda out n = V.lovelaceValueOf (txOutValue out) == n

-- StrictValueEquality: invalid
strictAdaReverse :: TxOut -> Lovelace -> Bool
strictAdaReverse out n = n == V.lovelaceValueOf (txOutValue out)

-- StrictValueEquality: valid
minimumAda :: TxOut -> Lovelace -> Bool
minimumAda out n = V.lovelaceValueOf (txOutValue out) >= n

-- UnvalidatedInputIndex: invalid
indexBad :: TxInfo -> IndexRedeemer -> Address -> Bool
indexBad info r addr = let selected = txInfoInputs info !! fromInteger (inputIndex r) in txOutAddress (txInInfoResolved selected) == addr

-- UnvalidatedInputIndex: valid
indexGood :: TxInfo -> IndexRedeemer -> CurrencySymbol -> TokenName -> Bool
indexGood info r cs tn = let selected = txInfoInputs info !! fromInteger (inputIndex r) in V.valueOf (txOutValue (txInInfoResolved selected)) cs tn == 1

-- UnvalidatedInputIndex: invalid
referenceIndexBad :: TxInfo -> IndexRedeemer -> Address -> Bool
referenceIndexBad info r addr = let selected = txInfoReferenceInputs info !! fromInteger (inputIndex r) in txOutAddress (txInInfoResolved selected) == addr

-- UnvalidatedInputIndex: valid
indexAssetGood :: TxInfo -> IndexRedeemer -> V.AssetClass -> Bool
indexAssetGood info r asset = let selected = txInfoReferenceInputs info !! fromInteger (inputIndex r) in V.assetClassValueOf (txOutValue (txInInfoResolved selected)) asset >= 1

-- UnvalidatedInputIndex: invalid
indexWrongInput :: TxInfo -> IndexRedeemer -> TxInInfo -> CurrencySymbol -> TokenName -> Address -> Bool
indexWrongInput info r other cs tn addr = let selected = txInfoInputs info !! fromInteger (inputIndex r) in txOutAddress (txInInfoResolved selected) == addr && V.valueOf (txOutValue (txInInfoResolved other)) cs tn == 1

-- HelperFunctions: invalid
forwardHelper :: PubKeyHash -> TxInfo -> Bool
forwardHelper pkh info = txSignedBy info pkh

-- HelperFunctions: invalid
patternHelper :: Maybe Integer -> Integer
patternHelper x = case x of
  Just n -> n
  Nothing -> 0

-- HelperFunctions: valid
substantialHelper :: Integer -> Integer -> Bool
substantialHelper x y = x > 0 && y > x

-- FixedStructureMap: invalid
mapBad :: MapDatum -> Bool
mapBad d = M.member "fee" (extraInfo d) && M.member "owner" (extraInfo d)

-- FixedStructureMap: valid
mapDynamic :: BuiltinByteString -> MapDatum -> Bool
mapDynamic key d = M.member key (extraInfo d)

-- FixedStructureMap: valid
recordGood :: TestDatum -> Bool
recordGood d = amount d > 0


-- UncheckedRedeemer: valid
redeemerCorresponding :: TxInfo -> Redeemer -> Bool
redeemerCorresponding info expected = all (\i -> case addressCredential (txOutAddress (txInInfoResolved i)) of
  ScriptCredential _ -> M.lookup (Spending (txInInfoOutRef i)) (txInfoRedeemers info) == Just expected
  _ -> True) (txInfoInputs info)

-- UncheckedRedeemer: invalid
redeemerWrongPurpose :: TxInfo -> Redeemer -> TxOutRef -> Bool
redeemerWrongPurpose info expected other = all (\i -> case addressCredential (txOutAddress (txInInfoResolved i)) of
  ScriptCredential _ -> M.lookup (Spending other) (txInfoRedeemers info) == Just expected
  _ -> True) (txInfoInputs info)

-- ReadOnlySpend: invalid
readOnlyFields :: TxInInfo -> TxOut -> Bool
readOnlyFields i out = let
  old = Tx.unsafeFromBuiltinData (getDatum (case txOutDatum (txInInfoResolved i) of OutputDatum d -> d; _ -> error "datum"))
  new = Tx.unsafeFromBuiltinData (getDatum (case txOutDatum out of OutputDatum d -> d; _ -> error "datum"))
  in amount old == amount new && owner old == owner new

-- ReadOnlySpend: valid
readOnlyPartialFields :: TxInInfo -> TxOut -> Bool
readOnlyPartialFields i out = let
  old = Tx.unsafeFromBuiltinData (getDatum (case txOutDatum (txInInfoResolved i) of OutputDatum d -> d; _ -> error "datum"))
  new = Tx.unsafeFromBuiltinData (getDatum (case txOutDatum out of OutputDatum d -> d; _ -> error "datum"))
  in amount old == amount new

-- ValidityRangeBound: invalid
timeWrongEndpoints :: TxInfo -> POSIXTime -> Bool
timeWrongEndpoints info maxDuration = case (I.ivFrom (txInfoValidRange info), I.ivTo (txInfoValidRange info)) of
  (I.LowerBound (I.Finite lo) _, I.UpperBound (I.Finite hi) _) -> hi - hi <= maxDuration

-- ValidityRangeBound: invalid
timeDeadGuard :: TxInfo -> POSIXTime -> POSIXTime -> Bool
timeDeadGuard info deadline maxDuration = case (I.ivFrom (txInfoValidRange info), I.ivTo (txInfoValidRange info)) of
  (I.LowerBound (I.Finite lo) _, I.UpperBound (I.Finite hi) _) -> let unused = hi - lo <= maxDuration in lo >= deadline
  _ -> False

-- ValidityRangeBound: invalid
timeDifferentRanges :: TxInfo -> TxInfo -> POSIXTime -> Bool
timeDifferentRanges info other maxDuration = case (I.ivFrom (txInfoValidRange info), I.ivTo (txInfoValidRange other)) of
  (I.LowerBound (I.Finite lo) _, I.UpperBound (I.Finite hi) _) -> hi - lo <= maxDuration
  _ -> False

-- ValidityRangeBound: valid
timeAliasGood :: TxInfo -> POSIXTime -> Bool
timeAliasGood info maxDuration = let r = txInfoValidRange info in case (I.ivFrom r, I.ivTo r) of
  (I.LowerBound (I.Finite lo) _, I.UpperBound (I.Finite hi) _) -> let duration = hi - lo in duration < maxDuration
  _ -> False

-- ZipWithoutLengthCheck: invalid
zipWithBad :: [Integer] -> [Integer] -> [Integer]
zipWithBad xs ys = zipWith (+) xs ys

-- ZipWithoutLengthCheck: valid
zipWithGood :: [Integer] -> [Integer] -> [Integer]
zipWithGood xs ys = if length xs == length ys then zipWith (+) xs ys else []

-- ZipWithoutLengthCheck: invalid
zipOrBypass :: [Integer] -> [Integer] -> Bool
zipOrBypass xs ys = (length xs == length ys || True) && all (\(a,b) -> a == b) (zip xs ys)

-- ZipWithoutLengthCheck: valid
zipMultilineGood :: [Integer] -> [Integer] -> Bool
zipMultilineGood xs ys = length
  xs == length
  ys && all (\(a,b) -> a == b) (zip xs ys)

-- ZipWithoutLengthCheck: invalid
zipSameBindingNames :: [Integer] -> [Integer] -> Bool
zipSameBindingNames xs ys = all (\(a,b) -> a == b) (zip xs ys)

-- MissingAddressValidation: invalid
addressDeadGuard :: TxOut -> Datum -> Address -> Bool
addressDeadGuard out d addr = let unused = txOutAddress out == addr in txOutDatum out == OutputDatum d

-- MissingAddressValidation: invalid
addressOrBypass :: TxOut -> Datum -> Address -> Bool
addressOrBypass out d addr = (txOutAddress out == addr || True) && txOutDatum out == OutputDatum d

-- UnvalidatedReferenceScript: invalid
referenceComment :: TxOut -> Address -> Bool
referenceComment out addr =
  -- txOutReferenceScript out == Nothing
  txOutAddress out == addr

-- UnvalidatedReferenceScript: invalid
referenceOther :: TxOut -> TxOut -> Address -> Bool
referenceOther out other addr = txOutAddress out == addr && txOutReferenceScript other == Nothing

-- TrashTokens: invalid
trashOtherBound :: TxOut -> TxOut -> Value -> Bool
trashOtherBound out other expected = txOutValue out `V.geq` expected && length (V.flattenValue (txOutValue other)) <= 2

-- IncompleteTokenValidation: invalid
tokenFilter :: TxInfo -> CurrencySymbol -> Bool
tokenFilter info cs = length (filter (\(sym, _, _) -> sym == cs) (V.flattenValue (txInfoMint info))) == 1

-- StrictValueEquality: invalid
strictAdaAlias :: TxOut -> Lovelace -> Bool
strictAdaAlias out n = let ada = V.lovelaceValueOf (txOutValue out) in ada == n

-- StrictValueEquality: valid
strictAdaDead :: TxOut -> Lovelace -> Bool
strictAdaDead out n = let unused = V.lovelaceValueOf (txOutValue out) == n in True

-- UnvalidatedInputIndex: invalid
indexDeadGuard :: TxInfo -> IndexRedeemer -> CurrencySymbol -> TokenName -> Address -> Bool
indexDeadGuard info r cs tn addr = let
  selected = txInfoInputs info !! fromInteger (inputIndex r)
  unused = V.valueOf (txOutValue (txInInfoResolved selected)) cs tn == 1
  in txOutAddress (txInInfoResolved selected) == addr

-- FixedStructureMap: valid
mapComment :: TestDatum -> Bool
mapComment d =
  -- member "fee" (extraInfo d)
  amount d > 0

-- HelperFunctions: invalid
helperFixedArg :: TxInfo -> Bool
helperFixedArg info = txSignedBy info "abc"

-- UnstableMakeIsData: additional contract boundary
spliceString :: String
spliceString = "unstableMakeIsData is not invoked"

-- UnstableMakeIsData: additional contract boundary
spliceComment :: Integer -> Integer
spliceComment x =
  {- nested {- unstableMakeIsData -} comment -}
  x

-- MissingStakingValidation: additional contract boundary
stakeWildcard :: TxOut -> ScriptHash -> Bool
stakeWildcard out sh = case txOutAddress out of
  Address (ScriptCredential h) _ -> h == sh
  _ -> False

-- MissingStakingValidation: additional contract boundary
stakePatternGood :: TxOut -> ScriptHash -> Bool
stakePatternGood out sh = case txOutAddress out of
  Address (ScriptCredential h) Nothing -> h == sh
  _ -> False

-- UnvalidatedDatum: additional contract boundary
datumHashWildcard :: TxOut -> Address -> Bool
datumHashWildcard out addr = txOutAddress out == addr && case txOutDatum out of
  OutputDatumHash _ -> True
  _ -> False

-- IncompleteTokenValidation: additional contract boundary
tokenFold :: TxInfo -> CurrencySymbol -> Bool
tokenFold info symbol = foldr (\(cs, _, _) acc -> cs == symbol && acc) True (V.flattenValue (txInfoMint info))

-- UnvalidatedInputIndex: additional contract boundary
indexConstant :: TxInfo -> Address -> Bool
indexConstant info addr = txOutAddress (txInInfoResolved (txInfoInputs info !! 0)) == addr

-- ValidityRangeBound: additional contract boundary
timeSeparateGood :: TxInfo -> POSIXTime -> Bool
timeSeparateGood info maximumDuration = let
  r = txInfoValidRange info
  lo = case I.ivFrom r of I.LowerBound (I.Finite t) _ -> t; _ -> error "lower"
  hi = case I.ivTo r of I.UpperBound (I.Finite t) _ -> t; _ -> error "upper"
  in hi - lo <= maximumDuration

-- UncheckedRedeemer: additional contract boundary
redeemerWrongTransaction :: TxInfo -> TxInfo -> Redeemer -> Bool
redeemerWrongTransaction info other expected = all (\i -> case addressCredential (txOutAddress (txInInfoResolved i)) of
  ScriptCredential _ -> M.lookup (Spending (txInInfoOutRef i)) (txInfoRedeemers other) == Just expected
  _ -> True) (txInfoInputs info)

-- Missing output constraints even when no other fields are checked.
addressNoFields :: TxOut -> Bool
addressNoFields _out = True

-- A predicate that always rejects cannot accept a redirected output.
addressRejectAll :: TxOut -> Bool
addressRejectAll _out = False
