{- |
Copyright: (c) 2026 IOHK
SPDX-License-Identifier: MPL-2.0

Teaching content for the PLU-STAN inspections, rendered by the
VS Code extension's finding detail panel. The analyzer binary is the
single source of truth for this content — the extension only renders it.
-}

module Stan.Plinth.Docs
    ( InspectionDocs (..)
    , lookupDocs
    , plinthDocsMap
    ) where

import Stan.Core.Id (Id (..))
import Stan.Inspection (Inspection)

import qualified Data.HashMap.Strict as HM


-- | Extended, Plinth-specific documentation for one inspection.
data InspectionDocs = InspectionDocs
    { docsWhyItMatters :: !Text
      -- ^ The on-chain rationale: what goes wrong and who exploits it.
    , docsBadExample   :: !Text
      -- ^ A short Haskell snippet showing the flagged pattern.
    , docsGoodExample  :: !Text
      -- ^ The corrected version of the same snippet.
    , docsAnchor       :: !Text
      -- ^ Anchor into RULES.md (e.g. \"equality\"); @\"\"@ if none.
    } deriving stock (Show, Eq)

lookupDocs :: Id Inspection -> Maybe InspectionDocs
lookupDocs insId = HM.lookup insId plinthDocsMap

plinthDocsMap :: HashMap (Id Inspection) InspectionDocs
plinthDocsMap = fromList
    [ ( Id "PLU-STAN-01"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "The signature builtins only prove that a signature matches a message and a"
              , "public key — they say nothing about which transaction the message authorizes."
              , "If the message is supplied by the redeemer and does not commit to this"
              , "specific spend (a nonce, the spent TxOutRef, a hash the script recomputes"
              , "on-chain), anyone who has seen one valid signature can replay it in a new"
              , "transaction and re-run the authorized action as often as they like."
              ]
          , docsBadExample = unlines
              [ "-- the redeemer supplies both message and signature: the same signed"
              , "-- blob authorizes this action in every future transaction (replay)"
              , "checkAuth (msg, sig) = verifyEd25519Signature ownerKey msg sig"
              ]
          , docsGoodExample = unlines
              [ "-- the message is rebuilt on-chain and commits to the spent UTxO,"
              , "-- so a captured signature is useless in any other transaction"
              , "checkAuth sig ownRef ="
              , "  let msg = serialiseData (toBuiltinData ownRef)"
              , "  in verifyEd25519Signature ownerKey msg sig"
              ]
          , docsAnchor = "data-handling--deserialization"
          }
      )
    , ( Id "PLU-STAN-02"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "'unsafeFromBuiltinData' eagerly decodes the whole BuiltinData blob into a"
              , "sums-of-products value. An attacker who controls the datum can attach a huge"
              , "or deeply nested blob so that decoding alone blows the execution budget and"
              , "the script can never run — an unbounded datum spam attack. Even on honest"
              , "data the full SOP conversion is one of the most expensive things a Plinth"
              , "script can do; inspecting only the needed fields on the raw BuiltinData is"
              , "far cheaper."
              ]
          , docsBadExample = unlines
              [ "-- decodes the entire datum, however large the attacker made it"
              , "validate d _redeemer _ctx ="
              , "  let LoanDatum{owner, amount} = unsafeFromBuiltinData d"
              , "  in checkTerms owner amount"
              ]
          , docsGoodExample = unlines
              [ "-- walk the BuiltinData directly and touch only the needed fields"
              , "-- (BI = PlutusTx.Builtins.Internal)"
              , "validate d _redeemer _ctx ="
              , "  let fields = BI.snd (BI.unsafeDataAsConstr d)"
              , "      owner  = BI.unsafeDataAsB (BI.head fields)"
              , "      amount = BI.unsafeDataAsI (BI.head (BI.tail fields))"
              , "  in checkTerms owner amount"
              ]
          , docsAnchor = "data-handling--deserialization"
          }
      )
    , ( Id "PLU-STAN-03"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "Every 'Maybe' produced on-chain allocates a 'Just'/'Nothing' constructor that"
              , "is usually deconstructed immediately afterwards, paying execution units for"
              , "nothing. Worse, defaulting with 'fromMaybe' silently substitutes a value in"
              , "exactly the cases where a validator should reject the transaction, so a"
              , "\"not found\" turns into a successful validation with a bogus value. Explicit"
              , "fast-fail matching (or continuation-passing) is both cheaper and safer."
              ]
          , docsBadExample = unlines
              [ "-- a missing value silently becomes 0 and validation \"succeeds\" on garbage"
              , "total = fromMaybe 0 (lookupValue token outputs)"
              ]
          , docsGoodExample = unlines
              [ "-- fail fast: a missing value must reject the transaction"
              , "-- (traceError is safe inside a case branch; Plinth's strict application"
              , "--  means it would fire eagerly in an argument position like fromMaybe's)"
              , "total = case lookupValue token outputs of"
              , "  Nothing -> traceError \"token value missing\""
              , "  Just v  -> v"
              ]
          , docsAnchor = "optional-types"
          }
      )
    , ( Id "PLU-STAN-04"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "When the hash is drawn from an output's address, comparing only a"
              , "PubKeyHash, ScriptHash, or Credential checks the payment part of that"
              , "address and ignores the staking part. An attacker can construct an output"
              , "that passes the credential check while redirecting the staking rewards of"
              , "the locked value to their own stake key — staking value theft. The rule"
              , "also fires on legitimate uses (e.g. 'txInfoSignatories' membership checks);"
              , "those can be reviewed and suppressed."
              ]
          , docsBadExample = unlines
              [ "-- only the payment credential is compared"
              , "paysToOwner out = addressCredential (txOutAddress out) == ownerCredential"
              ]
          , docsGoodExample = unlines
              [ "-- the full Address (payment + staking) is compared"
              , "paysToOwner out = txOutAddress out == ownerAddress"
              ]
          , docsAnchor = "equality"
          }
      )
    , ( Id "PLU-STAN-05"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "Higher-order helpers such as 'all', 'any', 'find', 'filter', and 'foldr'"
              , "compile to UPLC that builds and applies a closure for the predicate on every"
              , "element, and the Foldable-polymorphic versions add dictionary overhead on"
              , "top. Those execution units are paid by every user on every invocation of"
              , "your validator, and on large input/output lists they can push a legitimate"
              , "transaction over the execution budget. A specialized recursive function"
              , "inlines the predicate and eliminates the closure traffic."
              ]
          , docsBadExample = unlines
              [ "-- 'any' applies the lambda closure to each element"
              , "paidEnough outs = any (\\o -> txOutValue o `geq` price) outs"
              ]
          , docsGoodExample = unlines
              [ "-- specialized recursion: the predicate is inlined into the loop"
              , "paidEnough = go"
              , "  where"
              , "    go []       = False"
              , "    go (o : os) = if txOutValue o `geq` price then True else go os"
              ]
          , docsAnchor = "higher-order-functions"
          }
      )
    , ( Id "PLU-STAN-06"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "Composing traversals such as 'map' over 'filter' walks the list twice and"
              , "materializes an intermediate list in between; UPLC has no fusion, so every"
              , "extra pass and every intermediate cons cell is billed as CPU and memory"
              , "execution units. On transaction input/output lists this multiplies the cost"
              , "of one of the hottest paths in a validator. Fusing the passes into a single"
              , "recursive function does the same work in one traversal with no intermediate"
              , "allocation."
              ]
          , docsBadExample = unlines
              [ "-- two passes plus an intermediate list"
              , "scriptOuts = map txOutValue (filter isScriptOut (txInfoOutputs info))"
              ]
          , docsGoodExample = unlines
              [ "-- one fused pass, no intermediate list"
              , "scriptOuts = go (txInfoOutputs info)"
              , "  where"
              , "    go []       = []"
              , "    go (o : os) ="
              , "      if isScriptOut o then txOutValue o : go os else go os"
              ]
          , docsAnchor = "higher-order-functions"
          }
      )
    , ( Id "PLU-STAN-07"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "GHC desugars guards into nested pattern-match fall-through chains, and the"
              , "Plinth compiler turns those into deeper, more expensive UPLC than a plain"
              , "if-then-else expresses. The redundant match scaffolding costs execution"
              , "units on every single invocation of the validator — a pure waste, since the"
              , "same logic written with explicit conditionals compiles to a flat, cheap"
              , "chain of 'ifThenElse' calls."
              ]
          , docsBadExample = unlines
              [ "tier n"
              , "  | n <= 10   = 1"
              , "  | n <= 100  = 2"
              , "  | otherwise = 3"
              ]
          , docsGoodExample = unlines
              [ "tier n ="
              , "  if n <= 10 then 1"
              , "  else if n <= 100 then 2"
              , "  else 3"
              ]
          , docsAnchor = "guards"
          }
      )
    , ( Id "PLU-STAN-08"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "In Plinth a non-strict let binding is compiled as a delayed computation, so"
              , "a binding that is referenced several times can be re-evaluated at every use"
              , "site in the generated UPLC. If the bound expression is expensive — say,"
              , "summing 'valueSpent' over all inputs — you pay its full cost once per"
              , "reference instead of once per transaction. A bang pattern forces one shared"
              , "evaluation up front."
              ]
          , docsBadExample = unlines
              [ "-- 'total' may be recomputed at each of its two use sites"
              , "let total = valueSpent info"
              , "in total `geq` minDeposit && total `geq` minCollateral"
              ]
          , docsGoodExample = unlines
              [ "-- forced once, shared by both checks"
              , "let !total = valueSpent info"
              , "in total `geq` minDeposit && total `geq` minCollateral"
              ]
          , docsAnchor = "bindings"
          }
      )
    , ( Id "PLU-STAN-09"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "'valueOf' inspects a single currency/token entry, so a comparison built on"
              , "it says nothing about the rest of the Value. An output can satisfy"
              , "'valueOf out cs tn >= n' while also carrying any number of unexpected dust"
              , "tokens (a dust token attack when the token set is unbounded), and"
              , "lovelace-only checks silently depend on the min-UTxO requirement, which is a"
              , "protocol parameter that changes over time. Compare whole values, or bound"
              , "the token set explicitly."
              ]
          , docsBadExample = unlines
              [ "-- passes even if the output is stuffed with arbitrary extra tokens"
              , "paidOut out = valueOf (txOutValue out) cs tn == amount"
              ]
          , docsGoodExample = unlines
              [ "-- compare the full expected value: nothing unexpected can ride along"
              , "paidOut out = txOutValue out == expectedValue"
              ]
          , docsAnchor = "value-handling"
          }
      )
    , ( Id "PLU-STAN-10"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "'unsafeFromBuiltinData' checks only the shape of the encoding, not the"
              , "ledger's invariants — e.g. that a credential hash is exactly 28 bytes. A"
              , "user-supplied Address or PubKeyHash can be structurally valid yet impossible"
              , "for the ledger to ever produce in a real output, so an equality check"
              , "against it becomes unsatisfiable. In a lending validator this is fatal: the"
              , "lender supplies a bogus repayment address at loan creation, the borrower can"
              , "never build a repayment transaction, and their collateral is guaranteed to"
              , "be liquidated."
              ]
          , docsBadExample = unlines
              [ "-- repayAddr came from attacker-supplied data; if its hash is not"
              , "-- 28 bytes, no real output can ever satisfy this equality"
              , "LoanDatum{repayAddr} = unsafeFromBuiltinData datum"
              , "repaid out = txOutAddress out == repayAddr"
              ]
          , docsGoodExample = unlines
              [ "-- validate ledger invariants (hash lengths, well-formed staking part)"
              , "-- on the raw BuiltinData before trusting the address in a check"
              , "repaid out ="
              , "  isBuiltinAddress repayAddrData  -- e.g. checks 28-byte credential hash"
              , "    && txOutAddress out == repayAddr"
              ]
          , docsAnchor = "data-handling--deserialization"
          }
      )
    , ( Id "PLU-STAN-11"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "'currencySymbolValueOf' sums every token amount under a currency symbol"
              , "without requiring the entries to have a uniform sign. A burn-only check like"
              , "'currencySymbolValueOf minted ownCS < 0' is satisfied by a transaction that"
              , "burns 2 TokenA while minting 1 TokenB under the same symbol — the sum is"
              , "negative, yet the attacker just minted an unauthorized token through your"
              , "Burn redeemer."
              ]
          , docsBadExample = unlines
              [ "-- passes with txInfoMint = [(TokenA, -2), (TokenB, 1)]: TokenB is minted!"
              , "Burn -> currencySymbolValueOf (txInfoMint info) ownCS < 0"
              ]
          , docsGoodExample = unlines
              [ "-- every amount under our symbol must be strictly negative"
              , "Burn -> allBurns (tokenAmounts (txInfoMint info) ownCS)"
              , "  where"
              , "    allBurns []       = True"
              , "    allBurns (n : ns) = n < 0 && allBurns ns"
              ]
          , docsAnchor = "value-handling"
          }
      )
    , ( Id "PLU-STAN-12"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "A transaction's validity range is chosen by whoever builds the transaction,"
              , "and by default it is unbounded on both sides. Interval helpers like 'from',"
              , "'to', 'always', or 'contains' make it easy to write a time check that an"
              , "unbounded range satisfies — e.g. \"not before the deadline\" logic that a"
              , "range of [now, +inf) passes even when submitted long before the deadline,"
              , "letting vesting or auction funds be claimed early. Always require the"
              , "relevant bound of 'txInfoValidRange' to be 'Finite' before comparing it."
              ]
          , docsBadExample = unlines
              [ "-- an open-ended range [now, +inf) extends past the deadline, so this"
              , "-- passes even for a transaction submitted before the deadline"
              , "deadlinePassed = not (to deadline `contains` txInfoValidRange info)"
              ]
          , docsGoodExample = unlines
              [ "-- require a Finite lower bound and compare it against the deadline"
              , "deadlinePassed = case ivFrom (txInfoValidRange info) of"
              , "  LowerBound (Finite t) _ -> t > deadline"
              , "  _                       -> traceError \"unbounded validity range\""
              ]
          , docsAnchor = "validity-interval--posix-time-misuse"
          }
      )
    , ( Id "PLU-STAN-13"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "A validator that pins down an output's address, value, and datum but never"
              , "constrains 'txOutReferenceScript' lets the transaction builder attach an"
              , "arbitrary reference script to that output. That silently raises the"
              , "output's min-ADA requirement and plants attacker-chosen code on a UTxO your"
              , "protocol treats as trusted — off-chain code or downstream validators that"
              , "resolve reference scripts from your outputs can be steered to the"
              , "attacker's script. Assert the expected policy (usually 'Nothing')"
              , "explicitly, or document why it is irrelevant."
              ]
          , docsBadExample = unlines
              [ "okOutput out ="
              , "  txOutAddress out == vaultAddr"
              , "    && txOutValue out == expectedValue"
              , "    && txOutDatum out == expectedDatum"
              , "    -- txOutReferenceScript is never constrained"
              ]
          , docsGoodExample = unlines
              [ "okOutput out ="
              , "  txOutAddress out == vaultAddr"
              , "    && txOutValue out == expectedValue"
              , "    && txOutDatum out == expectedDatum"
              , "    && txOutReferenceScript out == Nothing"
              ]
          , docsAnchor = ""
          }
      )
    , ( Id "PLU-STAN-14"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "Checking several fields of an output while matching only the payment part"
              , "of its address leaves the staking credential attacker-chosen. The output"
              , "still \"pays to the script\" as far as the payment credential is concerned,"
              , "but the staking rewards earned by all value sitting at that output flow to"
              , "the attacker's stake key — staking value theft against the funds your"
              , "contract is supposed to protect. Compare the full 'Address', or assert the"
              , "staking credential explicitly."
              ]
          , docsBadExample = unlines
              [ "okOutput out ="
              , "  txOutValue out == expectedValue"
              , "    && addressCredential (txOutAddress out) == vaultCredential"
              , "    -- the staking part of the address is unconstrained"
              ]
          , docsGoodExample = unlines
              [ "okOutput out ="
              , "  txOutValue out == expectedValue"
              , "    && txOutAddress out == vaultAddress  -- payment AND staking parts"
              ]
          , docsAnchor = ""
          }
      )
    , ( Id "PLU-STAN-15"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "If a validator checks an output's address and datum but never constrains"
              , "'txOutValue', the transaction builder decides how much value that output"
              , "carries. The classic exploit is a continuing-output check: the attacker"
              , "re-locks the vault with a dust amount and the expected datum, satisfies the"
              , "validator, and walks away with the difference. Every security-relevant"
              , "output needs an explicit value constraint — at minimum the required assets"
              , "and amounts."
              ]
          , docsBadExample = unlines
              [ "okOutput out ="
              , "  txOutAddress out == vaultAddr"
              , "    && txOutDatum out == expectedDatum"
              , "    -- value is unconstrained: a dust output satisfies this"
              ]
          , docsGoodExample = unlines
              [ "okOutput out ="
              , "  txOutAddress out == vaultAddr"
              , "    && txOutDatum out == expectedDatum"
              , "    && txOutValue out == expectedValue"
              ]
          , docsAnchor = ""
          }
      )
    , ( Id "PLU-STAN-16"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "On-chain arithmetic is integer-only, and 'divide' truncates. Dividing before"
              , "multiplying throws the remainder away before it can be scaled back up, so"
              , "results are systematically too small — and an attacker picks the amounts."
              , "A fee computed as '(amount `divide` 10000) * rate' is exactly 0 for any"
              , "amount below 10000, letting users transact fee-free by splitting into small"
              , "amounts. Multiplying first keeps full precision until the single final"
              , "division."
              ]
          , docsBadExample = unlines
              [ "-- amount = 9999 gives fee 0 for any rate: rounding happens too early"
              , "fee amount rate = (amount `divide` 10000) * rate"
              ]
          , docsGoodExample = unlines
              [ "-- multiply first: rounding happens once, at the very end"
              , "fee amount rate = (amount * rate) `divide` 10000"
              ]
          , docsAnchor = "integers"
          }
      )
    , ( Id "PLU-STAN-17"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "Redeemers often carry indices into the inputs or outputs list to spare the"
              , "script an on-chain search. If the validator does not enforce that those"
              , "indices are pairwise distinct, an attacker submits the same index twice and"
              , "one honest element is validated multiple times — the double-satisfaction"
              , "attack, e.g. a single payment output counted against two separate"
              , "obligations. Enforce uniqueness (strictly increasing indices, or a bitmask)"
              , "or select elements by stable identifiers such as 'TxOutRef' instead."
              ]
          , docsBadExample = unlines
              [ "-- redeemer [2, 2] makes one output satisfy two claims"
              , "checkClaims is = go is"
              , "  where"
              , "    go []         = True"
              , "    go (i : rest) = paysClaim (txInfoOutputs info !! i) && go rest"
              ]
          , docsGoodExample = unlines
              [ "-- strictly increasing indices are necessarily unique"
              , "checkClaims is = go (negate 1) is"
              , "  where"
              , "    go _prev []         = True"
              , "    go prev  (i : rest) ="
              , "      i > prev && paysClaim (txInfoOutputs info !! i) && go i rest"
              ]
          , docsAnchor = ""
          }
      )
    , ( Id "PLU-STAN-18"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "Plinth compiles the lazy '(&&)' with delay/force wrappers so that the right"
              , "operand is only evaluated when the left one is 'True'. In the predicate of"
              , "a branch whose failure case immediately errors, that laziness buys nothing"
              , "— the transaction is rejected either way — but the extra delay/force nodes"
              , "are executed and billed on every validation. A strict combinator built on"
              , "'ifThenElse' avoids the overhead. Keep '(&&)' only when the right-hand side"
              , "deliberately throws (e.g. 'traceError') and short-circuiting matters."
              ]
          , docsBadExample = unlines
              [ "-- lazy (&&) adds delay/force overhead; the failure branch errors anyway"
              , "if signedByOwner && deadlineOk then () else traceError \"bad tx\""
              ]
          , docsGoodExample = unlines
              [ "{-# INLINE builtinAnd #-}"
              , "builtinAnd :: Bool -> Bool -> Bool"
              , "builtinAnd b1 b2 = BI.ifThenElse b1 b2 False"
              , ""
              , "if signedByOwner `builtinAnd` deadlineOk then () else traceError \"bad tx\""
              ]
          , docsAnchor = ""
          }
      )
    , ( Id "PLU-STAN-19"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "An output re-locked at a script address carries the contract's state in its"
              , "datum. If the validator checks address and value but leaves the datum"
              , "unconstrained, the transaction builder writes whatever state they like into"
              , "the continuing output — changing the recorded owner, price, or debt — or"
              , "attaches a malformed datum the validator can never parse, permanently"
              , "bricking the UTxO and the funds it holds. Pin down the datum shape and its"
              , "security-critical fields for every continuing output."
              ]
          , docsBadExample = unlines
              [ "okOutput out ="
              , "  txOutAddress out == vaultAddr"
              , "    && txOutValue out == expectedValue"
              , "    -- datum unconstrained: the attacker rewrites the vault state"
              ]
          , docsGoodExample = unlines
              [ "okOutput out ="
              , "  txOutAddress out == vaultAddr"
              , "    && txOutValue out == expectedValue"
              , "    && txOutDatum out == OutputDatum (Datum (toBuiltinData newState))"
              ]
          , docsAnchor = ""
          }
      )
    , ( Id "PLU-STAN-21"
      , InspectionDocs
          { docsWhyItMatters = unlines
              [ "A credential compiled into a validator is a key you can never rotate. A"
              , "script\'s address derives from its hash, so replacing the constant -- or the"
              , "value applied to the compiled code via applyCode -- produces a *different*"
              , "script at a *different* address. If that key is lost every UTxO it guards is"
              , "frozen; if it is compromised the attacker keeps that authority until you"
              , "deploy a new script and migrate every locked UTxO to it. Holding the"
              , "credential in datum instead lets a governance action replace it in one"
              , "transaction, with no redeploy and no migration."
              ]
          , docsBadExample = unlines
              [ "-- baked in two ways: as a top-level constant, and specialised into the"
              , "-- compiled validator, so neither can be changed after deployment"
              , "adminKey :: PubKeyHash"
              , "adminKey = PubKeyHash \"a1b2c3...\""
              , ""
              , "validator = $$(compile [|| mkValidator ||]) `unsafeApplyCode` liftCodeDef adminKey"
              ]
          , docsGoodExample = unlines
              [ "-- the authority lives in the datum and can be rotated in place"
              , "data VaultDatum = VaultDatum { vaultAdmin :: PubKeyHash }"
              , ""
              , "signedByAdmin d info = vaultAdmin d `elem` txInfoSignatories info"
              ]
          , docsAnchor = ""
          }
      )
    ]
