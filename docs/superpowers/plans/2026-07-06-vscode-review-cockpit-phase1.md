# Plu-Stan Review Cockpit — Phase 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Turn the vscode-plustan MVP into a review-session cockpit: JSON schema v2 with stable fingerprints and rule docs in the backend; session state machine, findings tree, detail panel, dismissals, staleness, and auto-rerun-on-save in the extension.

**Architecture:** The Haskell CLI stays a stateless spawn-per-request analyzer; all new payload assembly lives in testable library modules (`Stan.Plinth.*`). The TypeScript extension is restructured around a vscode-free `core/` (pure reducers, parsers — unit-tested with mocha) and a thin vscode-coupled shell (controller, tree, webview, diagnostics). All analyzer traffic goes through one `AnalyzerClient` interface.

**Tech Stack:** Haskell (GHC 9.6.x, cabal, hspec, microaeson, relude prelude in `src/`), TypeScript 5.5 / VS Code extension API 1.90, mocha for unit tests, @vscode/test-electron for one integration smoke test.

**Spec:** `docs/superpowers/specs/2026-07-06-vscode-review-cockpit-design.md`

**Branch:** all work happens on `feature/vscode-review-cockpit` (already created; the spec is committed there).

---

## Context for a fresh engineer

- The repo is a fork of kowainik/stan. The `plustan` executable is `app/PluStan.hs` (cabal stanza `executable plustan`, `stan.cabal:198`). There is also `executable stan` (upstream CLI) — do not touch it.
- `src/` modules use the **relude** prelude (via the cabal `common-relude` mixin): `Text`, `HashMap`, `fromList`, `toList`, `sortOn`, `foldl'`, `NonEmpty` are in scope unqualified; partial `head/init/last` on lists are not available.
- JSON is **microaeson** (`Data.Aeson.Micro`), not aeson: it has `Value`, `object`, `(.=)`, `encode`, `ToJSON`. Its `Object` is a `Map`, so `encode` output has **alphabetically sorted keys** (deterministic — good for golden tests). There is no `FromJSON`/decoding.
- Backend tests: hspec, entry `test/Spec.hs` (suite `stan-test`). `test/Spec.hs` builds `.hie` files for the repo itself on first run (slow once, then cached). Run with `cabal test` (needs `secp256k1`, `sodium`, `blst` installed — the dev machine has them).
- The extension is `vscode-plustan/` (TypeScript, `tsc` only, `rootDir: src`, `outDir: out`, currently two files: `extension.ts` 901 lines, `downloadManager.ts`). It currently reads schema-v1 payloads: `analyze --json` → `{version:1, inspections, analysis:{observations}}`.
- The current v1 `Observation` JSON id (`OBS-PLU-STAN-XX-<hash6>-line:col`, built by `mkObservationId` in `src/Stan/Observation.hs:269`) embeds line:col — that is exactly what fingerprints replace for identity purposes.
- `Observation` carries the **full source file** in `observationFileContent :: ByteString`, and the flagged span in `observationSrcSpan :: RealSrcSpan` (accessors `srcSpanStartLine/StartCol/EndLine/EndCol` from `Stan.Ghc.Compat`). GHC end columns are exclusive (one past the last character).

## File structure

**Backend — create:**
- `src/Stan/Plinth/Docs.hs` — per-rule teaching content (`InspectionDocs`, `lookupDocs`)
- `src/Stan/Plinth/Payload.hs` — schema-v2 payload assembly (pure, golden-testable)
- `test/Test/Stan/Plinth.hs` — hspec specs for both new modules

**Backend — modify:**
- `src/Stan/Observation.hs` — add `observationSpanText`, `observationFingerprint`, generalize `hashModuleName`
- `test/Test/Stan/Observation.hs` — fingerprint specs
- `app/PluStan.hs` — `capabilities` subcommand, emit v2 analyze payload
- `stan.cabal` — expose new modules, add test module, bump version to `0.2.5.0`
- `test/Spec.hs` — register new spec
- `CHANGELOG.md`

**Extension — create (under `vscode-plustan/src/`):**
- `core/schema.ts` — v2 payload types + validation (no vscode imports)
- `core/schema.test.ts`
- `core/sessionState.ts` — pure session reducer (no vscode imports)
- `core/sessionState.test.ts`
- `core/dismissals.ts` — pure dismissals-file logic (no vscode imports)
- `core/dismissals.test.ts`
- `core/runCoalescer.ts` — pending-run queue logic (no vscode imports)
- `core/runCoalescer.test.ts`
- `analyzer/client.ts` — `AnalyzerClient` + `SpawnAnalyzerClient` (no vscode imports; uses `AbortSignal`)
- `analyzer/client.test.ts`
- `session/controller.ts` — vscode-coupled orchestration
- `session/dismissalsStore.ts` — vscode-coupled file I/O for `.plustan/dismissals.json`
- `ui/findingsTree.ts`, `ui/detailPanel.ts`, `ui/statusBar.ts`
- `diagnostics.ts` — session-state → DiagnosticCollection + dismiss code action
- `test/runTest.ts`, `test/suite/index.ts`, `test/suite/session.test.ts` — integration smoke

**Extension — create (outside `src/`):**
- `vscode-plustan/test-fixtures/fake-plustan.js`, `test-fixtures/fake-plustan` (shim), `test-fixtures/workspace/` — stub binary + fixture workspace

**Extension — modify:**
- `src/extension.ts` — becomes thin wiring; spawn/JSON plumbing moves to `analyzer/client.ts`
- `package.json` — views, commands, menus, scripts, devDependencies, version 0.3.0
- `README.md`, `MARKETPLACE.md`

**Hard rule:** files under `core/` and `analyzer/` MUST NOT import `vscode`. That is what makes them mocha-testable without an extension host.

---

### Task 1: Observation fingerprints (backend)

**Files:**
- Modify: `src/Stan/Observation.hs`
- Test: `test/Test/Stan/Observation.hs`

- [ ] **Step 1: Write the failing tests**

Replace the whole body of `test/Test/Stan/Observation.hs` with (the existing `observationSpec` id test is kept):

```haskell
module Test.Stan.Observation
    ( observationSpec
    ) where

import Stan.Ghc.Compat (mkRealSrcLoc, mkRealSrcSpan)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import Stan.Core.Id (Id (..))
import Stan.Inspection (inspectionId)
import Stan.Inspection.Partial (stan0001)
import Stan.Observation (Observation (..), mkObservationId, observationFingerprint,
                         observationSpanText)

import qualified Data.ByteString.Char8 as BS8


observationSpec :: Spec
observationSpec = describe "Observation" $ do
    it "calculates Observation Id properly" $
        testObservationId `shouldBe` Id "OBS-STAN-0001-bbjjJQ-10:42"

    describe "observationSpanText" $ do
        -- line 2 is "bad expr here"; cols 5–14 (GHC end col is exclusive)
        -- select the 9 characters "expr here"
        it "extracts a single-line span, column-trimmed" $
            observationSpanText (mkObs 2 5 14 sampleContent)
                `shouldBe` "expr here"
        it "extracts a multi-line span" $
            observationSpanText (mkObs2 2 5 3 6 sampleContent)
                `shouldBe` "expr here\ny = 2"

    describe "observationFingerprint" $ do
        it "is stable when the same flagged text moves to another line" $
            observationFingerprint (mkObs 2 5 14 sampleContent)
                `shouldBe` observationFingerprint (mkObs 3 5 14 shiftedContent)
        it "changes when the flagged text changes" $
            observationFingerprint (mkObs 2 5 14 sampleContent)
                `shouldNotBe` observationFingerprint (mkObs 2 5 14 editedContent)
  where
    testObservationId :: Id Observation
    testObservationId = mkObservationId
        (inspectionId stan0001)
        "Test.Module.Name"
        $ mkRealSrcSpan
            (mkRealSrcLoc "src/Test/Module/Name.hs" 10 42)
            (mkRealSrcLoc "src/Test/Module/Name.hs" 10 42)

    -- line 1: "x = 1", line 2: "bad expr here", line 3: "y = 2"
    sampleContent, shiftedContent, editedContent :: ByteString
    sampleContent  = BS8.pack "x = 1\nbad expr here\ny = 2\n"
    -- same flagged text, one line lower (a comment was added above)
    shiftedContent = BS8.pack "-- c\nx = 1\nbad expr here\ny = 2\n"
    -- the flagged text itself changed
    editedContent  = BS8.pack "x = 1\nbad EXPR here\ny = 2\n"

    mkObs :: Int -> Int -> Int -> ByteString -> Observation
    mkObs line startC endC = mkObs2 line startC line endC

    mkObs2 :: Int -> Int -> Int -> Int -> ByteString -> Observation
    mkObs2 startL startC endL endC content = Observation
        { observationId = Id "test-obs"
        , observationInspectionId = inspectionId stan0001
        , observationSrcSpan = mkRealSrcSpan
            (mkRealSrcLoc "src/T.hs" startL startC)
            (mkRealSrcLoc "src/T.hs" endL endC)
        , observationFile = "src/T.hs"
        , observationModuleName = "Test.Module.Name"
        , observationFileContent = content
        }
```

- [ ] **Step 2: Run tests to verify they fail to compile**

Run: `cabal build stan-test 2>&1 | tail -20`
Expected: error — `observationFingerprint`/`observationSpanText` not exported from `Stan.Observation`.

- [ ] **Step 3: Implement in `src/Stan/Observation.hs`**

Add `observationFingerprint` and `observationSpanText` to the module export list. Generalize the hash helper (bottom of file, next to `hashModuleName`):

```haskell
-- | SHA1 + base64, truncated to @n@ characters.
hashText :: Int -> Text -> Text
hashText n =
    Text.take n
    . extractBase64
    . Base64.encodeBase64
    . SHA1.hash
    . encodeUtf8

hashModuleName :: ModuleName -> Text
hashModuleName = hashText 6 . unModuleName
```

Add (near `mkObservationId`):

```haskell
{- | The source text of the flagged span, column-trimmed on the first
and last line. GHC end columns are exclusive. Missing lines (file
content shorter than the span) contribute @""@.
-}
observationSpanText :: Observation -> Text
observationSpanText Observation{..} =
    Text.intercalate "\n" (trimCols spanLines)
  where
    startL, endL, startC, endC :: Int
    startL = srcSpanStartLine observationSrcSpan
    endL   = srcSpanEndLine observationSrcSpan
    startC = srcSpanStartCol observationSrcSpan
    endC   = srcSpanEndCol observationSrcSpan

    spanLines :: [Text]
    spanLines =
        [ maybe "" decodeUtf8 (BS.lines observationFileContent !!? (ln - 1))
        | ln <- [startL .. endL]
        ]

    trimCols :: [Text] -> [Text]
    trimCols [] = []
    trimCols [single] =
        [Text.take (endC - startC) (Text.drop (startC - 1) single)]
    trimCols (firstLine : rest) =
        Text.drop (startC - 1) firstLine : trimLast rest

    trimLast :: [Text] -> [Text]
    trimLast [] = []
    trimLast [lastLine] = [Text.take (endC - 1) lastLine]
    trimLast (l : ls) = l : trimLast ls

{- | Position-independent identity for an 'Observation':

@
FPR-<INSPECTION-ID>-<module-hash-6>-<span-text-hash-10>
@

Stable under edits elsewhere in the file; changes when the flagged
expression itself changes (which correctly forces re-triage).
-}
observationFingerprint :: Observation -> Text
observationFingerprint o = Text.intercalate "-"
    [ "FPR"
    , unId (observationInspectionId o)
    , hashModuleName (observationModuleName o)
    , hashText 10 (observationSpanText o)
    ]
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cabal test --test-show-details=direct 2>&1 | tail -30`
Expected: all `Observation` specs PASS (first run may take minutes while `.hie` files build).

- [ ] **Step 5: Commit**

```bash
git add src/Stan/Observation.hs test/Test/Stan/Observation.hs
git commit -m "feat(backend): position-independent observation fingerprints"
```

---

### Task 2: Plinth rule docs module (backend)

**Files:**
- Create: `src/Stan/Plinth/Docs.hs`
- Create: `test/Test/Stan/Plinth.hs`
- Modify: `stan.cabal` (library `exposed-modules`, test-suite `other-modules`), `test/Spec.hs`

- [ ] **Step 1: Write the failing test**

Create `test/Test/Stan/Plinth.hs`:

```haskell
module Test.Stan.Plinth
    ( plinthSpec
    ) where

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

import Stan.Core.Id (Id (..))
import Stan.Inspection.All (inspectionsMap)
import Stan.Plinth.Docs (InspectionDocs (..), lookupDocs)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text


plinthSpec :: Spec
plinthSpec = describe "Stan.Plinth.Docs" $ do
    it "covers every PLU-STAN inspection" $ do
        let plinthIds = filter (Text.isPrefixOf "PLU-STAN" . unId) (HM.keys inspectionsMap)
        let missing = filter (isNothing . lookupDocs) plinthIds
        missing `shouldBe` []
    it "has non-empty teaching content everywhere" $
        case lookupDocs (Id "PLU-STAN-04") of
            Nothing -> expectationFailure "no docs for PLU-STAN-04"
            Just InspectionDocs{..} -> do
                docsWhyItMatters `shouldSatisfy` (not . Text.null)
                docsBadExample `shouldSatisfy` (not . Text.null)
                docsGoodExample `shouldSatisfy` (not . Text.null)
```

Register it: in `test/Spec.hs` add `import Test.Stan.Plinth (plinthSpec)` next to the other `Test.Stan.*` imports, and add `plinthSpec` to the `hspec $ do` block (after `observationSpec`). In `stan.cabal` add `Test.Stan.Plinth` to the `stan-test` `other-modules` list.

- [ ] **Step 2: Run to verify failure**

Run: `cabal build stan-test 2>&1 | tail -5`
Expected: error — `Stan.Plinth.Docs` not found.

- [ ] **Step 3: Create `src/Stan/Plinth/Docs.hs`**

Add `Stan.Plinth.Docs` to the library `exposed-modules` in `stan.cabal` (the list starting at `stan.cabal:101`; insert alphabetically, after the `Stan.Pattern.*` entries and before `Stan.Report`). Module skeleton with the first entry written out in full:

```haskell
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
    [ ( Id "PLU-STAN-04"
      , InspectionDocs
          { docsWhyItMatters = Text.unlines
              [ "Comparing only a PubKeyHash, ScriptHash, or Credential checks the payment"
              , "part of an address and ignores the staking part. An attacker can construct"
              , "an output that passes the credential check while redirecting the staking"
              , "rewards of the locked value to their own stake key — staking value theft."
              ]
          , docsBadExample = Text.unlines
              [ "-- only the payment credential is compared"
              , "paysToOwner out = addressCredential (txOutAddress out) == ownerCredential"
              ]
          , docsGoodExample = Text.unlines
              [ "-- the full Address (payment + staking) is compared"
              , "paysToOwner out = txOutAddress out == ownerAddress"
              ]
          , docsAnchor = "equality"
          }
      )
    -- … one entry per PLU-STAN rule, see Step 3a …
    ]
```

(`Text.unlines` needs `import qualified Data.Text as Text`; add it.)

- [ ] **Step 3a: Fill in the remaining rules**

Add an entry for **every** inspection ID that the completeness test discovers (all `PLU-STAN-*` in `inspectionsMap` — currently 01–12 and 16, plus any added since; also 13/14/15 pattern-rules if present in `antiPatternInspectionsMap`, check `src/Stan/Inspection/AntiPattern.hs`). Content sources, in priority order:

1. The rule's section in `RULES.md` (repo root) — `docsAnchor` is that section's GitHub heading anchor. Mapping of rule → RULES.md section: 02, 10 → *Data Handling & Deserialization* (`data-handling--deserialization`); 09, 11 → *Value Handling* (`value-handling`); 04 → *Equality* (`equality`); 03 → *Optional Types* (`optional-types`); 05, 06 → *Higher-Order Functions* (`higher-order-functions`); 08 → *Bindings* (`bindings`); 07 → *Guards* (`guards`); 16 → *Integers* (`integers`); 12 → *Validity Interval / POSIX Time Misuse* (`validity-interval--posix-time-misuse`).
2. The rule's row in the README.md rules table and its `descriptionL`/`solutionL` text in `src/Stan/Inspection/AntiPattern.hs` — paraphrase into `docsWhyItMatters`.
3. For rules with no RULES.md section (e.g. 01): write `docsWhyItMatters` from the inspection's description and solutions, invent a minimal bad/good snippet from the names the rule matches (they are listed in the rule's `NameMeta` values in `AntiPattern.hs`), and set `docsAnchor = ""`.

Every entry must have non-empty `docsWhyItMatters`, `docsBadExample`, `docsGoodExample` (the shape shown for PLU-STAN-04 above). No entry may be a stub.

- [ ] **Step 4: Run tests**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: `Stan.Plinth.Docs` specs PASS (completeness test proves no rule was skipped).

- [ ] **Step 5: Commit**

```bash
git add src/Stan/Plinth/Docs.hs test/Test/Stan/Plinth.hs test/Spec.hs stan.cabal
git commit -m "feat(backend): per-rule Plinth teaching docs for the detail panel"
```

---

### Task 3: Schema-v2 payload assembly (backend)

**Files:**
- Create: `src/Stan/Plinth/Payload.hs`
- Modify: `stan.cabal` (add exposed module), `test/Test/Stan/Plinth.hs`

- [ ] **Step 1: Write the failing tests** (append inside `plinthSpec`'s `describe` — or add a sibling `describe "Stan.Plinth.Payload"`):

```haskell
import Data.Aeson.Micro (Value (..), encode, object, (.=))
import Stan.Ghc.Compat (mkRealSrcLoc, mkRealSrcSpan)
import Stan.Inspection.Partial (stan0001)
import Stan.Inspection (inspectionId)
import Stan.Observation (Observation (..))
import Stan.Plinth.Payload (mkAnalyzePayload, mkCapabilitiesPayload, uniquifyFingerprints)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8

payloadSpec :: Spec
payloadSpec = describe "Stan.Plinth.Payload" $ do
    it "capabilities carries schemaVersion 2" $
        mkCapabilitiesPayload "9.6.6" `shouldBe` object
            [ "schemaVersion" .= (2 :: Int)
            , "ghcVersion" .= ("9.6.6" :: Text)
            , "features" .= (["list-onchain", "analyze", "fingerprints", "inspection-docs"] :: [Text])
            ]
    it "suffixes duplicate fingerprints in span order" $ do
        let content = BS8.pack "f x\nf x\n"      -- identical flagged text on 2 lines
        let o1 = mkPayloadObs 1 content
        let o2 = mkPayloadObs 2 content
        let fps = map snd (uniquifyFingerprints [o2, o1])   -- input deliberately unordered
        case fps of
            [fp1, fp2] -> do
                fp2 `shouldBe` (fp1 <> "#2")
            _ -> expectationFailure "expected exactly two fingerprints"
    it "analyze payload is version 2 with top-level observations" $ do
        let payloadText = LBS8.unpack (encode (mkAnalyzePayload Nothing [] []))
        payloadText `shouldSatisfy` isInfixOf "\"version\":2"
        payloadText `shouldSatisfy` isInfixOf "\"observations\":[]"
  where
    mkPayloadObs :: Int -> ByteString -> Observation
    mkPayloadObs line content = Observation
        { observationId = Id ("obs-" <> show line)
        , observationInspectionId = inspectionId stan0001
        , observationSrcSpan = mkRealSrcSpan
            (mkRealSrcLoc "src/T.hs" line 1)
            (mkRealSrcLoc "src/T.hs" line 4)
        , observationFile = "src/T.hs"
        , observationModuleName = "Test.Module.Name"
        , observationFileContent = content
        }
```

Export `plinthSpec` so it runs both describes (wrap: `plinthSpec = docsSpec >> payloadSpec` or list both under one `describe`). Register nothing new in Spec.hs (already registered in Task 2).

- [ ] **Step 2: Verify compile failure**

Run: `cabal build stan-test 2>&1 | tail -5`
Expected: `Stan.Plinth.Payload` not found.

- [ ] **Step 3: Create `src/Stan/Plinth/Payload.hs`** (and add to `exposed-modules` in `stan.cabal`):

```haskell
{- |
Copyright: (c) 2026 IOHK
SPDX-License-Identifier: MPL-2.0

Assembly of the machine-readable (schema v2) payloads emitted by the
@plustan@ CLI for the VS Code extension. Kept in the library so the
JSON shape is golden-testable.
-}

module Stan.Plinth.Payload
    ( analyzeSchemaVersion
    , mkAnalyzePayload
    , mkCapabilitiesPayload
    , uniquifyFingerprints
    ) where

import Data.Aeson.Micro (Value, object, (.=))

import Stan.Core.ModuleName (ModuleName)
import Stan.Ghc.Compat (srcSpanEndCol, srcSpanEndLine, srcSpanStartCol, srcSpanStartLine)
import Stan.Inspection (Inspection (..))
import Stan.Observation (Observation (..), observationFingerprint)
import Stan.Plinth.Docs (InspectionDocs (..), lookupDocs)

import qualified Data.HashMap.Strict as HM


-- | Version of the JSON contract between the CLI and the extension.
analyzeSchemaVersion :: Int
analyzeSchemaVersion = 2

mkCapabilitiesPayload :: Text -> Value
mkCapabilitiesPayload ghcVersion = object
    [ "schemaVersion" .= analyzeSchemaVersion
    , "ghcVersion" .= ghcVersion
    , "features" .= (["list-onchain", "analyze", "fingerprints", "inspection-docs"] :: [Text])
    ]

mkAnalyzePayload :: Maybe ModuleName -> [Inspection] -> [Observation] -> Value
mkAnalyzePayload targetModule inspections observations = object
    [ "version" .= analyzeSchemaVersion
    , "runScope" .= maybe ("all" :: Text) (const "module") targetModule
    , "targetModule" .= targetModule
    , "inspections" .= map inspectionToJson inspections
    , "observations" .= map observationToJson (uniquifyFingerprints observations)
    ]

{- | Pair each observation with a run-unique fingerprint: duplicates
(same rule, same module, same flagged text) get @#2@, @#3@… suffixes
in span order, so dismissing one of two identical findings never
dismisses both.
-}
uniquifyFingerprints :: [Observation] -> [(Observation, Text)]
uniquifyFingerprints observations =
    reverse $ fst $ foldl' step ([], HM.empty) sorted
  where
    sorted :: [Observation]
    sorted = sortOn spanKey observations

    spanKey :: Observation -> (FilePath, Int, Int)
    spanKey o =
        ( observationFile o
        , srcSpanStartLine (observationSrcSpan o)
        , srcSpanStartCol (observationSrcSpan o)
        )

    step
        :: ([(Observation, Text)], HashMap Text Int)
        -> Observation
        -> ([(Observation, Text)], HashMap Text Int)
    step (acc, seen) o =
        let base = observationFingerprint o
            n = HM.lookupDefault 0 base seen + 1
            fp = if n == 1 then base else base <> "#" <> show n
        in ((o, fp) : acc, HM.insert base n seen)

inspectionToJson :: Inspection -> Value
inspectionToJson Inspection{..} = object $
    [ "id" .= inspectionId
    , "name" .= inspectionName
    , "description" .= inspectionDescription
    , "solution" .= inspectionSolution
    , "category" .= toList inspectionCategory
    , "severity" .= inspectionSeverity
    ] <> docsPairs
  where
    docsPairs = case lookupDocs inspectionId of
        Nothing -> []
        Just InspectionDocs{..} ->
            [ "whyItMatters" .= docsWhyItMatters
            , "badExample" .= docsBadExample
            , "goodExample" .= docsGoodExample
            , "docsAnchor" .= docsAnchor
            ]

observationToJson :: (Observation, Text) -> Value
observationToJson (Observation{..}, fingerprint) = object
    [ "id" .= observationId
    , "inspectionId" .= observationInspectionId
    , "fingerprint" .= fingerprint
    , "startLine" .= srcSpanStartLine observationSrcSpan
    , "startCol" .= srcSpanStartCol observationSrcSpan
    , "endLine" .= srcSpanEndLine observationSrcSpan
    , "endCol" .= srcSpanEndCol observationSrcSpan
    , "file" .= toText observationFile
    , "moduleName" .= observationModuleName
    ]
```

Note: v1's `srcSpan` display string is intentionally dropped — the extension never used it.

- [ ] **Step 4: Run tests**

Run: `cabal test --test-show-details=direct 2>&1 | tail -20`
Expected: payload specs PASS.

- [ ] **Step 5: Commit**

```bash
git add src/Stan/Plinth/Payload.hs test/Test/Stan/Plinth.hs stan.cabal
git commit -m "feat(backend): schema-v2 payload assembly with unique fingerprints"
```

---

### Task 4: Wire `capabilities` + v2 payload into the CLI

**Files:**
- Modify: `app/PluStan.hs`, `stan.cabal:3` (version), `CHANGELOG.md`

- [ ] **Step 1: Add the subcommand and switch the analyze payload**

In `app/PluStan.hs`:

1. Imports: add `import Stan.Plinth.Payload (analyzeSchemaVersion, mkAnalyzePayload, mkCapabilitiesPayload)`.
2. Extend the command type and parser (`parsePluStanCommand`, `app/PluStan.hs:278`):

```haskell
data PluStanCommand
  = CommandAnalyze AnalyzeArgs
  | CommandListOnchain ListOnchainArgs
  | CommandCapabilities
```

and add the branch **before** the fallback positional case:

```haskell
  "capabilities":_ -> Right CommandCapabilities
```

3. Dispatch in `runPluStan`:

```haskell
      CommandCapabilities -> runCapabilities
```

4. Add:

```haskell
-- | Machine-readable handshake: always JSON, no project needed.
runCapabilities :: IO ()
runCapabilities = putJson $
  mkCapabilitiesPayload (Text.pack (showVersion compilerVersion))
```

5. In `runAnalyze` (`app/PluStan.hs:193`), replace the `putJson AnalyzeJsonPayload {..}` block with:

```haskell
      then putJson $ mkAnalyzePayload
             (onchainModuleName <$> targetModule)
             usedInspections
             (toList observations)
```

(`observations` is an `Slist`; `Data.Foldable.toList` is already imported.) Delete the now-unused `AnalyzeJsonPayload` type and its `ToJSON` instance.

6. In `runListOnchain`, change `listPayloadVersion = 1` to `listPayloadVersion = analyzeSchemaVersion` (the list shape didn't change, but a single schema number for the whole CLI surface avoids confusion).
7. Add a `capabilities` line to `usage`:

```haskell
  , "  plustan capabilities            Print JSON schema/feature handshake"
```

- [ ] **Step 2: Build and verify by running**

```bash
cabal build exe:plustan
cabal run -v0 plustan -- capabilities
```
Expected output (exact, single line — microaeson sorts keys alphabetically; `System.Info.compilerVersion` carries only major.minor):
`{"features":["list-onchain","analyze","fingerprints","inspection-docs"],"ghcVersion":"9.6","schemaVersion":2}`

```bash
cabal run -v0 plustan -- analyze --json 2>/dev/null | python3 -c "import json,sys; p=json.load(sys.stdin); print(p['version'], len(p['observations']), p['observations'][0]['fingerprint'] if p['observations'] else 'none')"
```
Expected: `2 <N> FPR-PLU-STAN-…` — version 2, top-level observations, fingerprints present (the repo's own `target/` modules are the analyzed project).

- [ ] **Step 3: Bump version + changelog**

`stan.cabal:3`: `version: 0.2.5.0`. Add a `CHANGELOG.md` entry at the top following the existing format: schema v2 (`fingerprint` per observation, inspection docs fields, top-level `observations`, `analysis` removed from `--json`), new `capabilities` subcommand.

- [ ] **Step 4: Full backend test run**

Run: `cabal test --test-show-details=direct 2>&1 | tail -10`
Expected: suite PASS.

- [ ] **Step 5: Commit**

```bash
git add app/PluStan.hs stan.cabal CHANGELOG.md
git commit -m "feat(cli): capabilities handshake and schema-v2 analyze payload (v0.2.5.0)"
```

---

### Task 5: Extension unit-test infrastructure

**Files:**
- Modify: `vscode-plustan/package.json`
- Create: `vscode-plustan/src/core/sanity.test.ts`

- [ ] **Step 1: Add mocha**

```bash
cd vscode-plustan && npm install --save-dev mocha@^10.4.0 @types/mocha@^10.0.6
```

In `package.json` scripts, add:

```json
"test:unit": "npm run compile && mocha \"out/core/**/*.test.js\" \"out/analyzer/**/*.test.js\""
```

- [ ] **Step 2: Write a sanity test**

`vscode-plustan/src/core/sanity.test.ts`:

```ts
import * as assert from "node:assert";

describe("test infrastructure", () => {
  it("runs", () => {
    assert.strictEqual(1 + 1, 2);
  });
});
```

- [ ] **Step 3: Run**

Run: `cd vscode-plustan && npm run test:unit`
Expected: `1 passing`.

- [ ] **Step 4: Commit**

```bash
git add vscode-plustan/package.json vscode-plustan/package-lock.json vscode-plustan/src/core/sanity.test.ts
git commit -m "chore(ext): mocha unit-test infrastructure for vscode-free core modules"
```

---

### Task 6: `core/schema.ts` — v2 payload types + validation

**Files:**
- Create: `vscode-plustan/src/core/schema.ts`
- Test: `vscode-plustan/src/core/schema.test.ts`

- [ ] **Step 1: Write the failing tests**

```ts
import * as assert from "node:assert";
import { parseAnalyzePayload, parseCapabilities, SchemaError } from "./schema";

const validPayload = {
  version: 2,
  runScope: "all",
  targetModule: null,
  inspections: [{
    id: "PLU-STAN-04", name: "Credential eq", description: "d", solution: ["s"],
    category: ["Plutus"], severity: "Warning",
    whyItMatters: "w", badExample: "b", goodExample: "g", docsAnchor: "equality"
  }],
  observations: [{
    id: "o1", inspectionId: "PLU-STAN-04", fingerprint: "FPR-PLU-STAN-04-abc-def",
    file: "src/V.hs", moduleName: "V", startLine: 4, startCol: 1, endLine: 4, endCol: 10
  }]
};

describe("schema", () => {
  it("parses a valid v2 analyze payload", () => {
    const p = parseAnalyzePayload(validPayload);
    assert.strictEqual(p.observations[0].fingerprint, "FPR-PLU-STAN-04-abc-def");
    assert.strictEqual(p.inspections[0].whyItMatters, "w");
  });
  it("rejects v1 payloads as unsupported-version", () => {
    assert.throws(
      () => parseAnalyzePayload({ version: 1, inspections: [], analysis: { observations: [] } }),
      (e: unknown) => e instanceof SchemaError && e.reason === "unsupported-version"
    );
  });
  it("rejects structurally broken payloads as malformed", () => {
    assert.throws(
      () => parseAnalyzePayload({ version: 2, runScope: "all", targetModule: null, inspections: [] }),
      (e: unknown) => e instanceof SchemaError && e.reason === "malformed"
    );
  });
  it("parses capabilities and rejects wrong schemaVersion", () => {
    const c = parseCapabilities({ schemaVersion: 2, ghcVersion: "9.6", features: ["fingerprints"] });
    assert.strictEqual(c.schemaVersion, 2);
    assert.throws(
      () => parseCapabilities({ schemaVersion: 3, features: [] }),
      (e: unknown) => e instanceof SchemaError && e.reason === "unsupported-version"
    );
  });
});
```

- [ ] **Step 2: Run to verify failure** — `npm run test:unit` → compile error (module missing).

- [ ] **Step 3: Implement `core/schema.ts`** (no vscode imports):

```ts
export const SUPPORTED_SCHEMA_VERSION = 2;

export interface InspectionV2 {
  id: string; name: string; description: string;
  solution: string[]; category: string[]; severity: string;
  whyItMatters?: string; badExample?: string; goodExample?: string; docsAnchor?: string;
}

export interface ObservationV2 {
  id: string; inspectionId: string; fingerprint: string;
  file: string; moduleName: string;
  startLine: number; startCol: number; endLine: number; endCol: number;
}

export interface AnalyzePayloadV2 {
  version: number;
  runScope: "all" | "module";
  targetModule: string | null;
  inspections: InspectionV2[];
  observations: ObservationV2[];
}

export interface CapabilitiesPayload {
  schemaVersion: number;
  ghcVersion?: string;
  features: string[];
}

export interface OnchainModule { moduleName: string; file: string; annotationSource: string; }
export interface ListOnchainPayload { version: number; workspaceRoot: string; hieDir: string; modules: OnchainModule[]; }

export class SchemaError extends Error {
  constructor(readonly reason: "unsupported-version" | "malformed", message: string) {
    super(message);
    this.name = "SchemaError";
  }
}

function asRecord(raw: unknown, what: string): Record<string, unknown> {
  if (typeof raw !== "object" || raw === null || Array.isArray(raw)) {
    throw new SchemaError("malformed", `${what} is not an object`);
  }
  return raw as Record<string, unknown>;
}

function requireVersion(actual: unknown, what: string): void {
  if (actual !== SUPPORTED_SCHEMA_VERSION) {
    throw new SchemaError(
      "unsupported-version",
      `${what} has schema version ${String(actual)}; this extension requires ${SUPPORTED_SCHEMA_VERSION}. ` +
      `Run "Plu-Stan: Check for Updates" to fetch a matching binary.`
    );
  }
}

export function parseAnalyzePayload(raw: unknown): AnalyzePayloadV2 {
  const o = asRecord(raw, "analyze payload");
  requireVersion(o.version, "analyze payload");
  if (!Array.isArray(o.inspections) || !Array.isArray(o.observations)) {
    throw new SchemaError("malformed", "analyze payload: missing inspections/observations arrays");
  }
  for (const obs of o.observations) {
    const r = asRecord(obs, "observation");
    for (const key of ["fingerprint", "inspectionId", "file", "moduleName"]) {
      if (typeof r[key] !== "string") {
        throw new SchemaError("malformed", `observation: missing string field '${key}'`);
      }
    }
    for (const key of ["startLine", "startCol", "endLine", "endCol"]) {
      if (typeof r[key] !== "number") {
        throw new SchemaError("malformed", `observation: missing numeric field '${key}'`);
      }
    }
  }
  return o as unknown as AnalyzePayloadV2;
}

export function parseCapabilities(raw: unknown): CapabilitiesPayload {
  const o = asRecord(raw, "capabilities payload");
  requireVersion(o.schemaVersion, "plustan binary");
  if (!Array.isArray(o.features)) {
    throw new SchemaError("malformed", "capabilities payload: missing features array");
  }
  return o as unknown as CapabilitiesPayload;
}

export function parseListOnchain(raw: unknown): ListOnchainPayload {
  const o = asRecord(raw, "list-onchain payload");
  if (!Array.isArray(o.modules)) {
    throw new SchemaError("malformed", "list-onchain payload: missing modules array");
  }
  return o as unknown as ListOnchainPayload;
}
```

- [ ] **Step 4: Run** — `npm run test:unit` → all schema tests PASS.

- [ ] **Step 5: Commit**

```bash
git add vscode-plustan/src/core/schema.ts vscode-plustan/src/core/schema.test.ts
git commit -m "feat(ext): schema-v2 payload types and validation"
```

---

### Task 7: `analyzer/client.ts` — AnalyzerClient + SpawnAnalyzerClient

**Files:**
- Create: `vscode-plustan/src/analyzer/client.ts`
- Create: `vscode-plustan/test-fixtures/fake-plustan.js`
- Test: `vscode-plustan/src/analyzer/client.test.ts`

This moves `spawnCommand`, `parseJsonFromOutput`, and `describePluStanFailure` out of `extension.ts` (they get deleted from there in Task 14) and makes them vscode-free: logging via an injected `(line: string) => void`, cancellation via `AbortSignal`.

- [ ] **Step 1: Create the fake binary fixture** `vscode-plustan/test-fixtures/fake-plustan.js`:

```js
#!/usr/bin/env node
// Stub plustan for tests: emits canned schema-v2 JSON per subcommand.
const cmd = process.argv[2];
const payloads = {
  capabilities: { schemaVersion: 2, ghcVersion: "9.6", features: ["list-onchain", "analyze", "fingerprints", "inspection-docs"] },
  "list-onchain": {
    version: 2, workspaceRoot: process.cwd(), hieDir: ".hie",
    modules: [{ moduleName: "Fixture.Validator", file: "src/Fixture/Validator.hs", annotationSource: "source" }]
  },
  analyze: {
    version: 2, runScope: process.argv.includes("--module") ? "module" : "all",
    targetModule: null,
    inspections: [{
      id: "PLU-STAN-04", name: "Credential equality", description: "desc", solution: ["sol"],
      category: ["Plutus"], severity: "Warning",
      whyItMatters: "staking theft", badExample: "bad", goodExample: "good", docsAnchor: "equality"
    }],
    observations: [
      { id: "o1", inspectionId: "PLU-STAN-04", fingerprint: "FPR-PLU-STAN-04-aaa-bbb",
        file: "src/Fixture/Validator.hs", moduleName: "Fixture.Validator",
        startLine: 3, startCol: 1, endLine: 3, endCol: 10 },
      { id: "o2", inspectionId: "PLU-STAN-04", fingerprint: "FPR-PLU-STAN-04-aaa-ccc",
        file: "src/Fixture/Validator.hs", moduleName: "Fixture.Validator",
        startLine: 7, startCol: 1, endLine: 7, endCol: 10 }
    ]
  }
};
const p = payloads[cmd];
if (!p) { process.stderr.write(`unknown command ${cmd}\n`); process.exit(1); }
process.stdout.write(JSON.stringify(p) + "\n");
```

- [ ] **Step 2: Write the failing tests** `src/analyzer/client.test.ts`:

```ts
import * as assert from "node:assert";
import * as path from "node:path";
import { AnalyzerError, SpawnAnalyzerClient } from "./client";

// out/analyzer/client.test.js → repo fixture dir
const fixtures = path.resolve(__dirname, "..", "..", "test-fixtures");
const fakeBinary = process.execPath; // node itself
const fakeScript = path.join(fixtures, "fake-plustan.js");

function client(extraArgs: string[] = []): SpawnAnalyzerClient {
  return new SpawnAnalyzerClient(
    () => ({
      binaryPath: fakeBinary,
      binaryPrefixArgs: [fakeScript], // test seam: lets node run the stub script
      cwd: fixtures,
      hieDir: ".hie",
      extraArgs
    }),
    () => { /* silent log */ }
  );
}

describe("SpawnAnalyzerClient", () => {
  it("fetches and validates capabilities", async () => {
    const caps = await client().capabilities();
    assert.strictEqual(caps.schemaVersion, 2);
  });
  it("analyzes and returns a validated v2 payload", async () => {
    const p = await client().analyze({ kind: "workspace" });
    assert.strictEqual(p.observations.length, 2);
    assert.strictEqual(p.observations[0].fingerprint, "FPR-PLU-STAN-04-aaa-bbb");
  });
  it("classifies a missing binary as not-found", async () => {
    const bad = new SpawnAnalyzerClient(
      () => ({ binaryPath: "/nonexistent/plustan", binaryPrefixArgs: [], cwd: fixtures, hieDir: ".hie", extraArgs: [] }),
      () => { /* silent */ }
    );
    await assert.rejects(bad.capabilities(), (e: unknown) => e instanceof AnalyzerError && e.kind === "not-found");
  });
});
```

- [ ] **Step 3: Run to verify failure** — `npm run test:unit` → module missing.

- [ ] **Step 4: Implement `src/analyzer/client.ts`** (no vscode imports):

```ts
import { spawn } from "node:child_process";
import {
  AnalyzePayloadV2, CapabilitiesPayload, ListOnchainPayload,
  parseAnalyzePayload, parseCapabilities, parseListOnchain, SchemaError
} from "../core/schema";

export type AnalyzeScope = { kind: "workspace" } | { kind: "module"; moduleName: string };

export type AnalyzerErrorKind =
  | "not-found"        // binary missing (ENOENT)
  | "schema"           // JSON parsed but wrong schema version / shape
  | "ghc-mismatch"     // .hie files built by a different GHC
  | "build-failed"     // cabal build inside plustan failed
  | "crash"            // GHC panic
  | "no-json";         // anything else that produced no usable JSON

export class AnalyzerError extends Error {
  constructor(readonly kind: AnalyzerErrorKind, message: string) {
    super(message);
    this.name = "AnalyzerError";
  }
}

export interface SpawnConfig {
  binaryPath: string;
  /** Args inserted before the subcommand — empty in production, used by tests to run `node fake-plustan.js …`. */
  binaryPrefixArgs: string[];
  cwd: string;
  hieDir: string;
  extraArgs: string[];
}

export interface AnalyzerClient {
  capabilities(signal?: AbortSignal): Promise<CapabilitiesPayload>;
  listOnchain(signal?: AbortSignal): Promise<ListOnchainPayload>;
  analyze(scope: AnalyzeScope, signal?: AbortSignal): Promise<AnalyzePayloadV2>;
}

export class SpawnAnalyzerClient implements AnalyzerClient {
  constructor(
    private readonly getConfig: () => SpawnConfig,
    private readonly log: (line: string) => void
  ) {}

  async capabilities(signal?: AbortSignal): Promise<CapabilitiesPayload> {
    return parseCapabilities(await this.runJson(["capabilities"], signal));
  }

  async listOnchain(signal?: AbortSignal): Promise<ListOnchainPayload> {
    const config = this.getConfig();
    return parseListOnchain(await this.runJson(["list-onchain", "--json", "--hiedir", config.hieDir], signal));
  }

  async analyze(scope: AnalyzeScope, signal?: AbortSignal): Promise<AnalyzePayloadV2> {
    const config = this.getConfig();
    const args = ["analyze", "--json", "--hiedir", config.hieDir, ...config.extraArgs];
    if (scope.kind === "module") {
      args.push("--module", scope.moduleName);
    }
    return parseAnalyzePayload(await this.runJson(args, signal));
  }

  private async runJson(args: string[], signal?: AbortSignal): Promise<unknown> {
    const config = this.getConfig();
    const fullArgs = [...config.binaryPrefixArgs, ...args];
    this.log(`$ ${config.binaryPath} ${fullArgs.join(" ")}`);

    const { stdout, stderr, exitCode } = await this.spawnOnce(config, fullArgs, signal);

    let parsed: unknown;
    try {
      parsed = parseJsonFromOutput(stdout);
    } catch {
      throw classifyNoJsonFailure(stdout, stderr, exitCode);
    }
    if (exitCode !== 0) {
      this.log(`plustan exited with code ${exitCode}; using emitted JSON payload.`);
    }
    return parsed;
  }

  private spawnOnce(
    config: SpawnConfig,
    args: string[],
    signal?: AbortSignal
  ): Promise<{ stdout: string; stderr: string; exitCode: number }> {
    return new Promise((resolve, reject) => {
      const child = spawn(config.binaryPath, args, { cwd: config.cwd, env: process.env });
      let stdout = "";
      let stderr = "";
      const onAbort = (): void => {
        child.kill("SIGTERM");
      };
      signal?.addEventListener("abort", onAbort, { once: true });

      child.stdout.on("data", (chunk: Buffer) => { stdout += chunk.toString("utf8"); });
      child.stderr.on("data", (chunk: Buffer) => {
        const text = chunk.toString("utf8");
        stderr += text;
        this.log(text.trimEnd());
      });
      child.on("error", (error: NodeJS.ErrnoException) => {
        signal?.removeEventListener("abort", onAbort);
        if (error.code === "ENOENT") {
          reject(new AnalyzerError("not-found",
            `Plu-Stan binary not found: ${config.binaryPath}. Set \`plustan.binaryPath\` or run "Plu-Stan: Check for Updates".`));
        } else {
          reject(new AnalyzerError("no-json", `Failed to start plustan: ${error.message}`));
        }
      });
      child.on("close", (code) => {
        signal?.removeEventListener("abort", onAbort);
        resolve({ stdout, stderr, exitCode: code ?? 1 });
      });
    });
  }
}

/** Scan stdout lines from the end for the JSON payload (build noise may precede it). */
export function parseJsonFromOutput(stdout: string): unknown {
  const lines = stdout.split(/\r?\n/).map((l) => l.trim()).filter(Boolean);
  for (let i = lines.length - 1; i >= 0; i -= 1) {
    try {
      return JSON.parse(lines[i]);
    } catch {
      // keep scanning earlier lines
    }
  }
  return JSON.parse(stdout);
}

/** Turn a no-JSON plustan run into a typed, user-actionable error. */
export function classifyNoJsonFailure(stdout: string, stderr: string, exitCode: number): AnalyzerError {
  const haystack = `${stderr}\n${stdout}`;
  if (/hie file versions|readHieFile|built by a different ghc|different ghc/i.test(haystack)) {
    return new AnalyzerError("ghc-mismatch",
      "Plu-Stan couldn't read your project's .hie files: they were built with a different GHC than the plustan binary. " +
      "Rebuild with the matching GHC, or run \"Plu-Stan: Check for Updates\".");
  }
  if (/panic!|the 'impossible' happened/i.test(haystack)) {
    return new AnalyzerError("crash", `Plu-Stan crashed (exit ${exitCode}). See the Plu-Stan output channel.`);
  }
  if (exitCode !== 0 && /error:|\[error\]/i.test(stderr)) {
    return new AnalyzerError("build-failed",
      "The project build failed, so analysis could not run. Fix the compile errors and save again.");
  }
  return new AnalyzerError("no-json",
    `Plu-Stan produced no JSON output (exit ${exitCode}). The binary may be outdated — try "Plu-Stan: Check for Updates".`);
}
```

Note: `SchemaError` thrown by the `parse*` functions passes through untranslated — callers distinguish `SchemaError` (update the binary) from `AnalyzerError` (run/environment problems).

- [ ] **Step 5: Run** — `npm run test:unit` → all client tests PASS.

- [ ] **Step 6: Commit**

```bash
git add vscode-plustan/src/analyzer vscode-plustan/test-fixtures/fake-plustan.js
git commit -m "feat(ext): AnalyzerClient seam with typed failure classification"
```

---

### Task 8: `core/sessionState.ts` — the session reducer

**Files:**
- Create: `vscode-plustan/src/core/sessionState.ts`
- Test: `vscode-plustan/src/core/sessionState.test.ts`

- [ ] **Step 1: Write the failing tests**

```ts
import * as assert from "node:assert";
import { countByStatus, initialSessionState, reduceSession, SessionState } from "./sessionState";
import { ObservationV2 } from "./schema";

const obs = (fingerprint: string, file = "src/V.hs", line = 3): ObservationV2 => ({
  id: "o", inspectionId: "PLU-STAN-04", fingerprint,
  file, moduleName: "V", startLine: line, startCol: 1, endLine: line, endCol: 10
});

const started = (): SessionState =>
  reduceSession(initialSessionState, { type: "sessionStarted", startedAt: "2026-07-06T10:00:00Z" });

const afterRun = (state: SessionState, observations: ObservationV2[], dismissed: string[] = []): SessionState =>
  reduceSession(state, { type: "runCompleted", coveredFiles: ["src/V.hs"], observations, dismissedFingerprints: dismissed });

describe("session reducer", () => {
  it("marks findings from a run as open", () => {
    const s = afterRun(started(), [obs("f1"), obs("f2", "src/V.hs", 7)]);
    assert.strictEqual(countByStatus(s).open, 2);
  });
  it("marks a finding fixed when a later run over its file no longer reports it", () => {
    const s1 = afterRun(started(), [obs("f1"), obs("f2", "src/V.hs", 7)]);
    const s2 = afterRun(s1, [obs("f2", "src/V.hs", 7)]);
    assert.strictEqual(s2.findings["f1"].status, "fixed");
    assert.strictEqual(s2.findings["f2"].status, "open");
  });
  it("does not touch findings in files not covered by the run", () => {
    const s1 = afterRun(started(), [obs("f1"), obs("g1", "src/Other.hs")]);
    const s2 = afterRun(s1, []); // covers only src/V.hs
    assert.strictEqual(s2.findings["f1"].status, "fixed");
    assert.strictEqual(s2.findings["g1"].status, "open");
  });
  it("marks open findings stale when their file is edited, and re-opens on re-run", () => {
    const s1 = afterRun(started(), [obs("f1")]);
    const s2 = reduceSession(s1, { type: "fileEdited", file: "src/V.hs" });
    assert.strictEqual(s2.findings["f1"].status, "stale");
    const s3 = afterRun(s2, [obs("f1")]);
    assert.strictEqual(s3.findings["f1"].status, "open");
  });
  it("applies and persists dismissals across runs", () => {
    const s1 = afterRun(started(), [obs("f1")], ["f1"]);
    assert.strictEqual(s1.findings["f1"].status, "dismissed");
    const s2 = afterRun(s1, [obs("f1")], ["f1"]);
    assert.strictEqual(s2.findings["f1"].status, "dismissed");
  });
  it("dismisses and undismisses interactively", () => {
    const s1 = afterRun(started(), [obs("f1")]);
    const s2 = reduceSession(s1, { type: "findingDismissed", fingerprint: "f1" });
    assert.strictEqual(s2.findings["f1"].status, "dismissed");
    const s3 = reduceSession(s2, { type: "findingUndismissed", fingerprint: "f1" });
    assert.strictEqual(s3.findings["f1"].status, "open");
  });
  it("ends the session back to idle but keeps findings for the summary", () => {
    const s = reduceSession(afterRun(started(), [obs("f1")]), { type: "sessionEnded" });
    assert.strictEqual(s.phase, "idle");
    assert.strictEqual(Object.keys(s.findings).length, 1);
  });
});
```

- [ ] **Step 2: Run to verify failure** — `npm run test:unit`.

- [ ] **Step 3: Implement `core/sessionState.ts`** (no vscode imports):

```ts
import { ObservationV2 } from "./schema";

export type FindingStatus = "open" | "stale" | "fixed" | "dismissed";

export interface SessionFinding extends ObservationV2 {
  status: FindingStatus;
  lastSeenRun: number;
}

export interface SessionState {
  phase: "idle" | "active";
  startedAt: string | null;
  runCount: number;
  findings: Record<string, SessionFinding>;
}

export type SessionEvent =
  | { type: "sessionStarted"; startedAt: string }
  | { type: "runCompleted"; coveredFiles: string[]; observations: ObservationV2[]; dismissedFingerprints: string[] }
  | { type: "fileEdited"; file: string }
  | { type: "findingDismissed"; fingerprint: string }
  | { type: "findingUndismissed"; fingerprint: string }
  | { type: "sessionEnded" };

export const initialSessionState: SessionState = {
  phase: "idle",
  startedAt: null,
  runCount: 0,
  findings: {}
};

export function reduceSession(state: SessionState, event: SessionEvent): SessionState {
  switch (event.type) {
    case "sessionStarted":
      return { phase: "active", startedAt: event.startedAt, runCount: 0, findings: {} };

    case "sessionEnded":
      return { ...state, phase: "idle" };

    case "runCompleted": {
      const covered = new Set(event.coveredFiles);
      const dismissed = new Set(event.dismissedFingerprints);
      const runCount = state.runCount + 1;
      const findings: Record<string, SessionFinding> = {};

      // Carry forward everything in files this run did not cover.
      for (const f of Object.values(state.findings)) {
        if (!covered.has(f.file)) {
          findings[f.fingerprint] = f;
        }
      }
      // Everything reported by this run is open (or dismissed).
      for (const o of event.observations) {
        findings[o.fingerprint] = {
          ...o,
          status: dismissed.has(o.fingerprint) ? "dismissed" : "open",
          lastSeenRun: runCount
        };
      }
      // Previously-known findings in covered files that were NOT re-reported: fixed.
      for (const f of Object.values(state.findings)) {
        if (covered.has(f.file) && findings[f.fingerprint] === undefined) {
          findings[f.fingerprint] =
            f.status === "open" || f.status === "stale" ? { ...f, status: "fixed" } : f;
        }
      }
      return { ...state, runCount, findings };
    }

    case "fileEdited": {
      const findings = { ...state.findings };
      for (const f of Object.values(findings)) {
        if (f.file === event.file && f.status === "open") {
          findings[f.fingerprint] = { ...f, status: "stale" };
        }
      }
      return { ...state, findings };
    }

    case "findingDismissed":
    case "findingUndismissed": {
      const f = state.findings[event.fingerprint];
      if (!f) {
        return state;
      }
      const status: FindingStatus = event.type === "findingDismissed" ? "dismissed" : "open";
      return { ...state, findings: { ...state.findings, [event.fingerprint]: { ...f, status } } };
    }
  }
}

export function countByStatus(state: SessionState): Record<FindingStatus, number> {
  const counts: Record<FindingStatus, number> = { open: 0, stale: 0, fixed: 0, dismissed: 0 };
  for (const f of Object.values(state.findings)) {
    counts[f.status] += 1;
  }
  return counts;
}
```

- [ ] **Step 4: Run** — `npm run test:unit` → all reducer tests PASS.

- [ ] **Step 5: Commit**

```bash
git add vscode-plustan/src/core/sessionState.ts vscode-plustan/src/core/sessionState.test.ts
git commit -m "feat(ext): pure review-session reducer (open/stale/fixed/dismissed)"
```

---

### Task 9: Dismissals — pure logic + workspace store

**Files:**
- Create: `vscode-plustan/src/core/dismissals.ts`, `vscode-plustan/src/session/dismissalsStore.ts`
- Test: `vscode-plustan/src/core/dismissals.test.ts`

- [ ] **Step 1: Write the failing tests**

```ts
import * as assert from "node:assert";
import { addDismissal, emptyDismissals, parseDismissals, removeDismissal, serializeDismissals } from "./dismissals";

describe("dismissals", () => {
  it("round-trips through serialize/parse", () => {
    const d1 = addDismissal(emptyDismissals(), {
      fingerprint: "f1", inspectionId: "PLU-STAN-04", note: "intentional", dismissedAt: "2026-07-06T10:00:00Z"
    });
    const d2 = parseDismissals(serializeDismissals(d1));
    assert.deepStrictEqual(d2, d1);
  });
  it("dedupes by fingerprint", () => {
    const base = addDismissal(emptyDismissals(), { fingerprint: "f1", inspectionId: "X", dismissedAt: "t" });
    const twice = addDismissal(base, { fingerprint: "f1", inspectionId: "X", dismissedAt: "t2" });
    assert.strictEqual(twice.dismissals.length, 1);
    assert.strictEqual(twice.dismissals[0].dismissedAt, "t2");
  });
  it("removes by fingerprint", () => {
    const base = addDismissal(emptyDismissals(), { fingerprint: "f1", inspectionId: "X", dismissedAt: "t" });
    assert.strictEqual(removeDismissal(base, "f1").dismissals.length, 0);
  });
  it("tolerates broken file content", () => {
    assert.deepStrictEqual(parseDismissals("not json {"), emptyDismissals());
    assert.deepStrictEqual(parseDismissals('{"version":1}'), emptyDismissals());
  });
});
```

- [ ] **Step 2: Run to verify failure.**

- [ ] **Step 3: Implement `core/dismissals.ts`** (no vscode imports):

```ts
export interface DismissalEntry {
  fingerprint: string;
  inspectionId: string;
  note?: string;
  dismissedAt: string;
}

export interface DismissalsFile {
  version: 1;
  dismissals: DismissalEntry[];
}

export function emptyDismissals(): DismissalsFile {
  return { version: 1, dismissals: [] };
}

export function parseDismissals(text: string): DismissalsFile {
  try {
    const raw = JSON.parse(text) as { version?: unknown; dismissals?: unknown };
    if (!Array.isArray(raw.dismissals)) {
      return emptyDismissals();
    }
    const dismissals = raw.dismissals.filter(
      (d): d is DismissalEntry =>
        typeof d === "object" && d !== null &&
        typeof (d as DismissalEntry).fingerprint === "string" &&
        typeof (d as DismissalEntry).inspectionId === "string"
    );
    return { version: 1, dismissals };
  } catch {
    return emptyDismissals();
  }
}

export function serializeDismissals(file: DismissalsFile): string {
  return JSON.stringify(file, null, 2) + "\n";
}

export function addDismissal(file: DismissalsFile, entry: DismissalEntry): DismissalsFile {
  return {
    version: 1,
    dismissals: [...file.dismissals.filter((d) => d.fingerprint !== entry.fingerprint), entry]
  };
}

export function removeDismissal(file: DismissalsFile, fingerprint: string): DismissalsFile {
  return { version: 1, dismissals: file.dismissals.filter((d) => d.fingerprint !== fingerprint) };
}
```

- [ ] **Step 4: Implement `session/dismissalsStore.ts`** (vscode-coupled, thin):

```ts
import * as vscode from "vscode";
import { DismissalEntry, DismissalsFile, addDismissal, emptyDismissals, parseDismissals, removeDismissal, serializeDismissals } from "../core/dismissals";

const FILE_PATH = ".plustan/dismissals.json";

export class DismissalsStore {
  constructor(private readonly folder: vscode.WorkspaceFolder) {}

  private get uri(): vscode.Uri {
    return vscode.Uri.joinPath(this.folder.uri, FILE_PATH);
  }

  async load(): Promise<DismissalsFile> {
    try {
      const bytes = await vscode.workspace.fs.readFile(this.uri);
      return parseDismissals(Buffer.from(bytes).toString("utf8"));
    } catch {
      return emptyDismissals(); // file absent
    }
  }

  async add(entry: DismissalEntry): Promise<DismissalsFile> {
    const next = addDismissal(await this.load(), entry);
    await this.save(next);
    return next;
  }

  async remove(fingerprint: string): Promise<DismissalsFile> {
    const next = removeDismissal(await this.load(), fingerprint);
    await this.save(next);
    return next;
  }

  private async save(file: DismissalsFile): Promise<void> {
    await vscode.workspace.fs.createDirectory(vscode.Uri.joinPath(this.folder.uri, ".plustan"));
    await vscode.workspace.fs.writeFile(this.uri, Buffer.from(serializeDismissals(file), "utf8"));
  }
}
```

- [ ] **Step 5: Run** — `npm run test:unit` → dismissal tests PASS; `npm run compile` clean.

- [ ] **Step 6: Commit**

```bash
git add vscode-plustan/src/core/dismissals.ts vscode-plustan/src/core/dismissals.test.ts vscode-plustan/src/session/dismissalsStore.ts
git commit -m "feat(ext): persistent dismissals in .plustan/dismissals.json"
```

---

### Task 10: `core/runCoalescer.ts` — pending-run queue

**Files:**
- Create: `vscode-plustan/src/core/runCoalescer.ts`
- Test: `vscode-plustan/src/core/runCoalescer.test.ts`

- [ ] **Step 1: Write the failing tests**

```ts
import * as assert from "node:assert";
import { RunCoalescer } from "./runCoalescer";

describe("RunCoalescer", () => {
  it("coalesces duplicate module requests", () => {
    const q = new RunCoalescer();
    q.request({ kind: "module", moduleName: "A" });
    q.request({ kind: "module", moduleName: "A" });
    q.request({ kind: "module", moduleName: "B" });
    assert.strictEqual(q.size, 2);
  });
  it("a workspace request subsumes all pending module requests", () => {
    const q = new RunCoalescer();
    q.request({ kind: "module", moduleName: "A" });
    q.request({ kind: "workspace" });
    assert.strictEqual(q.size, 1);
    assert.deepStrictEqual(q.takeNext(), { kind: "workspace" });
  });
  it("module requests while a workspace run is pending are absorbed", () => {
    const q = new RunCoalescer();
    q.request({ kind: "workspace" });
    q.request({ kind: "module", moduleName: "A" });
    assert.strictEqual(q.size, 1);
  });
  it("serves FIFO otherwise", () => {
    const q = new RunCoalescer();
    q.request({ kind: "module", moduleName: "A" });
    q.request({ kind: "module", moduleName: "B" });
    assert.deepStrictEqual(q.takeNext(), { kind: "module", moduleName: "A" });
    assert.deepStrictEqual(q.takeNext(), { kind: "module", moduleName: "B" });
    assert.strictEqual(q.takeNext(), undefined);
  });
});
```

- [ ] **Step 2: Run to verify failure.**

- [ ] **Step 3: Implement `core/runCoalescer.ts`:**

```ts
export type PendingRun = { kind: "workspace" } | { kind: "module"; moduleName: string };

/** Pending analysis runs: duplicates coalesce, workspace subsumes modules. */
export class RunCoalescer {
  private pending: PendingRun[] = [];

  request(run: PendingRun): void {
    if (this.pending.some((p) => p.kind === "workspace")) {
      return; // a pending workspace run already covers everything
    }
    if (run.kind === "workspace") {
      this.pending = [{ kind: "workspace" }];
      return;
    }
    if (!this.pending.some((p) => p.kind === "module" && p.moduleName === run.moduleName)) {
      this.pending.push(run);
    }
  }

  takeNext(): PendingRun | undefined {
    return this.pending.shift();
  }

  get size(): number {
    return this.pending.length;
  }
}
```

- [ ] **Step 4: Run** — `npm run test:unit` → PASS.

- [ ] **Step 5: Commit**

```bash
git add vscode-plustan/src/core/runCoalescer.ts vscode-plustan/src/core/runCoalescer.test.ts
git commit -m "feat(ext): run coalescing for save-triggered analyses"
```

---

### Task 11: Status bar + review controller

**Files:**
- Create: `vscode-plustan/src/ui/statusBar.ts`, `vscode-plustan/src/session/controller.ts`

These are vscode-coupled; their pure logic already lives in Tasks 8/10. Verification is `npm run compile` here plus the integration test in Task 15.

- [ ] **Step 1: Implement `ui/statusBar.ts`:**

```ts
import * as vscode from "vscode";
import { countByStatus, SessionState } from "../core/sessionState";

export class PluStanStatusBar implements vscode.Disposable {
  private readonly item: vscode.StatusBarItem;

  constructor() {
    this.item = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 50);
    this.item.command = "plustanFindings.focus";
  }

  update(state: SessionState, running: boolean, buildFailed: boolean): void {
    if (state.phase === "idle") {
      this.item.hide();
      return;
    }
    const counts = countByStatus(state);
    const spinner = running ? "$(sync~spin) " : "";
    const failure = buildFailed ? " · build failed — results stale" : "";
    this.item.text = `${spinner}Plu-Stan: ${counts.open + counts.stale} open · ${counts.fixed} fixed${failure}`;
    this.item.tooltip = "Plu-Stan review session — click to open findings";
    this.item.show();
  }

  dispose(): void {
    this.item.dispose();
  }
}
```

- [ ] **Step 2: Implement `session/controller.ts`:**

```ts
import * as vscode from "vscode";
import { AnalyzerClient, AnalyzerError, AnalyzeScope } from "../analyzer/client";
import { InspectionV2, SchemaError } from "../core/schema";
import { initialSessionState, reduceSession, SessionState } from "../core/sessionState";
import { PendingRun, RunCoalescer } from "../core/runCoalescer";
import { DismissalsStore } from "./dismissalsStore";
import { PluStanStatusBar } from "../ui/statusBar";

const SESSION_STORAGE_KEY = "plustan.session.v1";
const SAVE_DEBOUNCE_MS = 500;

interface PersistedSession {
  state: SessionState;
  inspections: [string, InspectionV2][];
  moduleByFile: [string, string][];
}

export class ReviewController implements vscode.Disposable {
  private state: SessionState = initialSessionState;
  private inspections = new Map<string, InspectionV2>();
  private moduleByFile = new Map<string, string>(); // absolute-ish file path → module name
  private readonly queue = new RunCoalescer();
  private running = false;
  private buildFailed = false;
  private debounceTimer: NodeJS.Timeout | undefined;
  private abort: AbortController | undefined;
  private readonly disposables: vscode.Disposable[] = [];

  constructor(
    private readonly client: AnalyzerClient,
    private readonly dismissals: DismissalsStore,
    private readonly statusBar: PluStanStatusBar,
    private readonly workspaceState: vscode.Memento,
    private readonly onStateChange: (state: SessionState, inspections: Map<string, InspectionV2>) => void,
    private readonly output: vscode.OutputChannel
  ) {
    this.disposables.push(
      vscode.workspace.onDidSaveTextDocument((doc) => this.handleSave(doc)),
      // Spec: a finding is stale as soon as its file is *edited*, not only
      // once saved — unsaved buffer changes already invalidate positions.
      vscode.workspace.onDidChangeTextDocument((e) => {
        if (this.state.phase === "active" && e.contentChanges.length > 0) {
          const file = this.fileKeyForDocument(e.document);
          if (this.moduleForDocument(e.document)) {
            this.dispatch({ type: "fileEdited", file });
          }
        }
      })
    );
  }

  get sessionState(): SessionState { return this.state; }
  get inspectionDocs(): Map<string, InspectionV2> { return this.inspections; }

  restore(): void {
    const saved = this.workspaceState.get<PersistedSession>(SESSION_STORAGE_KEY);
    if (saved && saved.state.phase === "active") {
      this.state = saved.state;
      this.inspections = new Map(saved.inspections);
      this.moduleByFile = new Map(saved.moduleByFile);
      void vscode.commands.executeCommand("setContext", "plustan.sessionActive", true);
      this.publish();
    }
  }

  /**
   * Start a review. Scope per spec: the whole workspace or a chosen set of
   * onchain modules. `scopeArg === "all"` (or a workspace with ≤1 module)
   * skips the picker — used programmatically and by the integration test.
   */
  async startReview(scopeArg?: "all" | string[]): Promise<void> {
    // Capabilities handshake: old binaries and future schemas fail here, up front.
    let moduleByFile: Map<string, string>;
    try {
      await this.client.capabilities();
      const list = await this.client.listOnchain();
      moduleByFile = new Map(list.modules.map((m) => [m.file, m.moduleName]));
    } catch (error) {
      await this.explainStartFailure(error);
      return;
    }

    if (scopeArg === undefined && moduleByFile.size > 1) {
      const picks = await vscode.window.showQuickPick(
        [...moduleByFile.values()].sort().map((moduleName) => ({ label: moduleName, picked: true })),
        { canPickMany: true, placeHolder: "Modules to review (all preselected)" }
      );
      if (!picks) {
        return; // user cancelled
      }
      scopeArg = picks.map((p) => p.label);
    }
    if (Array.isArray(scopeArg)) {
      const wanted = new Set(scopeArg);
      moduleByFile = new Map([...moduleByFile.entries()].filter(([, m]) => wanted.has(m)));
    }
    this.moduleByFile = moduleByFile;

    this.dispatch({ type: "sessionStarted", startedAt: new Date().toISOString() });
    await vscode.commands.executeCommand("setContext", "plustan.sessionActive", true);
    this.queue.request({ kind: "workspace" });
    void this.pump();
  }

  async endReview(): Promise<void> {
    this.dispatch({ type: "sessionEnded" });
    await vscode.commands.executeCommand("setContext", "plustan.sessionActive", false);
    this.abort?.abort();
    const c = this.state.findings;
    this.output.appendLine(
      `Plu-Stan review ended: ${Object.values(c).filter((f) => f.status === "fixed").length} fixed, ` +
      `${Object.values(c).filter((f) => f.status === "open" || f.status === "stale").length} open, ` +
      `${Object.values(c).filter((f) => f.status === "dismissed").length} dismissed.`
    );
  }

  async dismiss(fingerprint: string, inspectionId: string, note?: string): Promise<void> {
    await this.dismissals.add({ fingerprint, inspectionId, note, dismissedAt: new Date().toISOString() });
    this.dispatch({ type: "findingDismissed", fingerprint });
  }

  async undismiss(fingerprint: string): Promise<void> {
    await this.dismissals.remove(fingerprint);
    this.dispatch({ type: "findingUndismissed", fingerprint });
  }

  private handleSave(doc: vscode.TextDocument): void {
    if (this.state.phase !== "active") {
      return;
    }
    const moduleName = this.moduleForDocument(doc);
    if (!moduleName) {
      return;
    }
    this.dispatch({ type: "fileEdited", file: this.fileKeyForDocument(doc) });
    if (this.debounceTimer) {
      clearTimeout(this.debounceTimer);
    }
    this.debounceTimer = setTimeout(() => {
      this.queue.request({ kind: "module", moduleName });
      void this.pump();
    }, SAVE_DEBOUNCE_MS);
  }

  /** The backend reports workspace-relative paths; match on suffix. */
  private moduleForDocument(doc: vscode.TextDocument): string | undefined {
    for (const [file, moduleName] of this.moduleByFile) {
      if (doc.fileName.endsWith(file)) {
        return moduleName;
      }
    }
    return undefined;
  }

  private fileKeyForDocument(doc: vscode.TextDocument): string {
    for (const [file] of this.moduleByFile) {
      if (doc.fileName.endsWith(file)) {
        return file;
      }
    }
    return doc.fileName;
  }

  private async pump(): Promise<void> {
    if (this.running) {
      return;
    }
    const next = this.queue.takeNext();
    if (!next) {
      return;
    }
    this.running = true;
    this.publish();
    try {
      await this.runOne(next);
      this.buildFailed = false;
    } catch (error) {
      if (error instanceof AnalyzerError && error.kind === "build-failed") {
        this.buildFailed = true; // keep findings; status bar explains; next save retries
        this.output.appendLine(error.message);
      } else {
        this.output.appendLine(`Plu-Stan run failed: ${error instanceof Error ? error.message : String(error)}`);
        void vscode.window.showErrorMessage(`Plu-Stan: ${error instanceof Error ? error.message : String(error)}`);
      }
    } finally {
      this.running = false;
      this.publish();
      if (this.queue.size > 0) {
        void this.pump();
      }
    }
  }

  private async runOne(run: PendingRun): Promise<void> {
    this.abort = new AbortController();
    const scope: AnalyzeScope = run.kind === "workspace"
      ? { kind: "workspace" }
      : { kind: "module", moduleName: run.moduleName };
    const payload = await this.client.analyze(scope, this.abort.signal);

    for (const inspection of payload.inspections) {
      this.inspections.set(inspection.id, inspection);
    }
    const coveredFiles = run.kind === "workspace"
      ? [...this.moduleByFile.keys()]
      : [...this.moduleByFile.entries()].filter(([, m]) => m === run.moduleName).map(([f]) => f);
    // A module-scoped session still gets whole-project observations from a
    // workspace-kind run — keep only files inside the session scope.
    const covered = new Set(coveredFiles);
    const observations = payload.observations.filter((o) => covered.has(o.file));
    const dismissed = (await this.dismissals.load()).dismissals.map((d) => d.fingerprint);
    this.dispatch({
      type: "runCompleted",
      coveredFiles,
      observations,
      dismissedFingerprints: dismissed
    });
  }

  private dispatch(event: Parameters<typeof reduceSession>[1]): void {
    this.state = reduceSession(this.state, event);
    void this.workspaceState.update(SESSION_STORAGE_KEY, {
      state: this.state,
      inspections: [...this.inspections.entries()],
      moduleByFile: [...this.moduleByFile.entries()]
    } satisfies PersistedSession);
    this.publish();
  }

  private publish(): void {
    this.statusBar.update(this.state, this.running, this.buildFailed);
    this.onStateChange(this.state, this.inspections);
  }

  private async explainStartFailure(error: unknown): Promise<void> {
    const message = error instanceof SchemaError || error instanceof AnalyzerError
      ? error.message
      : `Plu-Stan handshake failed: ${error instanceof Error ? error.message : String(error)}`;
    const choice = await vscode.window.showErrorMessage(message, "Check for Updates");
    if (choice === "Check for Updates") {
      await vscode.commands.executeCommand("plustan.checkForUpdates");
    }
  }

  dispose(): void {
    this.abort?.abort();
    if (this.debounceTimer) {
      clearTimeout(this.debounceTimer);
    }
    for (const d of this.disposables) {
      d.dispose();
    }
  }
}
```

Note on the old-binary handshake path: a pre-v2 binary treats `capabilities` as a legacy positional project dir, prints an error to stderr, and emits no JSON — that surfaces as `AnalyzerError("no-json", …)` whose message already suggests "Check for Updates". No extra handling needed.

- [ ] **Step 3: Compile** — `cd vscode-plustan && npm run compile` → clean.

- [ ] **Step 4: Commit**

```bash
git add vscode-plustan/src/ui/statusBar.ts vscode-plustan/src/session/controller.ts
git commit -m "feat(ext): review-session controller with save-triggered re-runs"
```

---

### Task 12: Findings tree + package.json contributions

**Files:**
- Create: `vscode-plustan/src/ui/findingsTree.ts`
- Modify: `vscode-plustan/package.json`

- [ ] **Step 1: package.json contributions**

In `contributes.views.plustan`, replace the single view with (order matters — findings first):

```json
"views": {
  "plustan": [
    { "id": "plustanFindings", "name": "Findings" },
    { "id": "plustanFindingDetail", "name": "Finding Detail", "type": "webview" },
    { "id": "plustanOnchainModules", "name": "Onchain Modules" }
  ]
}
```

Add to `contributes.commands`:

```json
{ "command": "plustan.startReview", "title": "Plu-Stan: Start Review", "icon": "$(play)" },
{ "command": "plustan.endReview", "title": "Plu-Stan: End Review", "icon": "$(debug-stop)" },
{ "command": "plustan.toggleFindingsGrouping", "title": "Plu-Stan: Toggle Findings Grouping", "icon": "$(list-tree)" },
{ "command": "plustan.openFinding", "title": "Plu-Stan: Open Finding" },
{ "command": "plustan.dismissFinding", "title": "Plu-Stan: Dismiss Finding", "icon": "$(close)" },
{ "command": "plustan.undismissFinding", "title": "Plu-Stan: Restore Dismissed Finding", "icon": "$(redo)" }
```

Add `contributes.menus`:

```json
"menus": {
  "view/title": [
    { "command": "plustan.startReview", "when": "view == plustanFindings && plustan.sessionActive != true", "group": "navigation@1" },
    { "command": "plustan.endReview", "when": "view == plustanFindings && plustan.sessionActive == true", "group": "navigation@1" },
    { "command": "plustan.toggleFindingsGrouping", "when": "view == plustanFindings", "group": "navigation@2" }
  ],
  "view/item/context": [
    { "command": "plustan.dismissFinding", "when": "view == plustanFindings && viewItem == plustanFinding", "group": "inline" },
    { "command": "plustan.undismissFinding", "when": "view == plustanFindings && viewItem == plustanDismissedFinding", "group": "inline" }
  ]
}
```

- [ ] **Step 2: Implement `ui/findingsTree.ts`:**

```ts
import * as path from "node:path";
import * as vscode from "vscode";
import { InspectionV2 } from "../core/schema";
import { SessionFinding, SessionState, initialSessionState } from "../core/sessionState";

type Grouping = "severity" | "module";

const SEVERITY_ORDER = ["Error", "Warning", "PotentialBug", "Performance", "Style"];

class GroupItem extends vscode.TreeItem {
  constructor(label: string, readonly children: (GroupItem | FindingTreeItem)[], icon?: string) {
    super(label, vscode.TreeItemCollapsibleState.Expanded);
    if (icon) {
      this.iconPath = new vscode.ThemeIcon(icon);
    }
  }
}

export class FindingTreeItem extends vscode.TreeItem {
  constructor(readonly finding: SessionFinding, inspection: InspectionV2 | undefined) {
    super(`${path.basename(finding.file)}:${finding.startLine}`, vscode.TreeItemCollapsibleState.None);
    this.description = inspection ? inspection.name : finding.inspectionId;
    this.tooltip = `[${finding.inspectionId}] ${finding.file}:${finding.startLine}:${finding.startCol}`;
    this.contextValue = finding.status === "dismissed" ? "plustanDismissedFinding" : "plustanFinding";
    this.iconPath = new vscode.ThemeIcon(
      finding.status === "stale" ? "history"
        : finding.status === "fixed" ? "check"
        : finding.status === "dismissed" ? "circle-slash"
        : "warning"
    );
    if (finding.status === "stale") {
      this.description = `~ ${this.description ?? ""}`;
    }
    this.command = { command: "plustan.openFinding", title: "Open Finding", arguments: [this] };
  }
}

type Node = GroupItem | FindingTreeItem;

export class FindingsTreeProvider implements vscode.TreeDataProvider<Node> {
  private state: SessionState = initialSessionState;
  private inspections = new Map<string, InspectionV2>();
  private grouping: Grouping = "severity";
  private readonly emitter = new vscode.EventEmitter<Node | undefined>();
  readonly onDidChangeTreeData = this.emitter.event;

  setData(state: SessionState, inspections: Map<string, InspectionV2>): void {
    this.state = state;
    this.inspections = inspections;
    this.emitter.fire(undefined);
  }

  toggleGrouping(): void {
    this.grouping = this.grouping === "severity" ? "module" : "severity";
    this.emitter.fire(undefined);
  }

  getTreeItem(element: Node): vscode.TreeItem {
    return element;
  }

  getChildren(element?: Node): Node[] {
    if (element) {
      return element instanceof GroupItem ? element.children : [];
    }
    if (this.state.phase === "idle") {
      const idle = new vscode.TreeItem("No active review — press ▶ to start");
      return [idle as Node];
    }
    return this.buildRoots();
  }

  private buildRoots(): Node[] {
    const all = Object.values(this.state.findings);
    const active = all.filter((f) => f.status === "open" || f.status === "stale");
    const fixed = all.filter((f) => f.status === "fixed");
    const dismissed = all.filter((f) => f.status === "dismissed");

    const roots: Node[] = this.grouping === "severity"
      ? this.groupBySeverity(active)
      : this.groupByModule(active);

    if (fixed.length > 0) {
      const node = new GroupItem(`Fixed this session (${fixed.length})`, fixed.map((f) => this.item(f)), "check");
      node.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
      roots.push(node);
    }
    if (dismissed.length > 0) {
      const node = new GroupItem(`Dismissed (${dismissed.length})`, dismissed.map((f) => this.item(f)), "circle-slash");
      node.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
      roots.push(node);
    }
    return roots;
  }

  private groupBySeverity(findings: SessionFinding[]): Node[] {
    const severityOf = (f: SessionFinding): string => this.inspections.get(f.inspectionId)?.severity ?? "Warning";
    const severities = [...new Set(findings.map(severityOf))]
      .sort((a, b) => SEVERITY_ORDER.indexOf(a) - SEVERITY_ORDER.indexOf(b));
    return severities.map((severity) => {
      const inSeverity = findings.filter((f) => severityOf(f) === severity);
      const ruleIds = [...new Set(inSeverity.map((f) => f.inspectionId))].sort();
      const ruleNodes = ruleIds.map((ruleId) => {
        const inRule = inSeverity.filter((f) => f.inspectionId === ruleId).sort(bySpan);
        const name = this.inspections.get(ruleId)?.name ?? "";
        return new GroupItem(`${ruleId} ${name} (${inRule.length})`, inRule.map((f) => this.item(f)));
      });
      return new GroupItem(`${severity} (${inSeverity.length})`, ruleNodes, severityIcon(severity));
    });
  }

  private groupByModule(findings: SessionFinding[]): Node[] {
    const modules = [...new Set(findings.map((f) => f.moduleName))].sort();
    return modules.map((moduleName) => {
      const inModule = findings.filter((f) => f.moduleName === moduleName).sort(bySpan);
      return new GroupItem(`${moduleName} (${inModule.length})`, inModule.map((f) => this.item(f)), "symbol-module");
    });
  }

  private item(f: SessionFinding): FindingTreeItem {
    return new FindingTreeItem(f, this.inspections.get(f.inspectionId));
  }
}

function bySpan(a: SessionFinding, b: SessionFinding): number {
  return a.file.localeCompare(b.file) || a.startLine - b.startLine || a.startCol - b.startCol;
}

function severityIcon(severity: string): string {
  return severity === "Error" ? "error" : severity === "Performance" ? "zap" : "warning";
}
```

- [ ] **Step 3: Compile** — `npm run compile` → clean.

- [ ] **Step 4: Commit**

```bash
git add vscode-plustan/src/ui/findingsTree.ts vscode-plustan/package.json
git commit -m "feat(ext): findings tree with severity/module grouping and status sections"
```

---

### Task 13: Detail panel (webview view)

**Files:**
- Create: `vscode-plustan/src/ui/detailPanel.ts`

- [ ] **Step 1: Implement `ui/detailPanel.ts`:**

```ts
import * as vscode from "vscode";
import { InspectionV2 } from "../core/schema";
import { SessionFinding } from "../core/sessionState";

const RULES_URL = "https://github.com/input-output-hk/plu-stan/blob/main/RULES.md";

export class FindingDetailProvider implements vscode.WebviewViewProvider {
  static readonly viewId = "plustanFindingDetail";
  private view: vscode.WebviewView | undefined;
  private current: { finding: SessionFinding; inspection?: InspectionV2 } | undefined;

  constructor(
    private readonly onDismiss: (finding: SessionFinding) => void,
    private readonly onOpen: (finding: SessionFinding) => void
  ) {}

  resolveWebviewView(view: vscode.WebviewView): void {
    this.view = view;
    view.webview.options = { enableScripts: true };
    view.webview.onDidReceiveMessage((msg: { type: string }) => {
      if (!this.current) {
        return;
      }
      if (msg.type === "dismiss") {
        this.onDismiss(this.current.finding);
      } else if (msg.type === "open") {
        this.onOpen(this.current.finding);
      }
    });
    this.render();
  }

  showFinding(finding: SessionFinding, inspection: InspectionV2 | undefined): void {
    this.current = { finding, inspection };
    this.render();
    this.view?.show?.(true);
  }

  clear(): void {
    this.current = undefined;
    this.render();
  }

  private render(): void {
    if (!this.view) {
      return;
    }
    this.view.webview.html = this.current
      ? findingHtml(this.current.finding, this.current.inspection)
      : emptyHtml();
  }
}

function esc(text: string): string {
  return text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

function emptyHtml(): string {
  return wrap("<p class='muted'>Select a finding in the tree to see its explanation.</p>");
}

function findingHtml(f: SessionFinding, inspection: InspectionV2 | undefined): string {
  const name = inspection?.name ?? f.inspectionId;
  const severity = inspection?.severity ?? "";
  const why = inspection?.whyItMatters ?? inspection?.description ?? "";
  const solutions = (inspection?.solution ?? []).map((s) => `<li>${esc(s)}</li>`).join("");
  const docsLink = inspection?.docsAnchor
    ? `<a href="${RULES_URL}#${esc(inspection.docsAnchor)}">Rule documentation</a>`
    : "";
  const examples = inspection?.badExample && inspection?.goodExample
    ? `<h3>✗ Avoid</h3><pre>${esc(inspection.badExample)}</pre>
       <h3>✓ Prefer</h3><pre>${esc(inspection.goodExample)}</pre>`
    : "";
  return wrap(`
    <h2>${esc(f.inspectionId)} · ${esc(name)}</h2>
    <p class="muted">${esc(severity)} · ${esc(f.file)}:${f.startLine}:${f.startCol} · status: ${esc(f.status)}</p>
    ${why ? `<p>${esc(why)}</p>` : ""}
    ${examples}
    ${solutions ? `<h3>How to fix</h3><ul>${solutions}</ul>` : ""}
    <p>${docsLink}</p>
    <div class="actions">
      <button onclick="post('open')">Open file</button>
      <button onclick="post('dismiss')">Dismiss</button>
    </div>
  `);
}

function wrap(body: string): string {
  return `<!DOCTYPE html><html><head><meta charset="UTF-8">
    <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; script-src 'unsafe-inline';">
    <style>
      body { font-family: var(--vscode-font-family); font-size: 13px; padding: 8px; }
      pre { background: var(--vscode-textCodeBlock-background); padding: 6px; overflow-x: auto; }
      .muted { opacity: 0.7; }
      button { margin-right: 6px; }
    </style></head>
    <body>${body}
    <script>const vscode = acquireVsCodeApi(); function post(type) { vscode.postMessage({ type }); }</script>
    </body></html>`;
}
```

- [ ] **Step 2: Compile** — `npm run compile` → clean.

- [ ] **Step 3: Commit**

```bash
git add vscode-plustan/src/ui/detailPanel.ts
git commit -m "feat(ext): finding detail panel rendering backend-shipped rule docs"
```

---

### Task 14: Diagnostics from session state + dismiss code action + extension wiring

**Files:**
- Create: `vscode-plustan/src/diagnostics.ts`
- Modify: `vscode-plustan/src/extension.ts`

- [ ] **Step 1: Implement `src/diagnostics.ts`:**

```ts
import * as path from "node:path";
import * as vscode from "vscode";
import { InspectionV2 } from "./core/schema";
import { SessionFinding, SessionState } from "./core/sessionState";

export interface PluStanDiagnostic extends vscode.Diagnostic {
  plustanFingerprint?: string;
  plustanInspectionId?: string;
}

export function publishSessionDiagnostics(
  state: SessionState,
  inspections: Map<string, InspectionV2>,
  workspaceRoot: string,
  collection: vscode.DiagnosticCollection
): void {
  collection.clear();
  const byFile = new Map<string, PluStanDiagnostic[]>();

  for (const finding of Object.values(state.findings)) {
    if (finding.status !== "open" && finding.status !== "stale") {
      continue;
    }
    const inspection = inspections.get(finding.inspectionId);
    const filePath = path.isAbsolute(finding.file) ? finding.file : path.join(workspaceRoot, finding.file);
    const stalePrefix = finding.status === "stale" ? "(stale) " : "";
    const summary = inspection ? `${inspection.name} — ${inspection.description}` : "";
    const diagnostic: PluStanDiagnostic = new vscode.Diagnostic(
      toRange(finding),
      `${stalePrefix}[${finding.inspectionId}] ${summary}`.trim(),
      mapSeverity(inspection?.severity)
    );
    diagnostic.source = "plu-stan";
    diagnostic.code = finding.inspectionId;
    diagnostic.plustanFingerprint = finding.fingerprint;
    diagnostic.plustanInspectionId = finding.inspectionId;
    const list = byFile.get(filePath) ?? [];
    list.push(diagnostic);
    byFile.set(filePath, list);
  }
  collection.set([...byFile.entries()].map(([f, ds]) => [vscode.Uri.file(f), ds]));
}

export function toRange(finding: SessionFinding): vscode.Range {
  const startLine = Math.max(0, finding.startLine - 1);
  const startCharacter = Math.max(0, finding.startCol - 1);
  const endLine = Math.max(startLine, finding.endLine - 1);
  const rawEnd = Math.max(0, finding.endCol - 1);
  const endCharacter = endLine === startLine ? Math.max(startCharacter + 1, rawEnd) : rawEnd;
  return new vscode.Range(startLine, startCharacter, endLine, endCharacter);
}

function mapSeverity(severity: string | undefined): vscode.DiagnosticSeverity {
  switch (severity) {
    case "Error": return vscode.DiagnosticSeverity.Error;
    case "Warning":
    case "PotentialBug":
    case "Performance": return vscode.DiagnosticSeverity.Warning;
    default: return vscode.DiagnosticSeverity.Information;
  }
}

export class DismissCodeActionProvider implements vscode.CodeActionProvider {
  provideCodeActions(
    _document: vscode.TextDocument,
    _range: vscode.Range,
    context: vscode.CodeActionContext
  ): vscode.CodeAction[] {
    const actions: vscode.CodeAction[] = [];
    for (const diagnostic of context.diagnostics as PluStanDiagnostic[]) {
      if (diagnostic.source !== "plu-stan" || !diagnostic.plustanFingerprint) {
        continue;
      }
      const action = new vscode.CodeAction("Plu-Stan: Dismiss this finding", vscode.CodeActionKind.QuickFix);
      action.diagnostics = [diagnostic];
      action.command = {
        command: "plustan.dismissFinding",
        title: "Dismiss",
        arguments: [{ fingerprint: diagnostic.plustanFingerprint, inspectionId: diagnostic.plustanInspectionId }]
      };
      actions.push(action);
    }
    return actions;
  }
}
```

- [ ] **Step 2: Rewire `src/extension.ts`**

This is a refactor of the existing file; keep `downloadManager.ts` and all binary-resolution logic (`resolveSettings`, `isEffectivelyConfigured`, `maybeAutoCheckForUpdates`, `offerDownload` flow, `maybeRevealOnchainView`, `readSettings`, `saveWorkspaceBeforeRun`, `getWorkspaceFolder*`) exactly as they are. Changes:

1. **Delete** from `extension.ts`: `runPluStanJson`, `spawnCommand`, `parseJsonFromOutput`, `describePluStanFailure`, `isEnoentError`, `runListOnchain`, `runAnalyze`, `publishDiagnostics`, `toRange`, `mapSeverity`, and the `Inspection`/`Observation`/`AnalyzePayload`/`AnalysisSection` interfaces (replaced by `core/schema.ts` and `analyzer/client.ts`).
2. **Construct** in `activate` (after `output`/`diagnostics`):

```ts
const folder = ((): vscode.WorkspaceFolder | undefined => {
  try { return getWorkspaceFolder(); } catch { return undefined; }
})();

const client = new SpawnAnalyzerClient(
  () => {
    const f = getWorkspaceFolder();
    const settings = resolveSettings(f);
    return {
      binaryPath: settings.binaryPath,
      binaryPrefixArgs: [],
      cwd: settings.projectDir,
      hieDir: settings.hieDir,
      extraArgs: settings.extraArgs
    };
  },
  (line) => output.appendLine(line)
);

const statusBar = new PluStanStatusBar();
const findingsTree = new FindingsTreeProvider();
const detailPanel = new FindingDetailProvider(
  (finding) => { void controller.dismiss(finding.fingerprint, finding.inspectionId); },
  (finding) => { void openFinding(finding); }
);

const controller = new ReviewController(
  client,
  new DismissalsStore(folder ?? vscode.workspace.workspaceFolders![0]),
  statusBar,
  context.workspaceState,
  (state, inspections) => {
    findingsTree.setData(state, inspections);
    const root = folder?.uri.fsPath ?? "";
    publishSessionDiagnostics(state, inspections, root, diagnostics);
  },
  output
);
controller.restore();
```

3. **Register** the new UI + commands:

```ts
context.subscriptions.push(
  statusBar,
  controller,
  vscode.window.registerTreeDataProvider("plustanFindings", findingsTree),
  vscode.window.registerWebviewViewProvider(FindingDetailProvider.viewId, detailPanel),
  vscode.languages.registerCodeActionsProvider(
    { language: "haskell", scheme: "file" },
    new DismissCodeActionProvider(),
    { providedCodeActionKinds: [vscode.CodeActionKind.QuickFix] }
  ),
  vscode.commands.registerCommand("plustan.startReview", async (scopeArg?: "all" | string[]) => {
    if (!await saveWorkspaceBeforeRun()) { return; }
    const f = getWorkspaceFolderOrNotify();
    if (!f) { return; }
    const settings = await ensureBinaryConfigured(f, provider, resolveSettings);
    if (!settings) { return; }
    await controller.startReview(scopeArg);
  }),
  vscode.commands.registerCommand("plustan.endReview", () => controller.endReview()),
  vscode.commands.registerCommand("plustan.toggleFindingsGrouping", () => findingsTree.toggleGrouping()),
  vscode.commands.registerCommand("plustan.openFinding", async (item?: FindingTreeItem) => {
    if (!item) { return; }
    detailPanel.showFinding(item.finding, controller.inspectionDocs.get(item.finding.inspectionId));
    await openFinding(item.finding);
  }),
  vscode.commands.registerCommand("plustan.dismissFinding",
    async (arg?: FindingTreeItem | { fingerprint: string; inspectionId: string }) => {
      const target = arg instanceof FindingTreeItem
        ? { fingerprint: arg.finding.fingerprint, inspectionId: arg.finding.inspectionId }
        : arg;
      if (!target) { return; }
      const note = await vscode.window.showInputBox({
        prompt: "Optional note: why is this finding not applicable?",
        placeHolder: "e.g. credential-only comparison is intentional here"
      });
      await controller.dismiss(target.fingerprint, target.inspectionId, note || undefined);
    }),
  vscode.commands.registerCommand("plustan.undismissFinding", async (item?: FindingTreeItem) => {
    if (item) { await controller.undismiss(item.finding.fingerprint); }
  })
);

async function openFinding(finding: SessionFinding): Promise<void> {
  const f = getWorkspaceFolder();
  const filePath = path.isAbsolute(finding.file) ? finding.file : path.join(f.uri.fsPath, finding.file);
  const doc = await vscode.workspace.openTextDocument(filePath);
  const editor = await vscode.window.showTextDocument(doc, { preserveFocus: false });
  const range = toRange(finding);
  editor.revealRange(range, vscode.TextEditorRevealType.InCenter);
  editor.selection = new vscode.Selection(range.start, range.start);
}
```

4. **Rework the legacy commands** `plustan.runWorkspace` / `plustan.runModule`: keep them registered, but implement as one-shot runs through `client.analyze(...)` that publish diagnostics via a throwaway one-run `SessionState` (`reduceSession(reduceSession(initialSessionState, {type:"sessionStarted", startedAt: new Date().toISOString()}), {type:"runCompleted", coveredFiles:[...], observations: payload.observations, dismissedFingerprints: [...]})`) — no session is started, no auto-rerun armed. `plustan.refreshOnchainModules` switches to `client.listOnchain()`. The old `OnchainModulesProvider` stays as the provider for the (now secondary) `plustanOnchainModules` view; drop its `ActionItem` rows (Start/End/Refresh now live in the `view/title` menus) but keep the `MessageItem` unconfigured-binary hint.
5. Imports to add at the top of `extension.ts`:

```ts
import { SpawnAnalyzerClient } from "./analyzer/client";
import { ReviewController } from "./session/controller";
import { DismissalsStore } from "./session/dismissalsStore";
import { FindingsTreeProvider, FindingTreeItem } from "./ui/findingsTree";
import { FindingDetailProvider } from "./ui/detailPanel";
import { PluStanStatusBar } from "./ui/statusBar";
import { DismissCodeActionProvider, publishSessionDiagnostics, toRange } from "./diagnostics";
import { SessionFinding, initialSessionState, reduceSession } from "./core/sessionState";
```

- [ ] **Step 3: Compile and run unit tests** — `npm run compile && npm run test:unit` → clean, all PASS.

- [ ] **Step 4: Manual smoke in the Extension Development Host**

Launch: `code --extensionDevelopmentPath=$PWD/vscode-plustan <a plutus workspace>` (or the `cursor` equivalent). Verify: Start Review appears in the Findings view title → starting runs an analysis → findings appear grouped → clicking one opens the file and fills the detail panel → dismiss persists to `.plustan/dismissals.json` → saving an onchain file re-runs → status bar updates.

- [ ] **Step 5: Commit**

```bash
git add vscode-plustan/src/extension.ts vscode-plustan/src/diagnostics.ts
git commit -m "feat(ext): wire review cockpit into extension activation"
```

---

### Task 15: Integration smoke test (@vscode/test-electron)

**Files:**
- Create: `vscode-plustan/src/test/runTest.ts`, `vscode-plustan/src/test/suite/index.ts`, `vscode-plustan/src/test/suite/session.test.ts`
- Create: `vscode-plustan/test-fixtures/fake-plustan` (shell shim), `vscode-plustan/test-fixtures/workspace/` fixture
- Modify: `vscode-plustan/package.json`

- [ ] **Step 1: Install harness**

```bash
cd vscode-plustan && npm install --save-dev @vscode/test-electron@^2.4.0
```

Add script: `"test:integration": "npm run compile && node out/test/runTest.js"`.

- [ ] **Step 2: Create the executable shim** `test-fixtures/fake-plustan`:

```bash
#!/usr/bin/env bash
exec node "$(dirname "$0")/fake-plustan.js" "$@"
```

Then: `chmod +x vscode-plustan/test-fixtures/fake-plustan` (git preserves the executable bit).

- [ ] **Step 3: Create the fixture workspace**

- `test-fixtures/workspace/src/Fixture/Validator.hs`:

```haskell
module Fixture.Validator where
{-# ANN module ("onchain-contract" :: String) #-}

validator :: Bool
validator = True
```

- `test-fixtures/workspace/.vscode/settings.json`:

```json
{ "plustan.binaryPath": "${workspaceFolder}/../fake-plustan" }
```

**Note:** VS Code does not expand `${workspaceFolder}` in arbitrary settings — the extension reads the raw string. `readSettings` must therefore expand a leading `${workspaceFolder}` itself; add to `readSettings` in `extension.ts`:

```ts
const rawBinaryPath = config.get<string>("binaryPath", "").trim();
const binaryPath = rawBinaryPath.replace("${workspaceFolder}", folder.uri.fsPath);
```

(One-line behavior improvement, also useful to real users; mention it in the README table for `plustan.binaryPath`.)

- [ ] **Step 4: Write the harness**

`src/test/runTest.ts`:

```ts
import * as path from "node:path";
import { runTests } from "@vscode/test-electron";

async function main(): Promise<void> {
  const extensionDevelopmentPath = path.resolve(__dirname, "..", "..");
  const extensionTestsPath = path.resolve(__dirname, "suite", "index");
  const workspace = path.resolve(extensionDevelopmentPath, "test-fixtures", "workspace");
  await runTests({
    extensionDevelopmentPath,
    extensionTestsPath,
    launchArgs: [workspace, "--disable-extensions"]
  });
}

main().catch((err) => {
  console.error("Integration tests failed:", err);
  process.exit(1);
});
```

`src/test/suite/index.ts`:

```ts
import * as path from "node:path";
// require-form import: works regardless of the tsconfig's esModuleInterop setting
import Mocha = require("mocha");

export function run(): Promise<void> {
  const mocha = new Mocha({ ui: "bdd", timeout: 60_000, color: true });
  mocha.addFile(path.resolve(__dirname, "session.test.js"));
  return new Promise((resolve, reject) => {
    mocha.run((failures) => (failures > 0 ? reject(new Error(`${failures} tests failed`)) : resolve()));
  });
}
```

`src/test/suite/session.test.ts`:

```ts
import * as assert from "node:assert";
import * as vscode from "vscode";

async function poll<T>(fn: () => T | undefined, timeoutMs = 30_000): Promise<T> {
  const start = Date.now();
  for (;;) {
    const value = fn();
    if (value !== undefined) {
      return value;
    }
    if (Date.now() - start > timeoutMs) {
      throw new Error("poll timed out");
    }
    await new Promise((r) => setTimeout(r, 250));
  }
}

describe("review session (integration)", () => {
  it("start review produces diagnostics from the stub binary", async () => {
    // "all" skips the module-scope QuickPick (which would block a headless test)
    await vscode.commands.executeCommand("plustan.startReview", "all");
    const diags = await poll(() => {
      const all = vscode.languages.getDiagnostics()
        .flatMap(([, ds]) => ds)
        .filter((d) => d.source === "plu-stan");
      return all.length >= 2 ? all : undefined;
    });
    assert.strictEqual(diags.length, 2);
    assert.ok(diags[0].message.includes("PLU-STAN-04"));
  });
});
```

- [ ] **Step 5: Run** — `cd vscode-plustan && npm run test:integration`
Expected: downloads a VS Code build once, then `1 passing`.

If `plustan.startReview` stalls on the binary-configured guard: the fixture settings provide `plustan.binaryPath`, so `ensureBinaryConfigured` passes; if the run instead fails on workspace-save, the fixture has no dirty editors, so `saveWorkspaceBeforeRun` returns true. Debug via the `Plu-Stan` output channel content printed in the test console.

- [ ] **Step 6: Commit**

```bash
git add vscode-plustan/src/test vscode-plustan/test-fixtures vscode-plustan/package.json vscode-plustan/package-lock.json
git commit -m "test(ext): integration smoke test with stub plustan binary"
```

---

### Task 16: Docs, versions, final verification

**Files:**
- Modify: `vscode-plustan/package.json` (version), `vscode-plustan/README.md`, `vscode-plustan/MARKETPLACE.md`, `CHANGELOG.md`

- [ ] **Step 1: Docs**

- `vscode-plustan/package.json`: `"version": "0.3.0"`.
- `vscode-plustan/README.md`: replace the Features section with the review-session workflow (Start Review → findings tree → detail panel → dismiss → auto re-run on save → End Review); document `.plustan/dismissals.json` (commit it to share dismissals); document the new commands and the `${workspaceFolder}` expansion in `plustan.binaryPath`; note that extension 0.3.x requires plustan ≥ 0.2.5.0 (schema v2) and that "Check for Updates" fetches it.
- `vscode-plustan/MARKETPLACE.md`: same feature-list refresh, marketing tone.
- Repo `CHANGELOG.md`: extension 0.3.0 entry.

- [ ] **Step 2: Full verification**

```bash
cabal build exe:plustan && cabal test --test-show-details=direct 2>&1 | tail -5
cd vscode-plustan && npm run compile && npm run test:unit && npm run test:integration
```
Expected: everything green. Also do one manual end-to-end pass against a real Plutus workspace with the locally built binary (`cabal list-bin exe:plustan` → set `plustan.binaryPath`).

- [ ] **Step 3: Commit**

```bash
git add vscode-plustan/package.json vscode-plustan/README.md vscode-plustan/MARKETPLACE.md CHANGELOG.md
git commit -m "docs: review-cockpit workflow docs; extension v0.3.0"
```

---

## Out of scope for this plan (later phases)

- Backend-computed quick fixes (`fix` field) — Phase 2.
- Ask-AI handoff adapters — Phase 3.
- CLI reading `.plustan/dismissals.json` for CI parity — Phase 3.
- Daemon backend / LSP — deliberately excluded by the spec.
