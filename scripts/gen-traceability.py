#!/usr/bin/env python3
"""Regenerate TRACEABILITY.csv: Cardano-CWE-Research rules <-> PLU-STAN inspections.

Inspection facts (id, name, severity, analysis constructor, analyser function,
test spec, test-case count) are derived from the source tree, so they cannot
drift. The rule list and the coverage mapping are curated below: the mapping is
a judgement about how closely each inspection implements its rule, and has to be
reviewed by a human when inspections change.

Usage:  python3 scripts/gen-traceability.py
"""

import csv
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
BASE = "https://github.com/input-output-hk/Cardano-CWE-Research/blob/main/rules"

# --- curated: upstream rule -> categories --------------------------------
RULE_CATEGORIES = {
    "DatumComparisonOptimization": "PERFORMANCE",
    "DoubleSatisfaction": "SECURITY",
    "EmptyStringADACheck": "CODE-QUALITY",
    "FixedStructureMap": "CODE-QUALITY",
    "HelperFunctions": "CODE-QUALITY;PERFORMANCE",
    "ImmutableCredential": "CODE-QUALITY;SECURITY",
    "IncompleteTokenValidation": "SECURITY",
    "ListUniqueness": "SECURITY",
    "MissingAddressValidation": "SECURITY",
    "MissingStakingValidation": "SECURITY",
    "NoBurningLogic": "CODE-QUALITY;SECURITY",
    "PartialUnvalidatedDatum": "SECURITY",
    "PrecisionLoss": "CODE-QUALITY",
    "ReadOnlySpend": "PERFORMANCE;SECURITY",
    "StrictValueEquality": "SECURITY",
    "TrashTokens": "PERFORMANCE;SECURITY",
    "UncheckedRedeemer": "SECURITY",
    "UnstableMakeIsData": "SECURITY",
    "UnvalidatedDatum": "SECURITY",
    "UnvalidatedInputIndex": "SECURITY",
    "UnvalidatedReferenceScript": "PERFORMANCE",
    "ValidityRangeBound": "SECURITY",
    "ZipWithoutLengthCheck": "CODE-QUALITY;SECURITY",
}

# --- curated: rule -> (coverage, inspection ids, divergence/reason) ------
# coverage: direct | narrower | partial | none
MAPPING = [
    ("PrecisionLoss", "direct", ["PLU-STAN-16"], ""),
    ("EmptyStringADACheck", "direct", ["PLU-STAN-24"], ""),
    ("UnstableMakeIsData", "direct", ["PLU-STAN-23"], ""),
    ("ZipWithoutLengthCheck", "direct", ["PLU-STAN-26"],
     "Length check matched per zipped list; zipWith args fall back to a definition-wide length test"),
    ("MissingAddressValidation", "narrower", ["PLU-STAN-22"],
     "Only fires when >=3 of the 5 TxOut fields are already checked; rule fires on absence of txOutAddress generally"),
    ("MissingStakingValidation", "narrower", ["PLU-STAN-14", "PLU-STAN-04"],
     "PLU-STAN-14 only fires when >=3 of the 5 TxOut fields are already checked. PLU-STAN-04 covers the related hash-vs-Address comparison that leaks staking rewards"),
    ("UnvalidatedReferenceScript", "narrower", ["PLU-STAN-13"],
     "Only fires when >=3 of the 5 TxOut fields are already checked"),
    ("UnvalidatedDatum", "narrower", ["PLU-STAN-19"],
     "Only fires when >=3 of the 5 TxOut fields are already checked"),
    ("TrashTokens", "narrower", ["PLU-STAN-15"],
     "Only fires when >=3 of the 5 TxOut fields are already checked; leq/geq subset comparisons not detected"),
    ("UncheckedRedeemer", "narrower", ["PLU-STAN-25"],
     "Matches source text within a top-level definition, not resolved names; helper-name tokens are a fixed list"),
    ("ReadOnlySpend", "narrower", ["PLU-STAN-27"],
     "Requires all four field accessors plus txInInfoResolved; a 3-field recreation is not flagged"),
    ("ValidityRangeBound", "adjacent", ["PLU-STAN-12"],
     "Tool checks bound finiteness; rule requires an explicit (upper-lower) <= max duration constraint"),
    ("DatumComparisonOptimization", "adjacent", ["PLU-STAN-02"],
     "Tool targets unsafeFromBuiltinData/SOP cost, not the fromBuiltinData upcast-then-compare shape"),
    ("PartialUnvalidatedDatum", "adjacent", ["PLU-STAN-19"],
     "Rule targets partial field validation; inspection targets absent datum validation"),
    ("IncompleteTokenValidation", "adjacent", ["PLU-STAN-09", "PLU-STAN-11"],
     "No flattenValue detection, so the (symbol,name,quantity) tuple check is not verified"),
    ("StrictValueEquality", "adjacent", ["PLU-STAN-09", "PLU-STAN-15"],
     "No lovelaceValueOf detection"),
    ("UnvalidatedInputIndex", "adjacent", ["PLU-STAN-17"],
     "Redeemer-index detection exists; tool requires uniqueness, rule requires the selected input carry the expected NFT"),
    ("ListUniqueness", "adjacent", ["PLU-STAN-17"],
     "Rule wants nub-based uniqueness on credential lists; PLU-STAN-17 covers redeemer indices only. NOTE: STAN-0209 flags nub as slow, conflicting with this rule"),
    ("HelperFunctions", "adjacent", ["PLU-STAN-05"],
     "PLU-STAN-05 covers all/any/find; rule targets trivial pattern-matching wrappers"),
    ("NoBurningLogic", "none", [],
     "Implemented in open PR #39 as PLU-STAN-20, not merged: 20 of its own tests fail on its base and it carries 61 hlint hints"),
    ("ImmutableCredential", "none", [],
     "Implemented in open PR #40 as PLU-STAN-21 (stacked on #39), not merged. A Pattern-1-only version was prepared here and dropped in favour of the PR, which also covers Pattern 2 (applyCode/liftCode) and filters to validator-reachable bindings"),
    ("DoubleSatisfaction", "none", [],
     "Deferred: proving absence of uniqueness attribution is not tractable and legitimate value aggregation is common"),
    ("FixedStructureMap", "none", [],
     "Held: detection is trivial but the rule does not specify when a fixed map key is a defect"),
]


def read(rel):
    return (ROOT / rel).read_text()


def inspection_facts():
    ap = read("src/Stan/Inspection/AntiPattern.hs")
    an = read("src/Stan/Analysis/Analyser.hs")
    te = read("test/Test/Stan/Analysis/PlutusTx.hs")

    dispatch = dict(re.findall(r"^\s{8}(\w+) -> (analyse\w+) inspectionId hie node", an, re.M))

    insp = {}
    for m in re.finditer(
        r'^plustan(\d+) = mkAntiPatternInspection \(Id "(PLU-STAN-\d+)"\) "([^"]*)"\s*\n\s*(?:\(FindAst[^\n]*|(\w+))',
        ap, re.M,
    ):
        num, pid, name, ctor = m.groups()
        ctor = ctor or "FindAst"
        blk = re.search(r"^plustan%s = .*?(?=^plustan\d+ ::|\Z)" % num, ap, re.M | re.S)
        sev = re.search(r"severityL \.~ (\w+)", blk.group(0)) if blk else None
        if ctor == "FindAst":
            impl = "src/Stan/Inspection/AntiPattern.hs (declarative FindAst pattern)"
        elif ctor in dispatch:
            impl = f"src/Stan/Analysis/Analyser.hs:{dispatch[ctor]}"
        else:
            impl = f"src/Stan/Analysis/Analyser.hs (no dispatch entry for {ctor})"
        insp[pid] = {
            "name": name,
            "severity": sev.group(1) if sev else "PotentialBug",
            "ctor": ctor,
            "impl": impl,
        }

    tests = {}
    for m in re.finditer(
        r'plustan(\d+)Spec analysis = describe "(PLU-STAN-\d+)" \$ do(.*?)(?=\nplustan\d+Spec ::|\Z)',
        te, re.S,
    ):
        num, pid, body = m.groups()
        tests[pid] = {"spec": f"plustan{num}Spec", "cases": len(re.findall(r"^  it ", body, re.M))}

    return insp, tests


def main():
    insp, tests = inspection_facts()

    unknown = {i for _, _, ids, _ in MAPPING for i in ids} - set(insp)
    if unknown:
        sys.exit(f"mapping references unknown inspections: {sorted(unknown)}")

    rows = []
    for stem, cov, ids, note in MAPPING:
        rows.append({
            "rule_name": stem,
            "rule_categories": RULE_CATEGORIES.get(stem, ""),
            "rule_source": f"{BASE}/{stem}.md",
            "coverage": cov,
            "inspection_ids": ";".join(ids),
            "inspection_names": ";".join(insp[i]["name"] for i in ids),
            "inspection_severities": ";".join(insp[i]["severity"] for i in ids),
            "analysis_constructor": ";".join(insp[i]["ctor"] for i in ids),
            "implementation": ";".join(insp[i]["impl"] for i in ids),
            "fixtures": "target/Target/PlutusTx.hs" if ids else "",
            "test_spec": ";".join(tests.get(i, {}).get("spec", "") for i in ids),
            "test_cases": ";".join(str(tests.get(i, {}).get("cases", "")) for i in ids),
            "divergence_or_reason": note,
        })

    mapped = {i for _, _, ids, _ in MAPPING for i in ids}
    for pid in sorted(insp, key=lambda x: int(x.split("-")[-1])):
        if pid in mapped:
            continue
        rows.append({
            "rule_name": "", "rule_categories": "", "rule_source": "",
            "coverage": "tool-only",
            "inspection_ids": pid,
            "inspection_names": insp[pid]["name"],
            "inspection_severities": insp[pid]["severity"],
            "analysis_constructor": insp[pid]["ctor"],
            "implementation": insp[pid]["impl"],
            "fixtures": "target/Target/PlutusTx.hs",
            "test_spec": tests.get(pid, {}).get("spec", ""),
            "test_cases": str(tests.get(pid, {}).get("cases", "")),
            "divergence_or_reason": "No corresponding rule in Cardano-CWE-Research",
        })

    out = ROOT / "TRACEABILITY.csv"
    with out.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)

    from collections import Counter
    counts = Counter(r["coverage"] for r in rows)
    covered = sum(1 for r in rows if r["rule_name"] and r["coverage"] != "none")
    print(f"wrote {out.relative_to(ROOT)}: {len(rows)} rows")
    print(f"rules covered: {covered}/{len(MAPPING)}")
    print("  " + "  ".join(f"{k}={v}" for k, v in sorted(counts.items())))


if __name__ == "__main__":
    main()
