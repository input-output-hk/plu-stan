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

# --- curated: rule -> (coverage, inspection ids, gap class, divergence) ---
# coverage:  direct | narrower | adjacent | none
# gap_class: why it is not a direct match, and therefore what closing it costs.
#   trigger-gate          detection exists; a precondition suppresses it
#   scope-limited         detection exists; narrower shape than the rule
#   name-coverage         right idea, the rule's identifiers are not matched
#   remediation-mismatch  same trigger, different counter-evidence demanded
#   disjoint              overlapping theme, non-overlapping target
#   needs-new-analysis    requires analysis the tool does not do anywhere
#   deferred / blocked / spec-incomplete   see the note
MAPPING = [
    ("PrecisionLoss", "direct", ["PLU-STAN-16"], "", ""),
    ("EmptyStringADACheck", "direct", ["PLU-STAN-24"], "", ""),
    ("UnstableMakeIsData", "direct", ["PLU-STAN-23"], "", ""),
    ("ZipWithoutLengthCheck", "direct", ["PLU-STAN-26"], "",
     "Length check matched per zipped list; zipWith args fall back to a definition-wide length test"),

    ("MissingAddressValidation", "narrower", ["PLU-STAN-22"], "trigger-gate",
     "Only fires when >=3 of the 5 TxOut fields are already checked; the rule fires on absence of txOutAddress generally. A validator that checks nothing at all is flagged by the rule and silent here."),
    ("MissingStakingValidation", "narrower", ["PLU-STAN-14", "PLU-STAN-04"], "trigger-gate",
     "PLU-STAN-14 only fires when >=3 of the 5 TxOut fields are already checked. PLU-STAN-04 covers the related hash-vs-Address comparison that leaks staking rewards."),
    ("UnvalidatedReferenceScript", "narrower", ["PLU-STAN-13"], "trigger-gate",
     "Only fires when >=3 of the 5 TxOut fields are already checked."),
    ("UnvalidatedDatum", "narrower", ["PLU-STAN-19"], "trigger-gate",
     "Only fires when >=3 of the 5 TxOut fields are already checked."),
    ("TrashTokens", "narrower", ["PLU-STAN-15"], "trigger-gate",
     "Only fires when >=3 of the 5 TxOut fields are already checked; the rule's leq/geq subset comparisons are not detected (those names are local operator bindings in the analyser, not Plutus value functions)."),
    ("UncheckedRedeemer", "narrower", ["PLU-STAN-25"], "scope-limited",
     "Matches source text within a top-level definition rather than resolved names, so it also matches tokens in comments and strings; the script-input helper names it recognises are a fixed list. Reference inputs are not treated as a dependency: they carry no redeemer, so no redeemer check could ever cover them."),
    ("ReadOnlySpend", "narrower", ["PLU-STAN-27"], "scope-limited",
     "Requires all four field accessors plus txInInfoResolved, so a three-field identical recreation is not flagged."),

    ("ValidityRangeBound", "adjacent", ["PLU-STAN-12"], "needs-new-analysis",
     "The rule requires the ABSENCE of a range-length bound, (upper - lower) <= max. PLU-STAN-12 does no arithmetic or comparison analysis at all; it uses a proxy, isFiniteCheckNode, which needs a case/let mentioning lowerBound/upperBound AND LowerBound/UpperBound AND Finite. GAP: a validator that destructures both bounds and matches Finite but imposes no maximum duration is silent -- exactly the rule's target. PRECISION: ivFrom/ivTo, the spelling the rule's own examples use, appear 0 times in the analyser, so code written that way fails the proxy and is flagged whether or not it bounds the duration."),
    ("DatumComparisonOptimization", "adjacent", ["PLU-STAN-02"], "name-coverage",
     "The rule targets the shape 'case fromBuiltinData d of Just (Ctor fs) -> field comparisons', recommending 'd == toBuiltinData expected'. PLU-STAN-02 is a bare NameMeta match on unsafeFromBuiltinData. GAP BOTH WAYS: fromBuiltinData (the rule's primary spelling) is not matched and toBuiltinData appears 0 times, so neither the flagged shape nor the recommended one is recognised; conversely PLU-STAN-02 fires on every unsafeFromBuiltinData use whether or not a comparison follows. It measures decode cost, not the upcast-then-compare shape."),
    ("PartialUnvalidatedDatum", "adjacent", ["PLU-STAN-19"], "disjoint",
     "Disjoint rather than narrower. The rule fires when the datum IS extracted and SOME fields are validated but not all. PLU-STAN-19 fires only when the datum is untouched (no txOutDatum/OutputDatum/getDatumData token) and >=3 other fields are checked. Validating 2 of 5 datum fields sets txOutFieldDatumChecked = True and silences PLU-STAN-19 -- so satisfying PLU-STAN-19 is precisely what triggers the rule. The rule's entire target set is invisible."),
    ("IncompleteTokenValidation", "adjacent", ["PLU-STAN-09", "PLU-STAN-11"], "name-coverage",
     "The rule's pattern is a fold over flattenValue (txInfoMint ...) whose (symbol, name, quantity) tuple leaves a component as a wildcard. flattenValue appears 0 times in the analyser and nothing inspects tuple patterns for wildcard components. PLU-STAN-09/-11 catch a different incomplete-value idiom (valueOf in comparisons; currencySymbolValueOf on minted value). Same concern, different code shape."),
    ("StrictValueEquality", "adjacent", ["PLU-STAN-09", "PLU-STAN-15"], "disjoint",
     "Opposite polarity. The rule fires on an over-strict PRESENCE -- lovelaceValueOf (txOutValue x) == y, which can make a validator unsatisfiable under min-ADA or collateral change -- whereas PLU-STAN-15 fires on the ABSENCE of a value constraint. lovelaceValueOf appears 0 times in the analyser, so PLU-STAN-09 cannot match the rule's one-line pattern either."),
    ("UnvalidatedInputIndex", "adjacent", ["PLU-STAN-17"], "remediation-mismatch",
     "The triggers nearly coincide -- both start from a redeemer-derived index into txInfoInputs, and PLU-STAN-17 already has subtreeHasIndexingCall plus redeemerDecodeIndicators -- but the remediation each demands differs. The rule wants an NFT identity check on the selected input, valueOf (txOutValue (txInInfoResolved v)) cs tn >= 1, i.e. 'is this the RIGHT input?'. PLU-STAN-17 wants index UNIQUENESS. A validator that enforces uniqueness satisfies PLU-STAN-17 while still trusting an unverified input. PLU-STAN-17's counter-evidence is also a source comment ('plutstan uniqueness enforced'), so it can be silenced with no code change at all."),
    ("ListUniqueness", "adjacent", ["PLU-STAN-17"], "disjoint",
     "Effectively uncovered. The rule wants 'xs == nub xs' (or 'length xs == length (nub xs)') on lists of identity types -- PubKeyHash, ValidatorHash, Address, Credential -- whereas PLU-STAN-17 concerns redeemer-supplied indices, not credential lists. No uniqueness detection exists: the analyser's only nub references belong to STAN-0209, which flags nub as SLOW, so implementing this rule would put two inspections in direct opposition on the same code. A signer list with duplicates, amplifying a compromised key, is invisible today."),
    ("HelperFunctions", "adjacent", ["PLU-STAN-05"], "disjoint",
     "Disjoint. PLU-STAN-05 matches calls to LIBRARY higher-order functions (all/any/find/filter/foldl/foldr/elem/traverse_ from PlutusTx.Prelude, .List and .Foldable). The rule targets USER-DEFINED trivial wrappers -- 'f x = g x', or a helper that only pattern-matches. The rule's own invalid example, 'isAdmin pkh info = txSignedBy info pkh', contains no higher-order call, so PLU-STAN-05 cannot see it. Shared theme (inlining overhead), nothing else."),

    ("NoBurningLogic", "none", [], "blocked",
     "Implemented in open PR #39 as PLU-STAN-20, not merged: 20 of its own tests fail on its base and it carries 61 hlint hints. It does solve the (currencySymbol, tokenName) pairing that makes this rule hard."),
    ("ImmutableCredential", "direct", ["PLU-STAN-21"], "",
     "Covers both patterns the rule specifies: top-level credential constants, and credentials specialised into compiled code via applyCode/unsafeApplyCode/liftCode. Also filters to validator-reachable bindings, so an unused top-level credential is not flagged."),
    ("DoubleSatisfaction", "none", [], "deferred",
     "Deferred: proving the ABSENCE of uniqueness attribution is not tractable, and legitimate value aggregation over filtered outputs is common, so a shape-only rule would be noise."),
    ("FixedStructureMap", "none", [], "spec-incomplete",
     "Held: detection is trivial ('member <string> (<field> <datum>)') but the rule does not say when a fixed map key is a defect, so there is no criterion to implement."),
]


def read(rel):
    return (ROOT / rel).read_text()


def inspection_facts():
    ap = read("src/Stan/Inspection/AntiPattern.hs")
    an = read("src/Stan/Analysis/Analyser.hs")
    te = read("test/Test/Stan/Analysis/PlutusTx.hs")

    # Match the analyser name up to a word boundary: some analysers take extra
    # leading arguments (e.g. analyseImmutableCredential's precomputed span set),
    # and requiring the exact "insId hie node" tail silently lost those rows.
    dispatch = dict(re.findall(r"^\s{8}(\w+) -> (analyse\w+)\b", an, re.M))

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

    unknown = {i for _, _, ids, _, _ in MAPPING for i in ids} - set(insp)
    if unknown:
        sys.exit(f"mapping references unknown inspections: {sorted(unknown)}")

    rows = []
    for stem, cov, ids, gap, note in MAPPING:
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
            "gap_class": gap,
            "divergence_or_reason": note,
        })

    mapped = {i for _, _, ids, _, _ in MAPPING for i in ids}
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
            "gap_class": "tool-only",
            "divergence_or_reason": "No corresponding rule in Cardano-CWE-Research",
        })

    out = ROOT / "TRACEABILITY.csv"
    with out.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)

    render_readme(rows, insp)

    from collections import Counter
    counts = Counter(r["coverage"] for r in rows)
    covered = sum(1 for r in rows if r["rule_name"] and r["coverage"] != "none")
    print(f"wrote {out.relative_to(ROOT)}: {len(rows)} rows")
    print(f"rules covered: {covered}/{len(MAPPING)}")
    print("  " + "  ".join(f"{k}={v}" for k, v in sorted(counts.items())))


BEGIN = "<!-- BEGIN TRACEABILITY -->"
END = "<!-- END TRACEABILITY -->"

# Order the matrix by how much of the rule is actually covered.
TIER_ORDER = {"direct": 0, "narrower": 1, "adjacent": 2, "none": 3}


def render_readme(rows, insp):
    """Write the matrix into README.md between the markers, and refuse to
    finish if the README's own inspection table has drifted from the code."""
    readme_path = ROOT / "README.md"
    readme = readme_path.read_text()

    rule_rows = [r for r in rows if r["rule_name"]]
    rule_rows.sort(key=lambda r: (TIER_ORDER[r["coverage"]], r["rule_name"]))

    lines = [
        BEGIN,
        "",
        "| Research rule | Category | Coverage | Inspection(s) |",
        "|---|---|---|---|",
    ]
    for r in rule_rows:
        link = f'[{r["rule_name"]}]({r["rule_source"]})'
        cats = r["rule_categories"].replace(";", ", ").title().replace("Code-Quality", "Code quality")
        ids = ", ".join(f"`{i}`" for i in r["inspection_ids"].split(";") if i) or "—"
        lines.append(f'| {link} | {cats} | **{r["coverage"]}** | {ids} |')

    tool_only = [r for r in rows if r["coverage"] == "tool-only"]
    if tool_only:
        ids = ", ".join(f'`{r["inspection_ids"]}`' for r in tool_only)
        lines += [
            "",
            f"{len(tool_only)} inspections have no counterpart in the research rule set "
            f"(mostly UPLC efficiency, where the research rules skew towards security): {ids}.",
        ]
    lines += ["", END]

    start, end = readme.index(BEGIN), readme.index(END) + len(END)
    readme_path.write_text(readme[:start] + "\n".join(lines) + readme[end:])
    print(f"wrote README.md matrix: {len(rule_rows)} rules, {len(tool_only)} tool-only")

    # Drift guard: every registered inspection must have a row in the README's
    # hand-maintained Rules table. Checked with the table's row pattern
    # ("| PLU-STAN-NN |") rather than a bare substring search -- the ids also
    # appear inside the matrix rendered just above, which would make a plain
    # "is it mentioned anywhere" check satisfy itself.
    table = readme_path.read_text().split(BEGIN)[0]
    missing = sorted(pid for pid in insp if f"| {pid} |" not in table)
    if missing:
        sys.exit(
            "README.md's Rules table is missing rows for: "
            + ", ".join(missing)
            + "\nAdd them by hand -- the descriptions there are prose, not generated."
        )


if __name__ == "__main__":
    main()
