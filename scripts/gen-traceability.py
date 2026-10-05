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
import json
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RESEARCH_COMMIT = "10eeea42c9b18d37cc2985c02b8b6987c0bb1c13"
BASE = f"https://github.com/input-output-hk/Cardano-CWE-Research/blob/{RESEARCH_COMMIT}/rules"

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
MAPPING = [('PrecisionLoss',
  'direct',
  ['PLU-STAN-16'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('EmptyStringADACheck',
  'direct',
  ['PLU-STAN-24'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('UnstableMakeIsData',
  'direct',
  ['PLU-STAN-23'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('ZipWithoutLengthCheck',
  'direct',
  ['PLU-STAN-26'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('MissingAddressValidation',
  'direct',
  ['PLU-STAN-22', 'PLU-STAN-28'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('MissingStakingValidation',
  'direct',
  ['PLU-STAN-14', 'PLU-STAN-04', 'PLU-STAN-29'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('UnvalidatedReferenceScript',
  'direct',
  ['PLU-STAN-13', 'PLU-STAN-30'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('UnvalidatedDatum',
  'direct',
  ['PLU-STAN-19', 'PLU-STAN-31'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('TrashTokens',
  'direct',
  ['PLU-STAN-15', 'PLU-STAN-32'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('UncheckedRedeemer',
  'direct',
  ['PLU-STAN-25', 'PLU-STAN-33'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('ReadOnlySpend',
  'direct',
  ['PLU-STAN-27', 'PLU-STAN-34'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('ValidityRangeBound',
  'direct',
  ['PLU-STAN-12', 'PLU-STAN-35'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('DatumComparisonOptimization',
  'direct',
  ['PLU-STAN-02', 'PLU-STAN-36'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('PartialUnvalidatedDatum',
  'adjacent',
  ['PLU-STAN-19'],
  'disjoint',
  'Not counted: checking that some datum validation exists does not establish validation of every '
  'datum field.'),
 ('IncompleteTokenValidation',
  'direct',
  ['PLU-STAN-09', 'PLU-STAN-11', 'PLU-STAN-37'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('StrictValueEquality',
  'direct',
  ['PLU-STAN-09', 'PLU-STAN-15', 'PLU-STAN-38'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('UnvalidatedInputIndex',
  'direct',
  ['PLU-STAN-17', 'PLU-STAN-39'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('ListUniqueness',
  'adjacent',
  ['PLU-STAN-17'],
  'disjoint',
  'Not counted: integer-index uniqueness is a different concern from identity-list uniqueness.'),
 ('HelperFunctions',
  'direct',
  ['PLU-STAN-05', 'PLU-STAN-40'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('NoBurningLogic',
  'none',
  [],
  'blocked',
  'Unmerged implementation in PR #39; excluded from the numerator, included in the denominator.'),
 ('ImmutableCredential',
  'direct',
  ['PLU-STAN-21'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.'),
 ('DoubleSatisfaction',
  'none',
  [],
  'deferred',
  'Not counted: operation-to-payment attribution needs a separately reviewed detection contract. '
  'Retained in the denominator.'),
 ('FixedStructureMap',
  'direct',
  ['PLU-STAN-41'],
  '',
  'Representative contract and limitations: docs/cwe-conformance.md. Measured by the CLI corpus in '
  'test/cwe-conformance.json; mapping alone does not assert passing acceptance.')]


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
        if ctor in {"ResearchRule", "PrecisionLossDivisionBeforeMultiply", "ZipWithoutLengthCheck"}:
            impl = "src/Stan/Analysis/Research.hs:researchFindings"
        elif ctor == "FindAst":
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

    corpus = json.loads(read("test/cwe-conformance.json"))
    for pid in insp:
        cases = [c for c in corpus["cases"] if c["inspection"] == pid]
        if cases:
            previous = tests.get(pid, {"spec": "", "cases": 0})
            tests[pid] = {
                "spec": ";".join(filter(None, [previous["spec"], "CLI:test/cwe-conformance.json"])),
                "cases": previous["cases"] + len(cases),
            }
    return insp, tests


def main():
    insp, tests = inspection_facts()
    upstream = {p.stem for p in (ROOT / "test/research-source/rules").glob("*.md") if p.stem != "README"}
    if upstream != {r[0] for r in MAPPING}:
        sys.exit("Pinned research inventory and traceability mapping differ")

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
            "fixtures": "target/Target/PlutusTx.hs;target/Target/Research.hs" if ids else "",
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
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()), lineterminator="\n")
        w.writeheader()
        w.writerows(rows)

    render_readme(rows, insp)

    from collections import Counter
    counts = Counter(r["coverage"] for r in rows)
    covered = sum(1 for r in rows if r["rule_name"] and r["coverage"] != "none")
    print(f"wrote {out.relative_to(ROOT)}: {len(rows)} rows")
    print(f"rules with any mapping (NOT implementation coverage): {covered}/{len(MAPPING)}")
    print("Run scripts/check-cwe-conformance.py for measured acceptance coverage.")
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
            f"(reported separately from research-rule coverage): {ids}.",
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
