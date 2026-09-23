#!/usr/bin/env python3
"""Run the research acceptance corpus through the shipped JSON CLI.

A rule counts only if it has both positive and negative cases and every case
passes. Missing modules, stale/missing fixtures, unknown inspections, malformed
output, empty baselines and incomplete inventory are errors, never clean scans.
"""
import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import subprocess
import sys

ROOT = Path(__file__).resolve().parent.parent

def run(argv):
    p = subprocess.run(argv, cwd=ROOT, text=True, capture_output=True)
    if p.returncode:
        raise RuntimeError(f'{argv}: exit {p.returncode}\n{p.stdout}\n{p.stderr}')
    return p.stdout

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--no-build', action='store_true', help='Use an already built checkout (CI builds first)')
    parser.add_argument('--output', type=Path, default=ROOT / 'cwe-results.json')
    args = parser.parse_args()
    manifest = json.loads((ROOT / 'test/cwe-conformance.json').read_text())
    eligible = manifest['eligible_rules']
    if len(eligible) != len(set(eligible)) or len(eligible) != 23:
        raise RuntimeError('Expected the pinned inventory of 23 distinct research rules')
    snapshot = ROOT / 'test/research-source'
    inventory = {f.stem for f in (snapshot/'rules').glob('*.md') if f.stem != 'README'}
    if inventory != set(eligible):
        raise RuntimeError('Manifest differs from the pinned research rule inventory')
    for relative, expected_hash in manifest['source_hashes'].items():
        if hashlib.sha256((snapshot/relative).read_bytes()).hexdigest() != expected_hash:
            raise RuntimeError(f'Research snapshot changed: {relative}')
    if not args.no_build:
        print(run(['cabal', 'build', 'all', '--enable-tests', '-ffixtures']))
    binary = run(['cabal', 'list-bin', 'exe:plustan']).strip()
    modules = {c.get('module', 'Target.Research') for c in manifest['cases']}
    payloads = {}
    for module in modules:
        fixture = ROOT / 'target' / (module.replace('.', '/') + '.hs')
        hie = ROOT / '.hie' / (module.replace('.', '/') + '.hie')
        if not hie.exists() or hie.stat().st_mtime < fixture.stat().st_mtime:
            raise RuntimeError(f'Missing or stale fixture: {hie}; build before testing')
        payload = json.loads(run([binary, 'analyze', '--json', '--module', module]))
        if payload.get('targetModule') != module or payload.get('version') != 2:
            raise RuntimeError(f'Unexpected analysis scope/schema for {module}')
        payloads[module] = payload
    results = []
    for case in manifest['cases']:
        if case['rule'] not in eligible:
            raise RuntimeError(f'Unknown rule: {case}')
        module = case.get('module', 'Target.Research')
        fixture = ROOT / 'target' / (module.replace('.', '/') + '.hs')
        lines = fixture.read_text().splitlines()
        binding = case['binding']
        if binding.startswith('splice:'):
            starts = [i+1 for i,s in enumerate(lines) if s.startswith(binding[7:])]
            end = starts[0] if len(starts) == 1 else -1
        else:
            starts = [i+1 for i,s in enumerate(lines) if re.match(re.escape(binding)+r'\b', s) and ' :: ' not in s]
            end = len(lines)
            if len(starts) == 1:
                for i in range(starts[0], len(lines)):
                    if lines[i] and not lines[i][0].isspace() and not lines[i].startswith('--'):
                        end = i
                        break
        if len(starts) != 1 or end < starts[0]:
            raise RuntimeError(f'Ambiguous/missing fixture range: {binding}')
        if any('stan-ignore: ' + case['inspection'] in line for line in lines[max(0,starts[0]-2):end]):
            raise RuntimeError(f'Acceptance fixture suppresses its own diagnostic: {binding}')
        payload = payloads[module]
        if case['inspection'] not in {i['id'] for i in payload['inspections']}:
            raise RuntimeError(f'Inspection not enabled: {case["inspection"]}')
        observations = [o for o in payload['observations'] if o['inspectionId'] == case['inspection']
                        and o['moduleName'] == module and starts[0] <= o['startLine'] <= end]
        passed = bool(observations) == case['expected']
        results.append(dict(case, passed=passed, actual=bool(observations), start=starts[0], end=end,
                            observations=observations))
        if not passed:
            print(f'FAIL {case["rule"]}: {binding}: expected={case["expected"]}, actual={bool(observations)}')
    rules = []
    for name in eligible:
        cases = [c for c in results if c['rule'] == name]
        accepted = bool(cases) and {c['expected'] for c in cases} == {True,False} and all(c['passed'] for c in cases)
        rules.append(dict(rule=name, accepted=accepted, cases=len(cases), passing=sum(c['passed'] for c in cases)))
    accepted = sum(r['accepted'] for r in rules)
    digest = hashlib.sha256()
    for p in sorted([*ROOT.glob('src/**/*.hs'), *ROOT.glob('target/**/*.hs'), ROOT/'test/cwe-conformance.json']):
        digest.update(str(p.relative_to(ROOT)).encode()); digest.update(p.read_bytes())
    report = dict(research_commit=manifest['research_commit'], implementation_commit=run(['git','rev-parse','HEAD']).strip(),
                  working_tree_dirty=bool(run(['git','status','--porcelain'])), source_sha256=digest.hexdigest(),
                  compiler=run(['ghc','--numeric-version']).strip(), binary_sha256=hashlib.sha256(Path(binary).read_bytes()).hexdigest(),
                  eligible=len(eligible),accepted=accepted,percentage=100*accepted/len(eligible),
                  cases=len(results),passing=sum(c['passed'] for c in results),rules=rules,results=results)
    args.output.parent.mkdir(parents=True,exist_ok=True)
    args.output.write_text(json.dumps(report,indent=2)+'\n')
    print(f'{report["passing"]}/{len(results)} cases; {accepted}/{len(eligible)} accepted rules ({report["percentage"]:.1f}%)')
    return 0 if accepted >= math.ceil(.8*len(eligible)) and all(c['passed'] for c in results) else 1

if __name__ == '__main__':
    try:
        sys.exit(main())
    except (RuntimeError, KeyError, ValueError, OSError) as e:
        print(f'Conformance run failed: {e}',file=sys.stderr)
        sys.exit(2)
