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
