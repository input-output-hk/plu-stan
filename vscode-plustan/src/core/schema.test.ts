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
