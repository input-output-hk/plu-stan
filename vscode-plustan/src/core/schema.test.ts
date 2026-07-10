import * as assert from "node:assert";
import { parseAnalyzePayload, parseCapabilities, parseListOnchain, SchemaError } from "./schema";

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
  it("rejects inspections without a string id as malformed", () => {
    assert.throws(
      () => parseAnalyzePayload({
        version: 2, runScope: "all", targetModule: null,
        inspections: [{ name: "x" }], observations: []
      }),
      (e: unknown) => e instanceof SchemaError && e.reason === "malformed"
    );
  });
  it("parses list-onchain payloads version-agnostically", () => {
    const p = parseListOnchain({
      version: 1,
      workspaceRoot: "/ws",
      hieDir: ".hie",
      modules: [{ moduleName: "V", file: "src/V.hs", annotationSource: "hi" }]
    });
    assert.strictEqual(p.modules.length, 1);
    assert.strictEqual(p.modules[0].moduleName, "V");
  });
  it("rejects list-onchain payloads without a modules array as malformed", () => {
    assert.throws(
      () => parseListOnchain({ version: 2, workspaceRoot: "/ws", hieDir: ".hie" }),
      (e: unknown) => e instanceof SchemaError && e.reason === "malformed"
    );
  });
});
