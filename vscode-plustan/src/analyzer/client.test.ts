import * as assert from "node:assert";
import * as path from "node:path";
import { AnalyzerError, SpawnAnalyzerClient, classifyNoJsonFailure, parseJsonFromOutput } from "./client";

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
  it("passes --module for module-scoped analysis", async () => {
    // fake-plustan.js reports runScope "module" only when it sees a literal --module arg,
    // so this pins the exact flag spelling.
    const p = await client().analyze({ kind: "module", moduleName: "Fixture.Validator" });
    assert.strictEqual(p.runScope, "module");
  });
  it("lists onchain modules", async () => {
    const p = await client().listOnchain();
    assert.strictEqual(p.modules.length, 1);
    assert.strictEqual(p.modules[0].moduleName, "Fixture.Validator");
  });
  it("rejects with kind cancelled when the signal is already aborted", async () => {
    const controller = new AbortController();
    controller.abort();
    await assert.rejects(
      client().capabilities(controller.signal),
      (e: unknown) => e instanceof AnalyzerError && e.kind === "cancelled"
    );
  });
  it("classifies a missing binary as not-found", async () => {
    const bad = new SpawnAnalyzerClient(
      () => ({ binaryPath: "/nonexistent/plustan", binaryPrefixArgs: [], cwd: fixtures, hieDir: ".hie", extraArgs: [] }),
      () => { /* silent */ }
    );
    await assert.rejects(bad.capabilities(), (e: unknown) => e instanceof AnalyzerError && e.kind === "not-found");
  });
});

describe("parseJsonFromOutput", () => {
  it("finds JSON preceded by build noise", () => {
    const stdout = "Compiling Foo.hs\nLinking...\n" + JSON.stringify({ ok: true });
    assert.deepStrictEqual(parseJsonFromOutput(stdout), { ok: true });
  });
  it("finds JSON followed by trailing noise lines", () => {
    const stdout = JSON.stringify({ ok: true }) + "\nsome trailer that is not json\n";
    // Trailing non-JSON line means scanning from the end skips it and finds the JSON line before it.
    assert.deepStrictEqual(parseJsonFromOutput(stdout), { ok: true });
  });
  it("throws on pure garbage input", () => {
    assert.throws(() => parseJsonFromOutput("not json at all\nstill not json\n"));
  });
  it("does not let a stray numeric noise line shadow the payload", () => {
    const stdout = JSON.stringify({ ok: true }) + "\n42\n";
    assert.deepStrictEqual(parseJsonFromOutput(stdout), { ok: true });
  });
});

describe("classifyNoJsonFailure", () => {
  it("classifies GHC .hie version mismatches", () => {
    const err = classifyNoJsonFailure("", "Error: hie file versions do not match\n", 1);
    assert.strictEqual(err.kind, "ghc-mismatch");
  });
  it("classifies GHC panics as crash", () => {
    const err = classifyNoJsonFailure("", "ghc: panic! (the 'impossible' happened)\n", 1);
    assert.strictEqual(err.kind, "crash");
  });
  it("classifies a failing internal build as build-failed", () => {
    const err = classifyNoJsonFailure("", "src/Foo.hs:3:1: error: Parse error\n", 1);
    assert.strictEqual(err.kind, "build-failed");
  });
  it("falls back to no-json for an unrecognized silent failure", () => {
    const err = classifyNoJsonFailure("", "unknown command bogus\n", 1);
    assert.strictEqual(err.kind, "no-json");
  });
});
