import * as path from "node:path";
import { runTests } from "@vscode/test-electron";

// Some dev shells (e.g. tooling that itself runs on Electron) leak
// ELECTRON_RUN_AS_NODE=1 into the environment. If that survives into the
// spawned VS Code process, its Electron binary runs as a plain Node CLI
// instead of launching the app — @vscode/test-electron then fails with a
// confusing "Cannot find module '<workspace path>'" error. Strip it before
// handing off so the child always launches as a real VS Code instance.
delete process.env.ELECTRON_RUN_AS_NODE;

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
