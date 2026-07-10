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
