import * as path from "node:path";
import * as fs from "node:fs";

export interface FindingPathContext {
  /** Directory the analyzer runs plu-stan in (cwd). May be the package dir or the workspace root. */
  projectDir: string;
  /** `hieDir` from settings; relative (to projectDir) or absolute. */
  hieDir: string;
  /** The VS Code workspace folder. */
  workspaceRoot: string;
}

/**
 * Resolve a plu-stan finding's `file` — emitted *relative to the package
 * directory* (e.g. "src/Cardano/Djed/OnChain/Oracle.hs") — to an absolute path.
 *
 * The package dir isn't given to us directly, and it need not equal any single
 * setting: a user may set `projectDir` to the package (then it's projectDir), or
 * leave `projectDir` at the workspace root and point `hieDir` at <package>/.hie
 * (then it's dirname(hieDir)). So we try the likely bases in priority order and
 * return the first whose joined path exists on disk. The existence check makes a
 * wrong guess harmless — it simply falls through to the next base.
 *
 * No ambiguity handling on purpose: in a monorepo each package sits in its own
 * subdirectory, so a given package-relative path exists under exactly one base.
 */
export function resolveFindingPath(
  file: string,
  ctx: FindingPathContext,
  exists: (p: string) => boolean = fs.existsSync
): string {
  if (path.isAbsolute(file)) {
    return file;
  }

  const absHieDir = path.isAbsolute(ctx.hieDir)
    ? ctx.hieDir
    : path.join(ctx.projectDir, ctx.hieDir);

  const bases = dedupe([
    ctx.projectDir,           // projectDir pointed at the package (intended config)
    path.dirname(absHieDir),  // package inferred from a <package>/.hie layout
    ctx.workspaceRoot         // single-package project: package == workspace root
  ]);

  for (const base of bases) {
    const candidate = path.join(base, file);
    if (exists(candidate)) {
      return candidate;
    }
  }

  // Nothing matched on disk; keep the previous behaviour so a "cannot open"
  // error at least points somewhere sensible.
  return path.join(ctx.projectDir, file);
}

function dedupe(xs: string[]): string[] {
  return xs.filter((x, i) => xs.indexOf(x) === i);
}
