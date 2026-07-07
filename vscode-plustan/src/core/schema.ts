export const SUPPORTED_SCHEMA_VERSION = 2;

export interface InspectionV2 {
  id: string; name: string; description: string;
  solution: string[]; category: string[]; severity: string;
  whyItMatters?: string; badExample?: string; goodExample?: string; docsAnchor?: string;
}

export interface ObservationV2 {
  id: string; inspectionId: string; fingerprint: string;
  file: string; moduleName: string;
  startLine: number; startCol: number; endLine: number; endCol: number;
}

export interface AnalyzePayloadV2 {
  version: number;
  runScope: "all" | "module";
  targetModule: string | null;
  inspections: InspectionV2[];
  observations: ObservationV2[];
}

export interface CapabilitiesPayload {
  schemaVersion: number;
  ghcVersion?: string;
  features: string[];
}

export interface OnchainModule { moduleName: string; file: string; annotationSource: string; }
export interface ListOnchainPayload { version: number; workspaceRoot: string; hieDir: string; modules: OnchainModule[]; }

export class SchemaError extends Error {
  constructor(readonly reason: "unsupported-version" | "malformed", message: string) {
    super(message);
    this.name = "SchemaError";
  }
}

function asRecord(raw: unknown, what: string): Record<string, unknown> {
  if (typeof raw !== "object" || raw === null || Array.isArray(raw)) {
    throw new SchemaError("malformed", `${what} is not an object`);
  }
  return raw as Record<string, unknown>;
}

function requireVersion(actual: unknown, what: string): void {
  if (actual !== SUPPORTED_SCHEMA_VERSION) {
    throw new SchemaError(
      "unsupported-version",
      `${what} has schema version ${String(actual)}; this extension requires ${SUPPORTED_SCHEMA_VERSION}. ` +
      `Run "Plu-Stan: Check for Updates" to fetch a matching binary.`
    );
  }
}

export function parseAnalyzePayload(raw: unknown): AnalyzePayloadV2 {
  const o = asRecord(raw, "analyze payload");
  requireVersion(o.version, "analyze payload");
  if (!Array.isArray(o.inspections) || !Array.isArray(o.observations)) {
    throw new SchemaError("malformed", "analyze payload: missing inspections/observations arrays");
  }
  for (const obs of o.observations) {
    const r = asRecord(obs, "observation");
    for (const key of ["fingerprint", "inspectionId", "file", "moduleName"]) {
      if (typeof r[key] !== "string") {
        throw new SchemaError("malformed", `observation: missing string field '${key}'`);
      }
    }
    for (const key of ["startLine", "startCol", "endLine", "endCol"]) {
      if (typeof r[key] !== "number") {
        throw new SchemaError("malformed", `observation: missing numeric field '${key}'`);
      }
    }
  }
  return o as unknown as AnalyzePayloadV2;
}

export function parseCapabilities(raw: unknown): CapabilitiesPayload {
  const o = asRecord(raw, "capabilities payload");
  requireVersion(o.schemaVersion, "plustan binary");
  if (!Array.isArray(o.features)) {
    throw new SchemaError("malformed", "capabilities payload: missing features array");
  }
  return o as unknown as CapabilitiesPayload;
}

export function parseListOnchain(raw: unknown): ListOnchainPayload {
  const o = asRecord(raw, "list-onchain payload");
  if (!Array.isArray(o.modules)) {
    throw new SchemaError("malformed", "list-onchain payload: missing modules array");
  }
  return o as unknown as ListOnchainPayload;
}
