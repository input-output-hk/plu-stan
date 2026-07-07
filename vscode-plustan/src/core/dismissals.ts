export interface DismissalEntry {
  fingerprint: string;
  inspectionId: string;
  note?: string;
  dismissedAt: string;
}

export interface DismissalsFile {
  version: 1;
  dismissals: DismissalEntry[];
}

export function emptyDismissals(): DismissalsFile {
  return { version: 1, dismissals: [] };
}

export function parseDismissals(text: string): DismissalsFile {
  try {
    const raw = JSON.parse(text) as { version?: unknown; dismissals?: unknown };
    if (!Array.isArray(raw.dismissals)) {
      return emptyDismissals();
    }
    const dismissals = raw.dismissals.filter(
      (d): d is DismissalEntry =>
        typeof d === "object" && d !== null &&
        typeof (d as DismissalEntry).fingerprint === "string" &&
        typeof (d as DismissalEntry).inspectionId === "string"
    );
    return { version: 1, dismissals };
  } catch {
    return emptyDismissals();
  }
}

export function serializeDismissals(file: DismissalsFile): string {
  return JSON.stringify(file, null, 2) + "\n";
}

export function addDismissal(file: DismissalsFile, entry: DismissalEntry): DismissalsFile {
  return {
    version: 1,
    dismissals: [...file.dismissals.filter((d) => d.fingerprint !== entry.fingerprint), entry]
  };
}

export function removeDismissal(file: DismissalsFile, fingerprint: string): DismissalsFile {
  return { version: 1, dismissals: file.dismissals.filter((d) => d.fingerprint !== fingerprint) };
}
