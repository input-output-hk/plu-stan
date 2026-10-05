# Plu-Stan extension changelog

## 0.3.5

* Refresh the Marketplace rule list for CLI 1.1.0, including the 14 new
  inspections (`PLU-STAN-28` through `PLU-STAN-41`) and their documented
  conformance scope.
* Explain the separate extension and CLI versions, how to update a managed
  CLI, and how to update an explicitly configured `plustan.binaryPath`.
* Synchronize extension manifest and lockfile versions at 0.3.5 and include
  extension release notes in the VSIX.
* Compile before packaging and exclude test runners and fixtures from the VSIX.

CLI 1.1.0 supplies the new inspections; the extension remains compatible with
schema-v2 CLI releases from 0.2.5.0 onwards. Run **Plu-Stan: Check for Updates**
to install a matching CLI, or update your configured `plustan.binaryPath`.

## 0.3.4

* Clarify that binary download and update notifications refer to the Plu-Stan
  CLI, separately from the extension version.
