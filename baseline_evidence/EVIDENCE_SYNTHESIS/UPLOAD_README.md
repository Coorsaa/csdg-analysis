# Sanitized analysis upload bundle

Study: `evidence_synthesis`

This directory is a derived upload bundle. The full private run remains in the analyst environment.
Serialized objects, raw/task/backend/model content, and row-level predictions are intentionally excluded.
When an allowlisted fold-assignment table contains direct identifiers, those identifiers are replaced by deterministic run-specific hashes, duplicate identifiers and row-level strata are removed, and rows are reordered by pseudonym. Other shareable aggregate or pseudonymous tables retain their analytic ordering and strata. Local absolute paths are redacted from all copied text files.

Checksum, sanitization, and structural checks do not by themselves make results manuscript-ready. Consult OUTPUT_AUDIT.md: release eligibility separately requires publication mode, an affirmative publication-settings declaration, manuscript readiness, no logged warnings or errors, and a clean structural audit.
