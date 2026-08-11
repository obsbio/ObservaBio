# Legacy reference scripts

These are the original monolithic R scripts that ran on Rogério's machine. They
are kept **only as a parity reference** for the Darwin Core standardization
pipeline (the app's output must match `zhouse_dwc_biodiv_260626.R`, the newest
one — see the parity test planned in SPEC §11). They are not part of the package
(`data-raw/` is R-build-ignored) and are not sourced at runtime.

**Security note.** These scripts previously contained a hardcoded IUCN Red List
API key in plain text. The literal was removed before the first commit (it never
entered git history) and replaced with `Sys.getenv("IUCN_KEY")`. The exposed key
must be **revoked on the IUCN site** by the owner (SPEC ADR-005).

| Script | Role |
| --- | --- |
| `zhouse_dwc_biodiv_260626.R` | Newest (v2.0). Canonical parity target. |
| `zhouse_dwc_090526.R` | v1.0 baseline. |
| `zhouse_dwc_2026-02-16.R`, `zhouse_dwc_minimax.R`, `zhouse_dwc_minimax_long.R` | Earlier iterations/variants. |
