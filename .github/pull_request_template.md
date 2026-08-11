## Summary
<!-- One-line description of what this PR does. -->

## Why
<!-- Link to the issue or context explaining the motivation. -->

## How to test
<!-- Checklist of commands and manual steps a reviewer can run. -->
- [ ] `Rscript -e "pkgload::load_all('.', quiet = TRUE)"` — clean load
- [ ] `Rscript -e "devtools::test()"` — full suite passes
- [ ] (new/changed provider) `expect_valid_provider()` passes for the provider
- [ ] (UI changes only) `Rscript -e "pkgload::load_all(); run_app()"` — flow works in PT-BR
- [ ] (CSS source changes only) `Rscript data-raw/build_css.R` and verify the bundle changed

## Checklist
- [ ] `docs/DECISIONS.md` updated (if the change is architectural)
- [ ] `docs/LESSONS.md` updated (if the work exposed a recurring pitfall)
- [ ] Tests added or updated for changed pure functions, provider results, DwC mapping, export, geo, or module server flows
- [ ] CSS rebuilt via `data-raw/build_css.R` when CSS source files changed
- [ ] Breaking changes flagged with `BREAKING:` prefix in the PR title
- [ ] Conventional Commit prefix (`feat:`, `fix:`, `refactor:`, `docs:`, `test:`, `chore:`, `perf:`) in the PR title

## Notes
<!-- Anything a reviewer needs to know that doesn't fit above. -->

🤖 If this PR was authored with [Claude Code](https://claude.com/claude-code), keep the Co-Authored-By trailer on the squash-merge commit.
