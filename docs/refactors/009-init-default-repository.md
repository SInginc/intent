# Init Default Repository

Status: `implemented`

## Problem

`cmd_init()` requires explicit repository configuration or errors:

```r
stop("No repositories provided.")
```

The public `init()` signature already has `repos = NULL` as the default, and its
roxygen documentation says "Defaults to `getOption("repos")`." But the
implementation does not actually fall back to `getOption("repos")` — it errors.

This creates friction for new users who expect `intent::init("my_project")` to
just work, similar to `uv init` in Python. It also creates a mismatch between the
documented default and the actual behavior.

The roadmap (`docs/05-roadmap.md`, "Immediate Next Decisions") explicitly asks:

> Should `init()` have a default repository, or should it require explicit
> repository configuration?

## Goal

Resolve the default repository question with a clear, documented decision, and
align the implementation with that decision.

## Non-Goals

- Do not change how repositories are stored in `DESCRIPTION` (the
  `Config/intent/repos/` mechanism).
- Do not change how `load_intent_repos()` reads repositories during `add`,
  `remove`, or `sync`.
- Do not change the `repos` parameter signature of `init()`.
- Do not add repository management commands beyond `init`.

## Current State

`cmd_init()` in `R/commands.R:1-86` handles four cases for the `repos` argument:

| Has repos in DESCRIPTION? | `repos` argument | Behavior |
|---|---|---|
| Yes | Provided | Warning: repos already exist. Skipping. |
| No | Provided (named vector) | Write repos to DESCRIPTION. |
| Yes | NULL/empty | Read repos from DESCRIPTION. |
| No | NULL/empty | **Error: "No repositories provided."** |

The fourth case is the problem. The function errors instead of falling back to a
default.

`load_intent_repos()` in `R/utils.R:74-88` reads repos from DESCRIPTION and
optionally combines them with `more_repos`. It does not have a default fallback
either — it just returns an empty character vector if no repos are configured.

## Proposed Design

Three options, presented for decision:

### Option 1: Always Require Repos (Status Quo)

Keep the current behavior. `init()` errors without repos.

- **Pro:** Explicit. No hidden dependencies on external URLs or session state.
- **Pro:** No product decision needed — zero implementation effort.
- **Con:** Bad UX for new users. `intent::init()` fails on first use.
- **Con:** Requires updating the roxygen doc to remove the misleading "Defaults to
  `getOption("repos")`."

### Option 2: Default to Posit Package Manager (PPM)

Use `https://packagemanager.posit.co/cran/latest` as the default CRAN-like
repository when none is provided and none is configured.

- **Pro:** Best UX. `intent::init("my_project")` just works for the vast majority
  of R users.
- **Pro:** No interactive prompt — works in CI and headless sessions.
- **Pro:** PPM provides binary packages for Linux, macOS, and Windows, which makes
  `pak` installations faster.
- **Pro:** The package already has `Config/intent/repos/rspm` pointing to PPM in
  its own DESCRIPTION.
- **Con:** Hardcoded external URL adds a dependency on PPM availability.
- **Con:** Some organizations use internal mirrors and may not want PPM as a
  default.

### Option 3: Use `getOption("repos")`

When `repos = NULL`, call `getOption("repos")` and use the session's configured
repositories.

- **Pro:** Matches the current documented default.
- **Pro:** Respects the user's existing R configuration.
- **Pro:** Works with internal mirrors if the user has already configured them.
- **Con:** R's default `options("repos")` is `c(CRAN = "@CRAN@")`, which triggers
  an interactive prompt on first `install.packages()` — terrible UX in a
  non-interactive context.
- **Con:** The `@CRAN@` placeholder must be resolved or rejected. If rejected,
  we're back to Option 1 for most users.
- **Con:** Behavior depends on session state, which violates the project principle
  of "No Hidden Session Magic."

### Recommendation

**Option 2 (PPM default)**, with a fallback mechanism: if PPM is unreachable, error
with a clear message suggesting the user pass `repos` explicitly.

Rationale:

- PPM is the most widely used CRAN mirror and is maintained by Posit (the RStudio
  company).
- The project's own DESCRIPTION already references PPM.
- `pak` (the package's install engine) defaults to PPM as well, so `intent` would
  be consistent with its own backend.
- The UX improvement is substantial: `intent::init("my_project")` becomes a
  one-command onboarding experience.
- Option 3's `@CRAN@` problem makes it impractical without special-casing.

This is a product-level decision. The refactor document records the three options
and the recommendation, but the final decision should be made by the project
maintainer.

## Implementation Steps

1. If Option 2 is chosen: in `cmd_init()`, when `repos` is NULL/empty and no repos
   exist in DESCRIPTION, set `repos <- c(CRAN = "https://packagemanager.posit.co/cran/latest")`.
2. Update the error message in `cmd_init()` to guide users toward passing `repos`
   explicitly if they need a different repository.
3. Update `init()` roxygen documentation to reflect the actual default.
4. Update `docs/05-roadmap.md` to record the decision.
5. Add a test for the default repository fallback.

## Test Plan

- Unit test: `cmd_init(path = tmpdir, repos = NULL)` creates a project with the
  default PPM repository in DESCRIPTION.
- Unit test: `cmd_init(path = tmpdir, repos = c(CRAN = "https://example.com"))`
  uses the provided repository instead of the default.
- Unit test: `cmd_init(path = tmpdir)` on a directory that already has repos in
  DESCRIPTION preserves them and warns if new repos are passed.
- Run `devtools::test()` and `pre-commit run --all-files`.

## Acceptance Criteria

- `intent::init("my_project")` succeeds without explicit `repos`.
- The default repository is documented in the function help and the README.
- Users can still override the default by passing `repos = c(...)`.
- The roadmap's "Immediate Next Decisions" entry for this question is resolved.
- Tests and pre-commit pass.

## Result / Follow-Up Notes

Fill this in after implementation.

- Result: Chose Option 2 (PPM default). `cmd_init()` now defaults to
  `c(CRAN = "https://packagemanager.posit.co/cran/latest")` when neither the
  `repos` argument nor an existing DESCRIPTION provides repository
  configuration. Updated roxygen documentation and roadmap.
- Follow-up work: None. The roadmap's "Immediate Next Decisions" are now all
  resolved.
