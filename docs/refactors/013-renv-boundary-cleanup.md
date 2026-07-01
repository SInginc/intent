# Renv Boundary Cleanup

Status: `implemented`

## Problem

The architecture audit found three renv boundary violations in core logic:

1. **`resolve_project()` calls `renv::project()`** (`R/utils.R:66`) — The
   renv boundary rules (docs/04-renv-boundary.md, rule #2) state: "Core
   functions should not call `renv::project()`." This is the exact pattern
   the rule prohibits. The call exists as a last-resort fallback after
   explicit project resolution fails.

2. **`load_intent_repos()` sets global `options(repos = repos)`**
   (`R/utils.R:102`) — This modifies global session state without user
   awareness, violating Principle #5 (No Hidden Session Magic). The function
   is called by `intent_install()`, `intent_snapshot()`, and
   `intent_restore()`, meaning every backend operation mutates the global
   `repos` option as a side effect.

3. **Stale imports in `R/intent-package.R`** — `renv::deactivate` and
   `renv::load` are imported via `@importFrom` but are never used anywhere
   in the package. These are carry-over from an earlier implementation.

## Goal

Resolve all three boundary violations so the package fully satisfies the
renv boundary rules documented in `docs/04-renv-boundary.md`.

After this refactor, the only `renv` calls outside `backend.R` should be zero.
The only global state mutation should be explicitly intentional and documented.

## Non-Goals

- Do not change the project resolution algorithm (the order: explicit →
  cwd → parent search → fallback).
- Do not change how repositories are read from DESCRIPTION.
- Do not rename backend adapter functions (defer to a separate naming
  refactor).

## Current State

**`resolve_project()`** (`R/utils.R:34-71`):

```r
resolve_project <- function(project = NULL) {
  if (!is.null(project)) { ... return(project) }
  project <- find_project_from(getwd())
  if (!is.null(project)) { return(project) }
  active_project <- tryCatch(
    renv::project(),          # <-- boundary violation
    error = function(e) NULL
  )
  if (!is.null(active_project)) { ... return(active_project) }
  stop("No intent project found. ...")
}
```

**`load_intent_repos()`** (`R/utils.R:88-103`):

```r
load_intent_repos <- function(project, more_repos = NULL) {
  ...
  if (length(repos) > 0) {
    options(repos = repos)   # <-- hidden session magic
  }
}
```

## Proposed Design

### Fix 1: Replace `renv::project()` fallback

Replace the fallback with a explicit error that guides the user. The
resolution order becomes:

1. Explicit `project` argument.
2. Current working directory if it contains `DESCRIPTION`.
3. Parent directory search for `DESCRIPTION`.
4. Clear error: `"No intent project found. Run intent::init() or pass
   project = ."`

This removes the fourth fallback (`renv::project()`) entirely. The three
remaining resolution steps are deterministic and do not depend on hidden
session state. This is a slight behavior change, but the error message
already guides users to the solution.

If backward compatibility is a concern, the fallback can be gated behind
an option (`options(intent.use_renv_fallback = TRUE)`) that defaults to
`FALSE`.

### Fix 2: Return repos instead of setting options

Change `load_intent_repos()` to a pure function that returns repository
configuration instead of mutating global state:

```r
load_intent_repos <- function(project, more_repos = NULL) {
  path_to_desc <- file.path(project, "DESCRIPTION")
  repos <- character()
  if (file.exists(path_to_desc)) {
    repos <- get_repos(path_to_desc)
  }
  if (length(more_repos) > 0) {
    repos <- c(more_repos, repos)
  }
  repos
}
```

Then update callers to use the returned value:

- `intent_install()`: pass `repos` to `backend_install()` or set it
  temporarily in the `pak` call.
- `intent_snapshot()`: pass `repos` to `backend_snapshot()`.
- `intent_restore()`: pass `repos` to `backend_restore()`.

The backend functions already accept `repos` in their `renv` equivalents
(`renv::snapshot(..., repos = ...)`, etc.), so this becomes a threading
change rather than a new capability.

### Fix 3: Remove unused imports

Remove `renv::deactivate` and `renv::load` from `R/intent-package.R`.

## Implementation Steps

1. Remove the `renv::project()` fallback from `resolve_project()`.
2. Update `load_intent_repos()` to return repos instead of setting
   `options(repos = ...)`.
3. Update `intent_install()`, `intent_snapshot()`, `intent_restore()` to
   accept and thread `repos` through to their backend calls.
4. Update `backend_install()` to accept a `repos` parameter.
5. Remove `renv::deactivate` and `renv::load` importFrom statements.
6. Regenerate NAMESPACE and documentation.
7. Update tests that depend on `options(repos)` side effects.

## Test Plan

- Unit test: `resolve_project(NULL)` with no DESCRIPTION in path hierarchy
  returns a clear error (does not call `renv::project()`).
- Unit test: `load_intent_repos()` returns a character vector without
  modifying `options(repos)`.
- Integration test: `intent::init()` and `intent::add()` still resolve and
  install packages correctly.
- Run `devtools::test()` and `pre-commit run --all-files`.

## Acceptance Criteria

- No `renv::project()` calls exist outside `R/backend.R`.
- `load_intent_repos()` does not call `options()`.
- `intent_install()`, `intent_snapshot()`, `intent_restore()` explicitly
  pass repository configuration to the backend.
- No unused `renv` imports in NAMESPACE.
- All existing tests pass.
- The project resolution algorithm is documented in the error message when
  resolution fails.

## Result / Follow-Up Notes

Fill this in after implementation.

- Result: Removed `renv::project()` fallback from `resolve_project()`. Changed
  `load_intent_repos()` to a pure function that returns repos without mutating
  `options()`. Updated `intent_install()`, `intent_snapshot()`, and
  `intent_restore()` to thread repos through to their backend calls. Updated
  `backend_install()` to temporarily set `options(repos = repos)` for pak.
  Updated `backend_snapshot()` and `backend_restore()` to accept and forward
  repos. Removed unused `renv::deactivate` and `renv::load` imports.
- Follow-up work: The `renv::project()` fallback removal is a slight behavior
  change — users who relied on `renv` active project detection must now pass
  `project =` explicitly or run commands from within the project directory.
