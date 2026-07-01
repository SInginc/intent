# Layering Cleanup

Status: `implemented`

## Problem

The architecture audit found two layering issues:

1. **`cmd_init()` calls `cmd_sync()`** (`R/commands.R:69`) — Commands
   should be peers, not nested. The architecture doc (02-architecture.md)
   states: "No lower layer should call a higher layer." Two commands
   calling each other creates an implicit dependency cycle.

2. **`R/status.R` mixes three concerns in one file** — The file contains:
   - Public R API (`status()`).
   - Core domain logic (`intent_manifest_packages()`,
     `intent_locked_packages()`, `intent_library_packages()`).
   - S3 constructors and print methods (`new_intent_status()`,
     `new_intent_plan()`, `print.intent_status()`, `print.intent_plan()`,
     `as.character.intent_status()`, `as.character.intent_plan()`).

   The architecture says "Interface code should be thin" — this file is 140
   lines and handles at least two distinct concerns.

## Goal

Fix both layering issues so the codebase follows the documented
architecture:

- Commands call core and backend, not other commands.
- Core logic is separated from interface code.

## Non-Goals

- Do not introduce formal S4 or R6 classes.
- Do not change public behavior.
- Do not add new features.
- Do not rename backend adapter functions (defer to a separate refactor).

## Proposed Design

### Fix 1: Extract `cmd_init`'s sync logic

`cmd_init()` calls `cmd_sync()` only when the project already has an
`renv/` directory (i.e., re-initializing an existing project). Extract this
into a shared internal helper:

```r
# In R/utils.R or R/backend.R
intent_sync_project <- function(project) {
  intent_snapshot(project)
  intent_restore(project)
}
```

Then `cmd_init()` calls `intent_sync_project(project_dir)` instead of
`cmd_sync(project = project_dir)`.

This is not a new public function — it's an internal helper that both
`cmd_init()` and `cmd_sync()` can use if needed. The key is that it lives
in the core layer, not the command layer.

### Fix 2: Split `R/status.R`

Split into:

- `R/status.R` — public API only: `status()` and S3 methods
  (`print.intent_status`, `print.intent_plan`, `as.character.intent_status`,
  `as.character.intent_plan`).
- `R/status-core.R` (or move into `R/utils.R`) — core domain functions:
  `intent_manifest_packages()`, `intent_locked_packages()`,
  `intent_library_packages()`, `new_intent_status()`, `new_intent_plan()`.

The public API file becomes a thin 15-line wrapper. The core file holds
the domain logic that `cmd_status()` already calls.

## Implementation Steps

1. Create `R/status-core.R` with the five core functions.
2. Trim `R/status.R` to only the public API and S3 methods.
3. In `cmd_init()`, replace `cmd_sync(project = project_dir)` with
   `intent_sync_project(project_dir)`.
4. Add `intent_sync_project()` helper to `R/utils.R`.
5. Regenerate documentation and NAMESPACE.
6. Run tests.

## Test Plan

- Run `devtools::test()` — all existing tests should pass without changes
  (the functions are the same, just in different files).
- Verify `intent::init()` on an existing project still syncs correctly.
- Run `pre-commit run --all-files`.

## Acceptance Criteria

- `R/status.R` contains only the public API and S3 methods (≤ 30 lines).
- Core domain functions live in a separate file.
- `cmd_init()` does not call `cmd_sync()`.
- No public behavior changes.
- All tests pass.

## Result / Follow-Up Notes

Fill this in after implementation.

- Result: Created `R/status-core.R` with core domain functions
  (`new_intent_status`, `new_intent_plan`, `intent_manifest_packages`,
  `intent_locked_packages`, `intent_library_packages`). Trimmed `R/status.R`
  to ~60 lines containing only the public API and S3 methods. Added
  `intent_sync_project()` helper in `R/utils.R` and replaced `cmd_init()`'s
  call to `cmd_sync()` with a call to the helper.
- Follow-up work: None.
