# Command Layer

Status: `implemented`

## Problem

The public R API currently owns command workflow orchestration. Functions such
as `add()`, `remove()`, `sync()`, and `init()` parse user-facing arguments,
resolve projects, update manifests, call backend operations, print messages, and
return values directly.

This makes the future CLI likely to duplicate workflow logic or call public R
functions that were not designed as a shared command layer.

## Goal

Introduce an internal command layer that owns workflow orchestration. Public R
API functions should become thin wrappers that validate user-facing arguments
where appropriate and delegate to command functions.

The future CLI should be able to call the same command functions.

## Non-Goals

- Do not build the CLI in this refactor.
- Do not change public R function names or return values.
- Do not add dry-run or status behavior yet.
- Do not redesign backend adapters.
- Do not redesign dependency override syntax.

## Current State

The package now has explicit project resolution and an internal backend
boundary, but public functions still contain the workflow itself.

Current examples:

- `add()` resolves the project, installs packages, edits `DESCRIPTION`, and
  snapshots.
- `remove()` resolves the project, edits `DESCRIPTION`, removes packages,
  snapshots, and restores.
- `sync()` resolves the project, compares manifest and lockfile state, installs
  missing packages, snapshots, and restores.
- `init()` creates or reads `DESCRIPTION`, configures repositories, initializes
  backend state, and writes `.Renviron`.

## Proposed Design

Add internal command functions:

```r
cmd_init(path = ".", repos = NULL)
cmd_add(pkgs, dev = FALSE, project = NULL)
cmd_remove(pkgs, project = NULL)
cmd_sync(project = NULL)
```

Public R API functions should call these command functions and keep the same
external behavior.

Command functions may still use existing core helpers and backend functions.
This refactor only creates a shared orchestration layer; it does not fully
separate all core planning logic yet.

## Implementation Steps

1. Add an internal command module.
2. Move workflow bodies from public API functions into `cmd_*` functions.
3. Replace public API bodies with thin calls to command functions.
4. Keep user-facing roxygen on public API functions only.
5. Run documentation, tests, and pre-commit.

## Test Plan

- Run `devtools::document()`.
- Run `devtools::test()`.
- Run `pre-commit run --all-files`.
- Existing public API tests should continue to pass unchanged.
- No new CLI tests are required because no CLI is added in this refactor.

## Acceptance Criteria

- Public API functions delegate to `cmd_*` functions.
- Command functions own workflow orchestration.
- Public behavior and tests remain unchanged.
- No CLI is introduced.
- Result notes are filled in after implementation.

## Result / Follow-Up Notes

- Result: Added `R/commands.R` with internal `cmd_init()`, `cmd_add()`,
  `cmd_remove()`, and `cmd_sync()` workflow functions. Public R API functions
  now delegate to the command layer while preserving existing behavior.
- Follow-up work: Add `status` and dry-run planning so command functions can
  expose planned operations before mutating project state.
