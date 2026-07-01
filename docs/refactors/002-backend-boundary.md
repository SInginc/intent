# Backend Boundary

Status: `implemented`

## Problem

`intent` now resolves project paths explicitly, but backend operations are still
mixed into general utility helpers and public command workflows. Calls to
`renv` and `pak` are spread across command logic and helper functions, which
makes the package harder to test, harder to reason about, and harder to expose
through a future CLI command layer.

The current shape still treats `renv` as a structural dependency of the core
workflow instead of an execution backend.

## Goal

Create a narrow internal backend boundary for package-library and lockfile
operations. The first backend will still use `renv` and `pak`, but public
commands and core helpers should call intent-owned backend functions rather than
calling backend packages directly.

After this refactor, command workflows should read as:

```text
resolve project -> update/read intent -> call backend operation -> report result
```

## Non-Goals

- Do not introduce a user-facing backend selection setting.
- Do not build the CLI.
- Do not change the public behavior of `init()`, `add()`, `remove()`, or
  `sync()`.
- Do not implement a non-`renv` backend yet.
- Do not redesign dependency override syntax.

## Current State

Backend behavior currently lives in `R/utils.R` and public commands:

- `intent_install()` reads overrides, sets repositories, resolves package specs,
  finds the `renv` library, and calls `pak::pkg_install()`.
- `intent_snapshot()` calls `renv::snapshot()` directly.
- `intent_restore()` calls `renv::restore()` directly.
- `remove()` calls `renv::remove()` directly.
- `init()` calls `renv::init()`, `utils::install.packages()`,
  `renv::snapshot()`, and lockfile read/write directly.

This is improved from implicit project state, but backend concerns remain
scattered.

## Proposed Design

Add an internal backend module, initially backed by `renv` and `pak`.

Minimum internal backend surface:

```r
backend_init(project, repos)
backend_install(project, pkgs)
backend_remove(project, pkgs)
backend_snapshot(project)
backend_restore(project)
backend_read_lockfile(project)
backend_write_lockfile(lockfile, project)
backend_library(project)
```

These functions are not public API. They establish the boundary and can be
renamed or expanded later when the command layer becomes explicit.

Command and helper code should call backend functions instead of direct
`renv`/`pak` operations where practical in this refactor.

## Implementation Steps

1. Add a new internal backend file for the first backend boundary.
2. Move direct `renv`/`pak` execution from generic helpers into backend
   functions.
3. Update `add()`, `remove()`, `sync()`, and `init()` to call backend functions
   for execution operations.
4. Keep manifest parsing and dependency override parsing outside the backend.
5. Update tests that stub direct backend calls so they stub intent-owned backend
   functions instead.
6. Regenerate documentation and run the full test/pre-commit suite.

## Test Plan

- Run `devtools::document()`.
- Run `devtools::test()`.
- Run `pre-commit run --all-files`.
- Keep existing integration tests for `init`, `add/remove`, and `sync`.
- Keep unit tests for overrides and project resolution independent of package
  installation where possible.

## Acceptance Criteria

- Public command workflows no longer call `pak::pkg_install()`,
  `renv::snapshot()`, `renv::restore()`, or `renv::remove()` directly.
- Generic helpers no longer contain raw backend execution beyond calling
  backend boundary functions.
- Remaining direct `renv`/`pak` execution is concentrated in the backend module
  and `init()` bootstrapping only where the backend module owns the operation.
- Existing behavior and tests continue to pass.
- The result section records what changed and any follow-up work.

## Result / Follow-Up Notes

- Result: Added an internal backend boundary in `R/backend.R`, moved direct
  package install, remove, snapshot, restore, lockfile, and library operations
  behind `backend_*()` functions, and updated command workflows to call the
  intent-owned boundary.
- Follow-up work: Introduce a command layer so public R APIs and the future CLI
  can share workflow orchestration without duplicating command behavior.
