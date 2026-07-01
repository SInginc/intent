# Explicit Project Resolution

Status: `planned`

## Problem

`intent` currently relies on `renv::project()` in multiple internal helpers and
public command paths. This makes `renv` session state act as the package's
project model. As a result, commands can fail or behave differently depending on
whether an R session has already loaded an `renv` project.

This is especially visible in `sync(project = ...)`: the function accepts an
explicit project path, but still requires active `renv` state through
`check_renv_loaded()`.

## Goal

Introduce explicit project resolution as an Intent Core concern. Public commands
should resolve the target project once near the top of the workflow, then pass
that project path through internal helpers.

After this refactor, core logic should depend on an explicit project path rather
than implicit `renv` session state.

## Non-Goals

- Do not build the CLI in this refactor.
- Do not fully extract all backend adapters yet.
- Do not redesign dependency override syntax.
- Do not change the user-facing meaning of `add()`, `remove()`, `init()`, or
  `sync()` beyond making project resolution explicit.

## Current State

The current implementation mixes project discovery, command workflow, manifest
editing, and backend execution.

Known pressure points:

- Public functions call `check_renv_loaded()` before doing work.
- Internal helpers build paths from `renv::project()`.
- Repository loading reads from the active `renv` project instead of a supplied
  project path.
- `sync(project = ...)` does not fully honor its explicit `project` argument.

## Proposed Design

Add an internal project resolution helper, for example:

```r
resolve_project <- function(project = NULL) {
  # returns a normalized project path or errors clearly
}
```

Resolution should happen once near the top of public commands. Internal helpers
should receive `project` explicitly and avoid calling `renv::project()` unless
they are in a backend-specific fallback path.

The intended dependency direction is:

```text
public command -> resolve project -> command workflow -> internal helpers/backend
```

`sync(project = ...)` should not reject an explicit project just because
`renv::project()` is unset. Backend operations may still activate or use `renv`
internally, but the command should not treat active `renv` session state as the
source of truth.

## Implementation Steps

1. Add an internal `resolve_project(project = NULL)` helper.
2. Update public commands to resolve `project` at the top of the workflow.
3. Update internal helpers to accept `project` explicitly.
4. Replace direct path construction from `renv::project()` in core logic with
   paths derived from the resolved project.
5. Keep backend-specific `renv` calls isolated and pass `project` into them.
6. Update documentation and generated Rd files as needed after code changes.

## Test Plan

- Add unit tests for project resolution that do not install packages and do not
  require network access.
- Test explicit project paths.
- Test current-directory project discovery.
- Test clear failure when no project can be resolved.
- Test that `sync(project = ...)` no longer fails only because
  `renv::project()` is unset.
- Keep package-installing behavior in integration tests rather than core unit
  tests.

## Acceptance Criteria

- Public commands resolve the project path once near the top of the workflow.
- Core helpers receive explicit project paths.
- `sync(project = ...)` honors the supplied project path without requiring
  active `renv` session state at command entry.
- Core project resolution tests run without package installation or network
  access.
- Remaining direct `renv::project()` calls are either removed or clearly limited
  to backend-specific behavior.

## Result / Follow-Up Notes

Fill this in after implementation.

- Result:
- Follow-up work:

