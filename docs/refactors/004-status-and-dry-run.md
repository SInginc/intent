# Status and Dry Run

Status: `planned`

## Problem

`intent` can mutate project state, but users cannot inspect drift before running
mutating commands. This makes the tool harder to trust and keeps the command
layer from expressing operations as inspectable plans.

The package needs a clear read-only entry point before the CLI is introduced.

## Goal

Add `intent::status()` and `dry_run` support for mutating commands.

Users should be able to:

- See manifest, lockfile, and local library drift without changing files.
- Ask `add()`, `remove()`, and `sync()` what they would do before they do it.
- Receive a structured result that can later be used by the CLI.

## Non-Goals

- Do not build the CLI.
- Do not implement JSON output yet.
- Do not implement a full dependency solver.
- Do not inspect transitive dependency reachability beyond current lockfile
  presence.
- Do not change default behavior of existing mutating commands.

## Current State

`cmd_add()`, `cmd_remove()`, and `cmd_sync()` execute immediately. They print
messages and return simple invisible values, but they do not expose a structured
plan.

There is no exported `status()` command.

## Proposed Design

Add public API:

```r
status <- function(project = NULL)
add <- function(pkgs, dev = FALSE, project = NULL, dry_run = FALSE)
remove <- function(pkgs, project = NULL, dry_run = FALSE)
sync <- function(project = NULL, dry_run = FALSE)
```

Add internal command:

```r
cmd_status(project = NULL)
```

`status()` returns an object with class `intent_status` and fields:

- `project`: normalized project path.
- `manifest_packages`: direct `Imports` and `Suggests` packages.
- `locked_packages`: packages recorded in `renv.lock`.
- `missing_from_lockfile`: manifest packages not present in the lockfile.
- `extra_in_lockfile`: lockfile packages not directly listed in manifest.
- `library_path`: backend library path.
- `missing_from_library`: locked packages not installed in the project library.

`dry_run = TRUE` should avoid file, lockfile, and library mutation. It should
return an `intent_plan` object with fields:

- `project`
- `command`
- `actions`
- `packages`

Initial actions may be coarse-grained, such as `would_install`,
`would_update_manifest`, `would_snapshot`, `would_restore`, and
`would_remove_from_manifest`.

## Implementation Steps

1. Add status helpers for manifest packages, lockfile packages, and library
   package directories.
2. Add `cmd_status()` and public `status()`.
3. Add `intent_status` and `intent_plan` print methods.
4. Add `dry_run` arguments to public and command functions for `add`, `remove`,
   and `sync`.
5. Ensure dry-run paths do not call backend mutation functions.
6. Update roxygen docs and tests.

## Test Plan

- Unit test `status()` on temporary DESCRIPTION/lockfile/library fixtures without
  installing packages.
- Unit test `dry_run = TRUE` for `cmd_add()`, `cmd_remove()`, and `cmd_sync()`
  using stubs to prove backend mutation functions are not called.
- Keep existing integration tests for default mutating behavior.
- Run `devtools::document()`.
- Run `devtools::test()`.
- Run `pre-commit run --all-files`.

## Acceptance Criteria

- `status()` is exported and documented.
- `status()` returns structured drift data and prints a readable summary.
- `add(..., dry_run = TRUE)`, `remove(..., dry_run = TRUE)`, and
  `sync(..., dry_run = TRUE)` do not mutate manifest, lockfile, or library
  state.
- Existing default mutating behavior remains unchanged.
- Tests and pre-commit pass.

## Result / Follow-Up Notes

Fill this in after implementation.

- Result:
- Follow-up work:
