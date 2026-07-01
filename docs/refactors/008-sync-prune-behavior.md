# Sync Prune Behavior

Status: `implemented`

## Problem

`cmd_sync()` installs missing packages but never removes packages that are in
`renv.lock` but absent from `DESCRIPTION`. The README specification (line 303)
states that sync should "Remove extra dependencies," but the current
implementation only adds — it never prunes.

`cmd_status()` already reports `extra_in_lockfile` (lockfile packages not directly
listed in the manifest), so the drift detection exists. Users can see the problem
but cannot fix it through `sync()`.

This leaves the lockfile with stale entries after a user edits `DESCRIPTION`
directly or after a package was declared in one branch and removed in another.

## Goal

Give `cmd_sync()` the ability to bring the lockfile into full alignment with
`DESCRIPTION` — both by installing missing packages and by removing packages that
are no longer declared.

After this change, `sync()` should be the single command a user needs after
`git pull`, after manually editing `DESCRIPTION`, or when repairing a project
whose state has drifted.

## Non-Goals

- Do not change `cmd_remove()` — removing a single package with `intent::remove()`
  already handles lockfile cleanup through snapshot + restore.
- Do not implement a full dependency solver that inspects transitive reachability.
- Do not change the dry-run or status data structures beyond what is needed to
  expose prune actions.
- Do not add a separate `prune` command (a flag on the existing `sync` command is
  sufficient for this scope).

## Current State

`cmd_sync()` in `R/commands.R:162-241`:

1. Reads manifest packages from `DESCRIPTION`.
2. Reads lockfile packages from `renv.lock` (if it exists).
3. Computes `missing_pkgs` (manifest minus lockfile).
4. Installs missing packages and snapshots.
5. Runs `intent_restore()` to ensure the library matches the lockfile.

It never computes `extra_in_lockfile` (lockfile minus manifest) and never removes
stale packages from the lockfile.

`cmd_remove()` takes a different path: it removes packages from DESCRIPTION,
uninstalls them, snapshots, then restores (which removes orphaned transitive
dependencies). Sync has no equivalent cleanup step.

## Proposed Design

Add a `prune` argument to `cmd_sync()` and the public `sync()`, defaulting to
`TRUE` (sync should fully align by default, matching the README specification).

Behavior when `prune = TRUE`:

1. Compute `extra_in_lockfile` (packages in lockfile but not in manifest, excluding
   `intent` itself and backend packages like `pak`/`renv`).
2. If packages need to be removed, remove them from the lockfile by calling
   `renv::remove()` for each stale package, then snapshot.
3. Include prune actions in the dry-run plan when `dry_run = TRUE`.

Dry-run plan would gain a new action type:

```text
would_prune: pkg1, pkg2
```

The default `prune = TRUE` is the safer long-term choice because:

- It matches the README specification.
- It matches user expectations from `intent::remove()`.
- A new user who runs `sync` after editing `DESCRIPTION` expects full alignment.
- Users who want to inspect before pruning can use `--dry-run` first.

If tooling concerns require an escape hatch, `prune = FALSE` gives users explicit
control.

## Implementation Steps

1. Add `prune` argument to public `sync()` (`R/sync.R`) and `cmd_sync()`
   (`R/commands.R`), defaulting to `TRUE`.
2. In `cmd_sync()`, after computing `missing_pkgs`, compute `extra_pkgs` using the
   same logic as `cmd_status()`.
3. When `prune = TRUE` and `extra_pkgs` is non-empty, call `intent_del_project_dep()`
   for packages that also appear in the manifest's `Suggests`/`Imports` (though
   they should not if they are truly extra), then call `renv::remove()` and
   `intent_snapshot()` + `intent_restore()`.
4. Update dry-run path to include prune actions.
5. Add `prune` flag to CLI (`R/cli.R`): `--prune` / `--no-prune`.
6. Regenerate documentation with `devtools::document()`.

## Test Plan

- Unit test: `cmd_sync(prune = TRUE)` on a temporary project where the lockfile
  contains an extra package not in DESCRIPTION. Verify the extra package is removed
  from the lockfile.
- Unit test: `cmd_sync(prune = FALSE)` leaves the extra package in place.
- Unit test: `cmd_sync(dry_run = TRUE, prune = TRUE)` returns a plan with
  `would_prune` action and does not mutate files.
- Integration test: full `sync()` on a project with stale lockfile entries.
- Run `devtools::test()` and `pre-commit run --all-files`.

## Acceptance Criteria

- `sync()` removes packages from the lockfile that are not declared in
  `DESCRIPTION`.
- `sync(prune = FALSE)` skips removal.
- `sync(dry_run = TRUE)` reports planned prune actions without mutating state.
- CLI supports `--prune` / `--no-prune` flags on `intent sync`.
- Existing sync behavior for missing-package installation is unchanged.
- Tests and pre-commit pass.

## Result / Follow-Up Notes

Fill this in after implementation.

- Result: Added `prune` argument (default `TRUE`) to `sync()` and `cmd_sync()`.
  Sync now removes lockfile packages not declared in DESCRIPTION. Added
  `--prune`/`--no-prune` CLI flags. Added unit tests for prune behavior,
  prune=FALSE preservation, and dry-run reporting.
- Follow-up work: Consider pruning transitive dependencies that are orphaned
  after removing their last parent from the lockfile.
