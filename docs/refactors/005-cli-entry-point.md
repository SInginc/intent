# CLI Entry Point

Status: `implemented`

## Problem

`intent` now has a command layer, status reporting, and dry-run planning, but
users still need to enter R to use the package. The product direction is
terminal-first, so the package needs a CLI entry point that calls the same
command layer as the R API.

R packages do not automatically install arbitrary commands onto the user's
global `PATH`, so the first CLI refactor should provide a package-owned,
testable entry point without pretending to solve global command installation.

## Goal

Add an internal CLI dispatcher and package script that support:

```sh
intent init
intent add <package>...
intent remove <package>...
intent sync
intent status
```

The CLI should call `cmd_*` functions directly. It should be testable from the
repository without requiring a global shell command.

## Non-Goals

- Do not add a global installer or PATH management.
- Do not add JSON output yet.
- Do not add shell completion.
- Do not add a third-party argument parser.
- Do not change R API behavior.

## Current State

Public R functions are thin wrappers around `cmd_*` functions, which gives the
CLI a stable internal API to call. There is no terminal entry point yet.

## Proposed Design

Add internal functions:

```r
intent_cli <- function(args = commandArgs(trailingOnly = TRUE))
cli_main <- function(args)
```

Add a package script under `exec/intent` that calls the CLI dispatcher through
Rscript. The script is primarily a package-owned entry point and integration
test target; global command installation can be solved later.

Supported flags:

- `--project <path>` for `add`, `remove`, `sync`, and `status`.
- `--dev` for `add`.
- `--dry-run` for `add`, `remove`, and `sync`.
- `--repo NAME=URL` repeatable for `init`.
- `--help` for top-level help.

## Implementation Steps

1. Add a CLI module with base-R argument parsing.
2. Add `exec/intent` as the package script entry point.
3. Add tests for CLI argument dispatch using stubs around command functions.
4. Add a lightweight script smoke test if practical.
5. Update README or docs only if needed to avoid overclaiming global install
   behavior.
6. Run documentation, tests, and pre-commit.

## Test Plan

- Unit test dispatch for `status`, `sync --dry-run`, `add --dev`, `remove`, and
  `init --repo`.
- Test clear errors for unknown commands and missing package arguments.
- Run `devtools::document()`.
- Run `devtools::test()`.
- Run `pre-commit run --all-files`.

## Acceptance Criteria

- CLI dispatcher calls the same `cmd_*` functions as the R API.
- `exec/intent` exists and can be used as the package-owned script entry point.
- Supported commands and flags are tested.
- No global PATH behavior is claimed or required.
- Existing R API tests continue to pass.

## Result / Follow-Up Notes

- Result: Added a base-R CLI dispatcher, package-owned `exec/intent` script,
  and unit tests for command dispatch and argument parsing. The CLI calls the
  same `cmd_*` command layer used by the R API.
- Follow-up work: Add machine-readable output for status and dry-run plans, then
  decide how to install or expose a global `intent` command outside the package
  library.
