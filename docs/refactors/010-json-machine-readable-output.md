# JSON Machine-Readable Output

Status: `implemented`

## Problem

`intent_status` and `intent_plan` are structured S3 objects, but they only support
human-readable `print()` methods. The CLI design document (`docs/03-cli-design.md`,
line 21 and line 80) specifies that `status` should support `--json` for
machine-readable output. Two prior refactors explicitly deferred this:

- Refactor 004 (Status and Dry Run), follow-up: "Add CLI output modes, including
  machine-readable status/plan output."
- Refactor 005 (CLI Entry Point), follow-up: "Add machine-readable output for
  status and dry-run plans."

Without JSON output, `intent` cannot be used in CI pipelines, shell scripts, or
editor integrations that need to parse project state programmatically.

## Goal

Add JSON serialization support to `intent_status` and `intent_plan` objects, and
add a `--json` flag to the CLI's `status` and `sync --dry-run` commands.

The JSON output should be structurally stable, parseable by standard tools, and
include the same information as the human-readable output.

## Non-Goals

- Do not define a formal JSON Schema file (can be added later).
- Do not add JSON output to `add`, `remove`, or `init` commands yet.
- Do not change the structure of `intent_status` or `intent_plan` objects.
- Do not change the R API behavior — `print()` remains the default.
- Do not add an `--output file.json` flag (stdout is sufficient for this scope).

## Current State

`R/status.R` defines two structured S3 objects:

- `new_intent_status()` (lines 57–78): returns a list with `project`,
  `manifest_packages`, `locked_packages`, `missing_from_lockfile`,
  `extra_in_lockfile`, `library_path`, `missing_from_library`.
- `new_intent_plan()` (lines 80–95): returns a list with `project`, `command`,
  `actions`, `packages`.

Both have `print()` methods that format them for human consumption. Neither has a
JSON serialization method.

`R/cli.R` parses `--project`, `--dev`, `--dry-run` flags but has no `--json`
flag handling. The CLI dispatches to `cmd_status()` (which returns an
`intent_status` object) and `cmd_sync()` (which returns an `intent_plan` when
`dry_run = TRUE`).

The package's DESCRIPTION lists `jsonlite` in `Suggests` (for testing), so it is
available but not a hard dependency.

## Proposed Design

### R API

Add `as.character.intent_status()` and `as.character.intent_plan()` methods that
return a JSON string using `jsonlite::toJSON()` with `auto_unbox = TRUE` and
`pretty = FALSE`:

```r
as.character.intent_status <- function(x, ...) {
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = FALSE)
}

as.character.intent_plan <- function(x, ...) {
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = FALSE)
}
```

This is the standard R pattern for string coercion. Users calling
`as.character(status())` get JSON.

Alternatively, add dedicated `to_json()` methods if the coercion path feels
surprising. However, `as.character()` is idiomatic R and used by other packages
for text serialization.

### CLI

Add `--json` flag parsing to `cli_parse_common()`. When `--json` is passed:

- For `intent status --json`: call `cmd_status()`, convert to JSON via
  `as.character()`, and `cat()` the result to stdout.
- For `intent sync --dry-run --json`: call `cmd_sync(dry_run = TRUE)`, convert to
  JSON, and `cat()` to stdout.

The `--json` flag is ignored for mutating commands (`add`, `remove`, `sync`
without `--dry-run`) — they print their normal output. It could be made an error
in a future pass, but silently ignoring is the simpler initial behavior.

### Output Format

Example `intent status --json`:

```json
{
  "project": "/path/to/project",
  "manifest_packages": ["dplyr", "ggplot2", "testthat"],
  "locked_packages": ["dplyr", "ggplot2", "R6", "cli", "rlang", "testthat"],
  "missing_from_lockfile": [],
  "extra_in_lockfile": ["R6", "cli", "rlang"],
  "library_path": "/path/to/project/renv/library",
  "missing_from_library": []
}
```

Example `intent sync --dry-run --json`:

```json
{
  "project": "/path/to/project",
  "command": "sync",
  "actions": ["would_install: dplyr", "would_snapshot", "would_restore"],
  "packages": ["dplyr"]
}
```

## Implementation Steps

1. Add `jsonlite` from `Suggests` to `Imports` in `DESCRIPTION` (or add a
   conditional check with a clear error if `jsonlite` is not installed).
2. Add `as.character.intent_status()` and `as.character.intent_plan()` methods in
   `R/status.R`.
3. Add `--json` flag parsing to `cli_parse_common()` in `R/cli.R`.
4. Update `cli_status()` and `cli_sync()` to handle `--json` output.
5. Regenerate documentation with `devtools::document()`.

## Test Plan

- Unit test: `as.character(status_obj)` returns valid JSON with expected keys.
- Unit test: `as.character(plan_obj)` returns valid JSON with expected keys.
- CLI test: `cli_main(c("status", "--json"))` prints JSON to stdout.
- CLI test: `cli_main(c("sync", "--dry-run", "--json"))` prints JSON to stdout.
- CLI test: `cli_main(c("add", "dplyr", "--json"))` does not error (ignores flag
  or warns).
- Round-trip test: `jsonlite::fromJSON(as.character(status_obj))` returns a list
  with the same structure.
- Run `devtools::test()` and `pre-commit run --all-files`.

## Acceptance Criteria

- `as.character(intent::status())` returns a valid JSON string.
- `as.character(intent::sync(dry_run = TRUE))` returns a valid JSON string.
- `intent status --json` prints machine-readable JSON.
- `intent sync --dry-run --json` prints machine-readable JSON.
- The human-readable `print()` methods are unchanged.
- Tests and pre-commit pass.

## Result / Follow-Up Notes

Fill this in after implementation.

- Result: Moved `jsonlite` from Suggests to Imports. Added
  `as.character.intent_status()` and `as.character.intent_plan()` S3 methods
  that serialize to JSON via `jsonlite::toJSON(unclass(x), ...)`. Added
  `--json` CLI flag for `intent status` and `intent sync --dry-run`. Added
  tests for JSON round-trip and CLI flag parsing.
- Follow-up work: Consider a formal JSON Schema file and `--output` flag for
  writing JSON to a file instead of stdout.
