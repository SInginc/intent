# CLI Design

The CLI should be the clearest entry point for end users. It should feel like a
small dependency manager, not like a collection of R helper functions.

## Command Shape

```sh
intent init [path] [--repo NAME=URL]...
intent add <package>... [--project path] [--dev] [--dry-run]
intent remove <package>... [--project path] [--dry-run]
intent sync [--project path] [--dry-run] [--json] [--no-prune]
intent status [--project path] [--json]
```

## Common Flags

These flags are accepted by multiple commands:

```
--project <path>   Override automatic project discovery with an
                   explicit path. Accepted by add, remove, sync,
                   and status.

--dry-run          Report planned actions without changing any
                   files or installed packages. Accepted by add,
                   remove, and sync. Combine with --json for
                   machine-readable output.

--json             Output machine-readable JSON instead of
                   human-readable text. Accepted by status (always)
                   and sync (only with --dry-run).
```

## Command Semantics

### `intent init`

Initializes the current directory or a supplied path as an `intent` project.

**Flags:**

```
--repo NAME=URL   Add a repository to Config/intent/repos/. Repeatable.
                  Example: --repo CRAN=https://packagemanager.posit.co/cran/latest
```

**Expected behavior:**

- Ensure `DESCRIPTION` exists.
- Ensure required intent metadata exists.
- Initialize backend state if needed.
- Avoid overwriting existing user configuration without warning.
- Default to Posit Package Manager (PPM) when no repos are provided.

### `intent add`

Declares one or more direct dependencies and makes project state match.

**Flags:**

```
--project <path>   Override project discovery.
--dev              Add to Suggests instead of Imports.
--dry-run          Report planned actions without installing or writing.
```

**Expected behavior:**

- Add packages to `Imports` by default.
- Add packages to `Suggests` when `--dev` is used.
- Install and lock through the configured backend.
- Print a concise summary of changed files and installed packages.

### `intent remove`

Removes one or more direct dependencies and reconciles project state.

**Flags:**

```
--project <path>   Override project discovery.
--dry-run          Report planned actions without removing or writing.
```

**Expected behavior:**

- Remove packages from `DESCRIPTION`.
- Remove unused installed packages when the backend supports it.
- Update the lockfile.

### `intent sync`

Makes the local dependency state match declared intent.

**Flags:**

```
--project <path>   Override project discovery.
--dry-run          Report planned actions without changing state.
--json             Output machine-readable plan (requires --dry-run).
--prune            Remove lockfile packages not in DESCRIPTION (default).
--no-prune         Skip removal of extra lockfile packages.
```

**Expected behavior:**

- Read `DESCRIPTION`.
- Compare against lockfile and local library state.
- Install, update, remove, lock, and restore as needed.
- By default, prune orphaned lockfile packages.
- Support `--dry-run` and `--json`.

### `intent status`

Shows drift without mutating state.

**Flags:**

```
--project <path>   Override project discovery.
--json             Output machine-readable JSON.
```

**Expected behavior:**

- Report packages declared but not locked.
- Report packages locked but not declared where relevant.
- Report packages locked but not installed.
- Report repository configuration used by the project.
- Return machine-readable output with `--json`.

## JSON Output Format

### `intent status --json`

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

### `intent sync --dry-run --json`

```json
{
  "project": "/path/to/project",
  "command": "sync",
  "actions": [
    "would_install: dplyr",
    "would_snapshot",
    "would_restore"
  ],
  "packages": ["dplyr"]
}
```

Prune actions appear as a `would_prune` entry in the actions array when
`--prune` is enabled (the default).

## Output Style

Output is quiet, direct, and action-oriented. Non-JSON output uses `message()`
calls to report progress. Errors state what failed, why it failed, and what the
user can do next.

Example:

```text
Project: /path/to/project
Manifest: DESCRIPTION
Backend: renv

Added:
  Imports:
    dplyr

Updated:
  renv.lock
  renv/library
```

## Implementation

The CLI dispatches through `R/cli.R`:

- `intent_cli()` is the entry point called by `exec/intent`.
- `cli_main()` routes commands to per-command handlers.
- `cli_parse_common()` handles shared flag parsing (`--project`, `--dry-run`,
  `--json`, `--dev`, `--prune`, `--no-prune`).
- Each handler parses its own flags then delegates to the `cmd_*` command layer.

The CLI calls the same command layer used by the R API.
