# CLI Design

The CLI should be the clearest entry point for end users. It should feel like a
small dependency manager, not like a collection of R helper functions.

## Command Shape

```sh
intent init
intent add <package>...
intent remove <package>...
intent sync
intent status
```

Optional flags:

```sh
intent add testthat --dev
intent sync --dry-run
intent status --json
intent init --repo CRAN=https://packagemanager.posit.co/cran/latest
```

## Command Semantics

### `intent init`

Initializes the current directory or a supplied path as an `intent` project.

Expected behavior:

- Ensure `DESCRIPTION` exists.
- Ensure required intent metadata exists.
- Initialize backend state if needed.
- Avoid overwriting existing user configuration without warning.

### `intent add`

Declares one or more direct dependencies and makes project state match.

Expected behavior:

- Add packages to `Imports` by default.
- Add packages to `Suggests` when `--dev` is used.
- Install and lock through the configured backend.
- Print a concise summary of changed files and installed packages.

### `intent remove`

Removes one or more direct dependencies and reconciles project state.

Expected behavior:

- Remove packages from `DESCRIPTION`.
- Remove unused installed packages when the backend supports it.
- Update the lockfile.

### `intent sync`

Makes the local dependency state match declared intent.

Expected behavior:

- Read `DESCRIPTION`.
- Compare against lockfile and local library state.
- Install, update, remove, lock, and restore as needed.
- Support `--dry-run`.

### `intent status`

Shows drift without mutating state.

Expected behavior:

- Report packages declared but not locked.
- Report packages locked but not declared where relevant.
- Report packages locked but not installed.
- Report repository configuration used by the project.
- Return machine-readable output with `--json`.

## Output Style

Output should be quiet, direct, and action-oriented.

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

Errors should state:

- What failed.
- Why it failed if known.
- What the user can do next.

## Implementation Options

Possible CLI implementation paths:

- R `exec/intent` script installed with the package.
- `argparse` or `optparse`-based command parser.
- A small shell shim that calls R, if packaging constraints require it.

The CLI must call the same command layer used by the R API.

