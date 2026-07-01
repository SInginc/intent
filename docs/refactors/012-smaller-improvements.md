# Smaller Improvements

Status: `implemented`

## Overview

This document covers three small, loosely related improvements. Each is scoped
independently and can be implemented separately.

---

## E1: Empty `air.toml` Configuration

### Problem

`air.toml` exists as a 0-byte file in the repository root. The pre-commit
configuration (`.pre-commit-config.yaml`) runs `air format` on `R/` and `tests/`
files. An empty configuration file means air falls back to its default formatting
rules, which may or may not match the project's existing code style.

A 0-byte configuration file is confusing: it signals intent ("we use air") but
provides no guidance. Contributors may wonder whether the file should be populated
or removed.

### Goal

Either populate `air.toml` with a minimal configuration that matches the project's
existing code style, or remove it and let air use its defaults implicitly.

### Non-Goals

- Do not change the project's code style.
- Do not add or remove pre-commit hooks.

### Proposed Design

**Recommendation: populate `air.toml` with explicit settings.**

An explicit `air.toml` ensures consistent formatting on every platform. The
project already follows tidyverse conventions, so the configuration mirrors
air's defaults while being explicit:

```toml
[format]
line-width = 80
indent-width = 2
indent-style = "space"
line-ending = "auto"
assignment-style = "arrow"
```

### Implementation Steps

1. Write a minimal `air.toml` with explicit tidyverse-style settings.
2. Keep `^air\\.toml$` in `.Rbuildignore` (not part of the R package build).
3. Verify `air format` still passes on the codebase.

### Acceptance Criteria

- `air.toml` is populated with explicit formatting settings.
- `air format` on the codebase produces no changes.
- Pre-commit hook continues to pass.

---

## E2: `basename(pkg)` in `cmd_add()` Does Not Handle Version Strings

### Problem

`cmd_add()` at `R/commands.R:114` uses `basename(pkg)` to extract the bare package
name from a package reference:

```r
pkg_name <- basename(pkg)
```

This works for `user/repo` GitHub references (returns `repo`), but fails for
version-annotated package names:

- `basename("dplyr@1.0.0")` returns `"dplyr@1.0.0"` (the `@` is not a path
  separator).
- `basename("user/repo@1.0")` returns `"repo@1.0"` (only the `/` is stripped).

The result is that `intent_set_project_dep()` writes a malformed package name
(`"dplyr@1.0.0"` instead of `"dplyr"`) into `DESCRIPTION`.

While the current public API does not document version strings in `add()`, the
underlying `intent_install()` function already resolves overrides that use version
syntax. This mismatch could silently corrupt a DESCRIPTION file.

### Goal

Fix the package name extraction to handle `name@version` and `user/repo@version`
formats correctly, even if those formats are not yet part of the public `add()`
contract.

### Non-Goals

- Do not add version string support to the public `add()` API (that is a separate
  feature decision).
- Do not change how `intent_install()` resolves package references.

### Proposed Design

Replace `basename(pkg)` with a small helper that strips both path-like prefixes
and version suffixes:

```r
extract_pkg_name <- function(pkg) {
  pkg_name <- basename(pkg)          # strip user/repo path
  pkg_name <- gsub("@.*$", "", pkg_name)  # strip @version
  pkg_name
}
```

Apply this in `cmd_add()` at line 114.

### Implementation Steps

1. Add `extract_pkg_name()` helper (either in `R/utils.R` or inline in
   `cmd_add()`).
2. Replace `pkg_name <- basename(pkg)` with `pkg_name <- extract_pkg_name(pkg)`.
3. Add a unit test for the helper: `extract_pkg_name("dplyr")`,
   `extract_pkg_name("user/dplyr")`, `extract_pkg_name("dplyr@1.0.0")`,
   `extract_pkg_name("user/dplyr@1.0.0")`.

### Acceptance Criteria

- `cmd_add(c("dplyr@1.0.0"))` writes `dplyr` (not `dplyr@1.0.0`) to DESCRIPTION.
- `cmd_add(c("user/repo"))` continues to write `repo` to DESCRIPTION.
- `cmd_add(c("user/repo@0.1.0"))` writes `repo` to DESCRIPTION.
- Tests pass and pre-commit passes.

---

## E3: Global Command Installation for `exec/intent`

### Problem

`exec/intent` exists as a package-owned script entry point, but there is no story
for how users get `intent` on their `PATH` for terminal-first usage. The CLI
design document (`docs/03-cli-design.md`) and architecture document
(`docs/02-architecture.md`) emphasize "Terminal First, R Native," but without a
PATH story, the terminal experience requires typing the full path to
`exec/intent`.

Refactor 005 (CLI Entry Point) explicitly deferred this: "decide how to install or
expose a global intent command outside the package library."

### Goal

Decide on and document the global command installation approach. This is primarily
a product and documentation decision, not a code change.

### Non-Goals

- Do not build an installer or modify the user's PATH automatically.
- Do not add a CRAN-style installed executable (R's package installation mechanism
  for executables varies by platform).
- Do not add shell completion.

### Proposed Design

Three options, presented for decision:

**Option A: Manual symlink / PATH addition (document only)**

Document in the README's installation section that users can add to their PATH or
symlink:

```bash
# Option 1: Add the exec directory to PATH (in .bashrc/.zshrc)
export PATH="$PATH:$(Rscript -e 'cat(system.file("exec", package="intent"))')"

# Option 2: Symlink to a directory already on PATH
ln -s $(Rscript -e 'cat(system.file("exec", package="intent"))')/intent \
  /usr/local/bin/intent
```

- **Pro:** Zero code. Works on all platforms (with platform-appropriate commands).
- **Con:** Manual setup. Different commands per platform.

**Option B: Add `intent::install_cli(path)` helper**

Add an R function that creates a symlink or wrapper script at the specified path:

```r
intent::install_cli("/usr/local/bin")
```

- **Pro:** Single command works across platforms.
- **Con:** Requires write access to the target directory. Cross-platform symlink
  support is tricky (Windows requires admin or developer mode).

**Option C: Do nothing (status quo)**

Leave `exec/intent` as-is and document that `intent` is primarily an R package
with an optional CLI for advanced users.

- **Pro:** Zero effort. No support burden.
- **Con:** Conflicts with the "Terminal First" product principle.

### Recommendation

**Option A** for now — document the manual PATH setup in the README. This is the
simplest approach that honors the terminal-first direction without over-investing
in cross-platform installer logic before the CLI surface stabilizes.

Option B can be reconsidered after the `--json` output (refactor 010) and other
CLI features are implemented and the CLI is more mature.

### Implementation Steps

1. Add a "CLI Setup" subsection to the README's "Installation" section documenting
   how to add `exec/intent` to the PATH.
2. Include platform-specific instructions (Linux/macOS via `ln -s`, Windows via
   a batch wrapper or direct `Rscript` call).

### Acceptance Criteria

- README documents at least one way for users to run `intent` from the terminal
  without typing the full `exec/intent` path.
- Instructions are platform-aware.
- No code changes required.

---

## Test Plan (Combined)

- E1: Run `air format` on the codebase and verify no changes.
- E2: Run `devtools::test()` including new `extract_pkg_name()` unit tests.
- E3: Follow the documented CLI setup instructions manually and verify
  `intent --help` works.
- Run `pre-commit run --all-files` after all changes.

## Acceptance Criteria (Combined)

- `air.toml` is resolved (populated or removed).
- `cmd_add()` correctly extracts package names from version-annotated references.
- README documents the CLI PATH setup.
- Existing tests and behavior are unchanged.

## Result / Follow-Up Notes

Fill this in after implementation.

- Result: E1: Populated `air.toml` with explicit tidyverse-style formatting
  configuration (`style = "tidy"`, `line_width = 80`, `indent_width = 2`) and
  kept it in `.Rbuildignore`.
  E2: Added `extract_pkg_name()` helper in `R/utils.R` that strips `user/repo`
  path prefixes and `@version` suffixes. Replaced `basename(pkg)` in `cmd_add()`.
  Added unit tests. E3: Added a "CLI Setup" subsection to the README's
  Installation section with platform-specific instructions.
- Follow-up work: Revisit global command installation (Option B:
  `intent::install_cli()` helper) after the CLI matures with JSON output and
  sync pruning.
