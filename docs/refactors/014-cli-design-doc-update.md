# CLI Design Document Update

Status: `implemented`

## Problem

The CLI implementation has advanced beyond the design document
(`docs/03-cli-design.md`). Several flags are implemented but undocumented:

- `--project <path>`: accepted by `add`, `remove`, `sync`, `status`. Not
  mentioned anywhere in the CLI design doc.
- `--json`: supported on `status` and `sync --dry-run`. Documented only as
  a one-line example (`intent status --json`) without specification of
  output format or behavior.
- `--prune` / `--no-prune`: supported on `sync`. Not in the design doc.
- `--dry-run`: documented as a flag example but its semantics (returns a
  structured plan, no mutation) are not spelled out.

Additionally, the documented output style (structured sections like
"Added:", "Updated:") does not match the current `message()`-based output.

## Goal

Update `docs/03-cli-design.md` to accurately describe the current CLI
implementation. No code changes — this is a documentation-only refactor.

## Non-Goals

- Do not change the CLI output format.
- Do not add new flags.
- Do not remove existing flags.

## Proposed Design

Add the following sections to `docs/03-cli-design.md`:

### Common Flags

Document flags accepted by multiple commands:

```
--project <path>   Override automatic project discovery with an
                   explicit path. Accepted by add, remove, sync,
                   and status.

--dry-run          Report planned actions without changing any
                   files or installed packages. Accepted by add,
                   remove, and sync. Returns a machine-readable
                   plan when combined with --json.

--json             Output machine-readable JSON instead of
                   human-readable text. Accepted by status and
                   sync --dry-run.
```

### Per-Command Flag Tables

| Command | Flags |
|---------|-------|
| `init` | `--repo NAME=URL` (repeatable) |
| `add` | `--project`, `--dev`, `--dry-run` |
| `remove` | `--project`, `--dry-run` |
| `sync` | `--project`, `--dry-run`, `--json`, `--prune`, `--no-prune` |
| `status` | `--project`, `--json` |

### JSON Output Format

Document the JSON schema for `intent status --json` and
`intent sync --dry-run --json`.

## Implementation Steps

1. Read `docs/03-cli-design.md`.
2. Add a "Common Flags" section between the command shape examples and
   per-command semantics.
3. Update per-command semantics to include full flag lists.
4. Add JSON output format documentation.
5. Update the output style section to match current implementation.

## Acceptance Criteria

- All currently implemented flags are documented.
- Flag documentation matches actual behavior in `R/cli.R`.
- JSON output format is described.
- No code changes.

## Result / Follow-Up Notes

Fill this in after implementation.

- Result: Rewrote `docs/03-cli-design.md` to match current implementation.
  Added Common Flags section, per-command flag tables, JSON output format
  documentation, and updated command shape examples to include all supported
  flags (`--project`, `--json`, `--prune`, `--no-prune`).
- Follow-up work: None.
