# Product Principles

## 1. Intent Is the Product Model

The product is built around the distinction between intent and state.

- Intent is what the project says it wants.
- State is what the machine, lockfile, and local library currently contain.
- Operations are the steps needed to make state match intent.

The user should not need to understand backend details to perform common tasks.

## 2. Terminal First, R Native

`intent` should have a clear terminal entry point. The CLI is the primary mental
model for new users:

```sh
intent add dplyr
```

The R API remains important, but it should be a programmatic interface to the
same command model:

```r
intent::add("dplyr")
```

Neither interface should be a second-class wrapper around the other.

## 3. DESCRIPTION Is the User-Facing Manifest

The user-facing manifest should be `DESCRIPTION`, because it is already a
standard R project file. `intent` may add fields under `Config/intent/`, but it
should avoid inventing a separate manifest unless a future design document
justifies that decision.

The direct dependency sections are:

- `Imports` for runtime dependencies.
- `Suggests` for development or optional dependencies.

Additional `Config/intent/` fields should represent product policy, not hidden
backend state.

## 4. Backends Are Replaceable

`renv` and `pak` should be backend collaborators, not the core product model.
`intent` is a stricter project contract layer above those tools.

The core should be able to say:

- Read intent.
- Read state.
- Compare intent and state.
- Plan operations.
- Execute operations through an adapter.
- Normalize backend output before it becomes project state.

The adapter may use `renv`, `pak`, or another backend later.

The product contract is:

```text
DESCRIPTION declares.
intent enforces.
renv executes.
renv.lock records.
```

`renv.lock` is essential for reproducibility, but it should not become the
source of project policy. Policy belongs in `DESCRIPTION` and
`Config/intent/`.

## 5. No Hidden Session Magic

Public commands should not require the user to already be in a perfectly loaded
R session unless the command truly needs that. Prefer explicit project paths and
clear discovery rules over implicit `renv::project()` behavior.

Commands should also avoid depending on ambient `.libPaths()`, `options(repos)`,
or packages that happen to be installed in the caller's library. Those values
may differ by machine, platform, CI runner, or interactive session.

Good:

```r
intent::sync(project = ".")
```

Risky:

```r
renv::project()
```

## 6. Drift Must Be Visible

`intent` should make drift between manifest, lockfile, and library visible.
Before changing state, users should be able to inspect what will happen.

This implies a future `status` command and a dry-run mode for mutating commands.

## 7. Small Surface, Strong Guarantees

The first stable API should be small:

- `init`
- `add`
- `remove`
- `sync`
- `status`

Each command should have precise input, output, side effects, and failure modes.
