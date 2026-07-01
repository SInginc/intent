# renv Boundary

`renv` is useful and should remain part of the first implementation. The design
problem is not that `intent` uses `renv`; the problem is when `intent` becomes
defined by `renv` session behavior.

## Target Relationship

`intent` owns the product model:

- project intent
- dependency declaration
- drift detection
- operation planning
- user-facing commands

`renv` owns backend mechanics:

- project-local library management
- lockfile writing and reading
- package restore
- package removal where appropriate
- activation files where appropriate

## Boundary Rules

1. Public commands should accept or discover a project path before they call the
   backend.
2. Core functions should not call `renv::project()`.
3. Direct calls to `renv` should be isolated in adapter functions.
4. Tests for parsing, planning, and command semantics should not need network
   access or a real package install.
5. Backend tests may use `renv`, but they should be marked or organized as
   integration tests.

## Current Coupling Risks

Current implementation patterns that should be reduced:

- `renv::project()` used as a global source of truth.
- `sync(project = ...)` accepting a project path while still requiring active
  `renv` session state.
- `add()` and `remove()` relying on loaded backend state before resolving the
  project.
- Repository loading tied to `renv::project()` rather than an explicit project
  path.

These patterns make command behavior harder to reason about and harder to expose
through a terminal CLI.

## Adapter Contract

The first adapter can be called `renv_backend`. Its contract should be narrow:

```r
renv_backend_init(project, repos)
renv_backend_install(project, packages, repos)
renv_backend_remove(project, packages)
renv_backend_lock(project)
renv_backend_restore(project)
renv_backend_read_state(project)
```

The rest of the package should not need to know how those functions work.

## Explicit Project Resolution

Project resolution should happen once near the top of each command.

Possible resolution order:

1. Explicit `project` argument.
2. Current working directory if it contains `DESCRIPTION`.
3. Parent directory search for `DESCRIPTION`.
4. Backend-specific active project only as a fallback.

If resolution fails, return a clear error:

```text
No intent project found. Run `intent init` or pass `project =`.
```
