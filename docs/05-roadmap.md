# Roadmap

This roadmap moves the project from the current implementation toward a clear
product and architecture. It intentionally starts with documentation and design
before deeper code changes.

## Phase 0: Stabilize Direction

- Add development docs under `docs/`.
- Define the problem statement.
- Define the product principles.
- Define the `renv` boundary.
- Decide that the CLI and R API share one command layer.

Exit criteria:

- Contributors can explain what `intent` is and what it is not.
- New code can be reviewed against documented principles.

## Phase 1: Make Project Resolution Explicit

- Add a project resolution helper.
- Stop using `renv::project()` in core logic.
- Pass `project` paths through internal functions.
- Make `sync(project = ...)` work without requiring active `renv` state unless
  the backend operation itself requires it.

Exit criteria:

- Core parsing and planning tests run without a loaded `renv` project.
- Public commands have predictable project discovery.

## Phase 2: Separate Core from Backend

- Move manifest parsing and writing into core functions.
- Move lockfile/library operations into a backend adapter.
- Introduce operation plans for `add`, `remove`, `sync`, and `status`.
- Split unit tests from integration tests.

Exit criteria:

- Most tests do not install packages.
- Backend-specific behavior is isolated.

## Phase 3: Add Status and Dry Run

- Implement `intent::status()`.
- Add `dry_run = TRUE` support to mutating commands.
- Report drift between manifest, lockfile, and local library.

Exit criteria:

- Users can inspect what `intent` would change before changing it.

## Phase 4: Add CLI

- Add an installed CLI entry point.
- Implement `intent init`, `intent add`, `intent remove`, `intent sync`, and
  `intent status`.
- Ensure CLI commands call the same command layer as the R API.
- Add JSON output for `status`.

Exit criteria:

- A user can manage a project without opening an interactive R session.

## Phase 5: Refine Dependency Overrides

- Decide whether dependency overrides belong in `Config/intent/`.
- Define supported source formats.
- Define how override repositories interact with project repositories.
- Add validation and clear error messages.

Exit criteria:

- Overrides are documented, tested, and independent of backend internals.

## Immediate Next Decisions

The next implementation pass should answer:

- Should `init()` have a default repository, or should it require explicit
  repository configuration?
- What is the exact project discovery algorithm?
- What data structure represents drift?
- Which tests are unit tests and which are integration tests?
