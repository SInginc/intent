# Architecture

`intent` should be structured as a small product core with separate interfaces
and backend adapters.

## Target Layers

```text
CLI / R API
    |
Command Layer
    |
Intent Core
    |
Backend Adapters
```

## Interfaces

Interfaces translate user input into command calls.

Examples:

- CLI: `intent add dplyr`
- R API: `intent::add("dplyr")`

Interface code should be thin. It should parse arguments, call the command
layer, print useful output, and return structured results where appropriate.

## Command Layer

The command layer owns product workflows. It answers what should happen for
each user command.

Examples:

- `cmd_init(project, repos, backend)`
- `cmd_add(project, packages, dev, backend)`
- `cmd_remove(project, packages, backend)`
- `cmd_sync(project, backend)`
- `cmd_status(project, backend)`

Commands should not directly manipulate `renv` internals. They should call core
functions and backend adapters.

## Intent Core

The core owns product concepts and should be easy to test without installing
packages.

Core responsibilities:

- Locate a project.
- Read and write manifest intent.
- Parse `Config/intent/` fields.
- Read lockfile state through an abstract state reader.
- Compare manifest dependencies to locked dependencies.
- Build operation plans.

Potential core objects:

- `IntentProject`
- `IntentManifest`
- `IntentState`
- `DependencySpec`
- `OperationPlan`

These do not need to be formal classes immediately. The important rule is that
core functions should operate on explicit data rather than hidden session state.

## Backend Adapters

Backends execute operations.

The first backend can use `renv` and `pak`, but it should be accessed through a
small adapter surface:

- `backend_init(project, repos)`
- `backend_install(project, packages, repos)`
- `backend_remove(project, packages)`
- `backend_lock(project)`
- `backend_restore(project)`
- `backend_read_state(project)`

This keeps `renv` from spreading through the codebase.

## Current Architecture Smell

The current package mixes interface, workflow, core, and backend behavior in the
same functions. For example, public functions currently read `renv::project()`
directly and call `renv`/`pak` operations inline.

That creates these risks:

- Circular workflow assumptions between `init()`, `sync()`, and `renv::load()`.
- Commands that only work after a backend has already activated.
- Tests that require network or package installation for logic that should be
  unit-testable.
- No clean path to a CLI because there is no shared command layer.

## Refactoring Direction

Refactor toward this dependency direction:

```text
R API / CLI -> commands -> core -> adapters
```

Avoid this direction:

```text
core -> R API
core -> CLI
core -> renv session globals
commands -> public functions
```

No lower layer should call a higher layer.
