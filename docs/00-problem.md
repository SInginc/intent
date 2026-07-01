# Problem

R has strong dependency tooling, but day-to-day project dependency management is
still too fragmented for many users.

A project author often needs to understand and coordinate several separate
concepts:

- `DESCRIPTION` as the manifest of direct dependencies.
- `renv.lock` as the lockfile of resolved dependency state.
- A project-local library as the installed state.
- Repository configuration as package source policy.
- `pak`, `renv`, and base R installation behavior as separate operational tools.

Each tool is useful, but the user workflow is not unified. Users are asked to
decide which file is authoritative, when to install, when to snapshot, when to
restore, and how to repair drift.

`intent` exists to make that workflow explicit and boring.

## Core Problem Statement

`intent` helps R users declare what a project intends to depend on, then brings
the local dependency state into alignment with that declaration through one
clear interface.

The package should answer these questions for the user:

- What dependencies does this project intentionally use?
- What dependency state is currently locked?
- What is installed locally?
- What operations are needed to make state match intent?
- What command should I run next?

## Non-Goals

`intent` is not:

- A replacement for CRAN, Posit Package Manager, R-multiverse, or Bioconductor.
- A general package resolver implemented from scratch.
- A complete reimplementation of `renv`.
- A GUI project manager.
- A tool that infers intent by scanning arbitrary source files.

## Current Pain

The current implementation is too tightly coupled to `renv` internals and active
`renv` session state. This makes the package feel like a wrapper around `renv`
rather than a product with its own model.

Specific pain points:

- Public functions call `renv::project()` directly.
- The current entry point is primarily the R API, even though the product
  promise resembles a terminal-first workflow.
- The package does not yet have a clear internal boundary between product logic,
  package operations, and backend execution.
- Documentation and code can drift because there is no development spec.

## Desired User Experience

The target experience should be simple:

```sh
intent init
intent add dplyr
intent remove ggplot2
intent sync
intent status
```

R users should also be able to call the same behavior from R:

```r
intent::init()
intent::add("dplyr")
intent::sync()
intent::status()
```

The CLI and R API should share one internal command layer. They should not grow
separate behavior.
