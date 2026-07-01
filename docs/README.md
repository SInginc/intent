# intent Development Docs

This directory defines the product and engineering direction for `intent`.
The documents are written for contributors before implementation work begins.
They should be treated as the source of truth when deciding whether a code
change belongs in this package.

## Documents

- [00 Problem](00-problem.md): the problem `intent` exists to solve.
- [01 Product Principles](01-product-principles.md): design rules for product and API decisions.
- [02 Architecture](02-architecture.md): the intended internal structure.
- [03 CLI Design](03-cli-design.md): the terminal-first user interface.
- [04 renv Boundary](04-renv-boundary.md): how `intent` should use `renv` without becoming `renv`.
- [05 Roadmap](05-roadmap.md): staged work to move from the current package to the intended design.
- [Refactors](refactors/README.md): implementation-level plans for individual refactoring efforts.

## Working Rule

When implementation and documentation disagree, do not quietly follow the code.
Either update the documentation because the product decision changed, or update
the code because it drifted from the design.

Every refactoring should have a plan under `docs/refactors/` before code changes
begin. The plan should be updated after execution with the result and any
follow-up work.
