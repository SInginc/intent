# Refactoring Plans

This directory contains implementation-level plans for refactoring work.
Each refactor should be documented before code changes begin, reviewed as a
plan, and updated after implementation with the actual result.

## Workflow

1. Create a new document from [000 Template](000-template.md).
2. Scope the document to one architectural concern.
3. Mark the document as `planned`.
4. Review and revise the plan before implementation.
5. During implementation, mark the document as `in-progress` if useful.
6. After implementation, mark it as `implemented` and fill in the result notes.
7. If the plan is replaced by another approach, mark it as `superseded` and link
   to the replacement.

## Status Values

- `planned`: accepted as the intended direction, but not yet implemented.
- `in-progress`: implementation has started.
- `implemented`: implementation is complete and the result section has been
  updated.
- `superseded`: the plan was replaced by another plan or decision.

## Rules

- Every refactor must have a plan document before code changes.
- Each document should cover one concern, not a full roadmap phase.
- Implementation should follow the accepted plan unless the plan is updated
  first.
- The result section should record what actually changed, even if the final
  implementation differs from the original plan.

## Plans

- [001 Explicit Project Resolution](001-explicit-project-resolution.md)
- [002 Backend Boundary](002-backend-boundary.md)
