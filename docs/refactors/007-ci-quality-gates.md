# CI Quality Gates

Status: `planned`

## Problem

The GitHub Actions workflow runs `R CMD check`, but it does not run the same
quality gates we now rely on locally. In particular, CI does not check
`pre-commit`, `air` formatting, or roxygen-generated documentation drift.

This means a PR can pass CI while failing the local workflow.

## Goal

Update GitHub Actions so CI checks:

- R package correctness through `R CMD check`.
- Formatting and repository hygiene through `pre-commit run --all-files`.
- Generated documentation freshness through `devtools::document()` followed by
  a clean `git diff` for `NAMESPACE` and `man/`.

## Non-Goals

- Do not redesign the whole CI matrix.
- Do not add deployment or release automation.
- Do not add coverage upload.
- Do not change package code.

## Current State

`.github/workflows/R-CMD-check.yaml` runs a standard r-lib R CMD check matrix.
It triggers on pushes to `main` and `master`, and on pull requests targeting
`main`. The workflow does not run on direct pushes to `dev`.

## Proposed Design

Keep the existing R CMD check job and add a `quality` job on Ubuntu.

The `quality` job should:

1. Check out the repository.
2. Install Air with `posit-dev/setup-air@v1`.
3. Set up Python.
4. Run `pipx run pre-commit run --all-files`.
5. Set up R.
6. Install R dependencies needed for documentation.
7. Run `Rscript -e 'devtools::document()'`.
8. Run `git diff --exit-code NAMESPACE man`.

Also make `pull_request.branches` explicit and include `dev` in push triggers so
direct dev pushes get feedback.

## Implementation Steps

1. Update workflow triggers.
2. Add a `quality` job.
3. Keep the existing R CMD check matrix intact.
4. Run local pre-commit and YAML validation.

## Test Plan

- Run `pre-commit run --files .github/workflows/R-CMD-check.yaml`.
- Run `pre-commit run --all-files`.
- Inspect workflow syntax locally.

## Acceptance Criteria

- CI includes a quality job for pre-commit, Air, and roxygen drift.
- Existing R CMD check job remains present.
- Pushes to `dev` trigger CI.
- Workflow YAML passes local pre-commit checks.

## Result / Follow-Up Notes

Fill this in after implementation.

- Result:
- Follow-up work:
