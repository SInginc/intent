# Dependency Override Validation

Status: `implemented`

## Problem

Dependency overrides exist in code, but their format and validation rules are
not explicit enough. `parse_override()` currently accepts unknown source types
and treats them as pak prefixes, which makes typo-driven configuration errors
hard to catch.

Overrides are a powerful feature because they affect installation resolution.
They should fail clearly when the configuration is malformed.

## Goal

Make dependency override parsing strict and documented in code/tests.

Supported override format:

```text
package@version@source
```

Supported sources:

- `cran`
- `standard`
- `github`
- `bioc`
- `local`
- `url`
- `http://...` or `https://...` repository URLs

Unknown sources should error instead of being guessed.

## Non-Goals

- Do not redesign the `Config/intent/Imports` or `Config/intent/Suggests`
  location.
- Do not implement a full package source schema.
- Do not validate whether remote packages actually exist.
- Do not change backend installation behavior beyond receiving cleaner package
  refs and repositories.

## Current State

`get_intent_overrides()` extracts override strings from DESCRIPTION and
`parse_override()` turns them into pak refs. URL sources are converted to extra
repositories. Unknown sources are accepted.

This makes the feature flexible but too easy to misconfigure.

## Proposed Design

Keep the existing `package@version@source` format, but require exactly three
non-empty fields.

Behavior:

- `cran` and `standard`: package ref is `name@version`.
- `github`, `bioc`, `local`, and `url`: package ref is
  `source::package@version`.
- `http://...` and `https://...`: package ref is `name@version`, and the URL is
  added as an override repository.
- Unknown source values produce a clear error.

## Implementation Steps

1. Tighten `parse_override()` validation.
2. Add tests for malformed formats, empty fields, and unknown source values.
3. Keep existing supported source tests passing.
4. Regenerate documentation.
5. Run tests and pre-commit.

## Test Plan

- Unit test valid CRAN, URL repository, GitHub, Bioconductor, local, and URL pak
  refs.
- Unit test malformed override strings.
- Unit test unknown source values.
- Run `devtools::document()`.
- Run `devtools::test()`.
- Run `pre-commit run --all-files`.

## Acceptance Criteria

- Unknown override sources fail clearly.
- Malformed override strings fail clearly.
- Existing supported override behavior still works.
- Tests and pre-commit pass.

## Result / Follow-Up Notes

- Result: Tightened override parsing to require exactly three non-empty fields
  and reject unknown sources. Added tests for supported CRAN, repository URL,
  GitHub, Bioconductor, local, URL pak refs, malformed strings, and source
  typos.
- Follow-up work: Document override configuration in user-facing docs once the
  CLI and status output settle.
