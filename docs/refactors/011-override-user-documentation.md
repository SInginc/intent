# Override User Documentation

Status: `implemented`

## Problem

The dependency override feature is fully implemented (`R/desc.R`,
`parse_override()`, `get_intent_overrides()`) and tested
(`tests/testthat/test-overrides.R`), but there is zero user-facing documentation.
Users cannot discover the feature or learn the
`Config/intent/Imports/pkg: pkg@version@source` format without reading the source
code.

Refactor 006 (Dependency Override Validation) explicitly noted this in its
follow-up: "Document override configuration in user-facing docs once the CLI and
status output settle."

The CLI and status output have now settled (refactors 004, 005 implemented), so
this documentation gap is ready to be closed.

## Goal

Add user-facing documentation for dependency overrides that covers:

- What overrides are and when to use them.
- The `package@version@source` format.
- All supported source types with examples.
- Error messages and how to fix common mistakes.

## Non-Goals

- Do not change the override implementation or validation logic.
- Do not add a new vignette (a README section is sufficient for this scope).
- Do not redesign the `Config/intent/` field structure.
- Do not document internal implementation details — keep it user-facing.

## Current State

The override feature works as follows (from `R/desc.R`):

- Users add fields like `Config/intent/Imports/pkgname: pkg@version@source` to
  their `DESCRIPTION` file.
- `get_intent_overrides()` reads these fields, parses them with `parse_override()`,
  and returns resolved pak references and extra repositories.
- `intent_install()` applies overrides when installing packages.
- Supported sources: `cran`, `standard`, `github`, `bioc`, `local`, `url`, and
  `http(s)://` repository URLs.
- Unknown sources and malformed override strings produce clear errors.

The feature is explained in the refactor doc (006) but not in the README or any
other user-facing documentation.

## Proposed Design

Add a new section to the README, placed after the existing "Key Functions" section
and before the "Error Handling" section, titled "Dependency Overrides."

The section should include:

### Format

```text
Config/intent/Imports/<pkg>: <package>@<version>@<source>
Config/intent/Suggests/<pkg>: <package>@<version>@<source>
```

### Supported Sources Table

| Source | Example | Description |
|--------|---------|-------------|
| `cran` | `dplyr@1.1.4@cran` | Install from CRAN at the specified version. |
| `standard` | `dplyr@1.1.4@standard` | Same as `cran`. |
| `github` | `tidyverse/dplyr@1.1.4@github` | Install from GitHub. Uses `user/repo` format. |
| `bioc` | `Biobase@3.18@bioc` | Install from Bioconductor. |
| `local` | `mypkg@0.1.0@local` | Install from a local source package. |
| `url` | `mypkg@0.1.0@url` | Install from a URL. |
| `https://...` | `dplyr@1.1.4@https://example.com/cran` | Install from a custom CRAN-like repository. |

### Why Use Overrides?

Brief explanation: when you need a specific version of a package from a
non-standard source, or when CRAN's latest version doesn't work for your project.

### Common Errors

- `Invalid override format` — missing or extra `@` separators.
- `Invalid override source` — unsupported source type.

### Complete Example

Show a DESCRIPTION snippet with multiple overrides:

```dcf
Config/intent/Imports/dplyr: dplyr@1.1.4@cran
Config/intent/Suggests/testthat: testthat@3.2.0@cran
Config/intent/Imports/mypkg: myorg/mypkg@0.1.0@github
```

### Alternative Locations Considered

- **Vignette:** Better for long-form tutorials, but the feature is simple enough
  that a README section suffices. A vignette can be added later if the override
  surface grows.
- **Dedicated `docs/overrides.md`:** Would keep the README shorter, but users are
  less likely to find it. The README is the first place users look.

## Implementation Steps

1. Add a "Dependency Overrides" section to `README.md` between the "Key
   Functions" section and the "Error Handling" section.
2. Include the format specification, supported sources table, usage examples, and
   common errors.
3. Update refactor 006's follow-up notes to reference the new README section.
4. No code changes required.

## Test Plan

- Visual review of the rendered README on GitHub.
- Verify all supported sources listed in the README match the actual supported
  sources in `parse_override()` error messages.
- Verify the examples in the README are valid override strings (test against
  `parse_override()`).
- Run `pre-commit run --all-files` to ensure no formatting issues.

## Acceptance Criteria

- README contains a "Dependency Overrides" section with format specification.
- All supported source types are documented with examples.
- Common error messages are listed with explanations.
- The section is placed in a logical location in the README flow.
- No code changes to the override implementation.

## Result / Follow-Up Notes

Fill this in after implementation.

- Result: Added a "Dependency Overrides" section to README.md between the
  "Key Functions" and "Error Handling" sections. Includes format specification,
  supported sources table, complete example, and common error messages.
- Follow-up work: Consider adding a vignette if the override surface grows
  beyond what a README section can comfortably cover.
