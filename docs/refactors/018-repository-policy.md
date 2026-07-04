# Repository Policy

Status: `implemented`

## Problem

`intent` treats `DESCRIPTION` as the project intent file and `renv.lock` as the
resolved state file. Repository configuration is already stored in
`DESCRIPTION` under `Config/intent/repos/`, but the semantics are still thin:

- `init()` silently writes a default repository when no repository is provided.
- `sync()`, `add()`, and `remove()` use configured repositories for package
  operations, but they do not validate whether the resulting lockfile packages
  came from approved sources.
- Dependency overrides can introduce GitHub, Bioconductor, local, URL, or custom
  repository sources, but there is no project-level policy that says which
  source classes are allowed.
- Self-hosted package manager users need `intent` to avoid assuming CRAN,
  Posit Package Manager, GitHub, or R-universe as privileged sources.

This creates a product gap. If `DESCRIPTION` is the project's source of intent,
it should describe not only which packages are wanted, but also which package
sources are trusted for this project.

This is also a product principle. `intent` is a stricter layer on top of `renv`,
not a loose wrapper around R's default package behavior. R's package workflows
often tolerate ambient repositories, session-specific options, and loosely
defined restore state. `intent` should move in the opposite direction: the
project environment should be explicitly declared, reviewable, and reproducible.

## Goal

Define a repository and source policy model in `DESCRIPTION` that:

- Makes configured repositories explicit and reviewable.
- Lets `init()` surface the repository choice before it becomes project policy.
- Lets `status()` report packages whose resolved source is outside project
  policy.
- Lets `sync()` optionally warn or fail when lockfile provenance violates
  project policy.
- Works for CRAN, Posit Package Manager, R-universe, self-hosted package
  managers, GitHub, Bioconductor, URL, and local sources.

## Non-Goals

- Do not implement the policy in this refactor document.
- Do not implement cross-platform reproducibility CI in this refactor document.
- Do not redesign the entire dependency override format.
- Do not hard-code a single canonical source for `intent` itself.
- Do not require interactive prompts in non-interactive R sessions or CI.
- Do not try to make `renv.lock` unnecessary. The lockfile remains the resolved
  state.

## Current State

Repository configuration currently uses fields like:

```dcf
Config/intent/repos/RSPM: https://packagemanager.posit.co/cran/latest
```

`cmd_init()` writes that default when neither the `repos` argument nor an
existing `DESCRIPTION` provides repositories.

Dependency overrides currently use fields like:

```dcf
Config/intent/Imports/dplyr: dplyr@1.1.4@cran
Config/intent/Imports/mypkg: myorg/mypkg@0.1.0@github
Config/intent/Suggests/pkg: pkg@0.1.0@https://example.com/cran
```

`get_intent_overrides()` parses those fields and `intent_install()` uses the
resolved pak references. Overrides may also add extra repositories for a single
operation.

The current implementation does not inspect `renv.lock` provenance beyond
package names and versions. It does not verify that all repository packages came
from configured repositories, nor that non-repository sources are allowed by
project policy.

## Proposed Design

### DESCRIPTION as Policy

Use `DESCRIPTION` as the durable project policy file. The policy should remain
plain DCF fields under `Config/intent/` so it is easy to review in diffs and easy
for existing R tooling to preserve.

Keep the existing repository field:

```dcf
Config/intent/repos/<name>: <url>
```

Add optional source policy fields:

```dcf
Config/intent/source-policy/mode: warn
Config/intent/source-policy/allow/repository: true
Config/intent/source-policy/allow/github: false
Config/intent/source-policy/allow/bioc: false
Config/intent/source-policy/allow/url: false
Config/intent/source-policy/allow/local: false
Config/intent/source-policy/allow/unknown: false
```

`mode` values:

- `off`: do not check provenance.
- `warn`: report provenance violations but continue.
- `error`: fail mutating commands when provenance violates policy.

Default policy:

```dcf
Config/intent/source-policy/mode: warn
Config/intent/source-policy/allow/repository: true
Config/intent/source-policy/allow/github: true
Config/intent/source-policy/allow/bioc: true
Config/intent/source-policy/allow/url: true
Config/intent/source-policy/allow/local: true
Config/intent/source-policy/allow/unknown: false
```

Rationale: This default preserves current flexibility while making unknown
sources visible. Stricter organizations can switch to `error` and disable
specific source classes.

### Source Classes

`allow/*` controls source classes, not individual packages:

- `repository`: CRAN-like repositories, including CRAN, Posit Package Manager,
  R-universe, and self-hosted package managers. These sources are allowed only
  when the repository name and URL match `Config/intent/repos/`.
- `github`: GitHub references such as `user/repo`, usually introduced by an
  override.
- `bioc`: Bioconductor references.
- `url`: Direct package archive URLs or ad hoc repository URLs.
- `local`: Local package paths.
- `unknown`: Lockfile records without enough source metadata to classify.

Defaulting `github`, `bioc`, `url`, and `local` to `true` preserves the current
override surface. Organizations that require only approved repositories can set
those fields to `false` and use `mode: error`.

Defaulting `unknown` to `false` is intentional. Unknown sources should be visible
because they weaken reproducibility, but the default `mode: warn` keeps existing
projects usable.

Tool packages are exempt from source policy checks:

- `intent`
- `renv`
- `pak`

These are bootstrap/backend tools rather than ordinary project dependencies.
`intent` may be hydrated locally, `renv` has its own bootstrap behavior, and
`pak` is the installation engine. They should not cause a new project to report
policy violations immediately after `init()`.

### Repository Names and URLs as an Allowlist

For lockfile records with repository-like provenance, `intent` should compare
both the resolved repository name and URL against the repositories declared in
`Config/intent/repos/`.

The checker should recognize at least:

- Lockfile `Repository` values such as `CRAN`, `RSPM`, or named repositories.
- Lockfile source records that include a repository URL.
- Package records installed from Posit Package Manager, CRAN mirrors,
  R-universe, or self-hosted CRAN-like repositories.

Repository name mismatch is a policy violation. If `DESCRIPTION` declares:

```dcf
Config/intent/repos/RSPM: https://packagemanager.posit.co/cran/latest
```

then a lockfile record from `Repository: CRAN` is not considered equivalent even
if the URL points to the same service. The names in `renv.lock` and
`DESCRIPTION` must agree. This keeps repository policy reviewable and avoids
ambiguous aliases.

Matching rule:

1. The lockfile repository name must exist in `Config/intent/repos/`.
2. When the lockfile includes a repository URL, it must match that named
   repository's URL.
3. URL matching should normalize harmless differences such as trailing slashes,
   but should not silently equate different hosts or paths.

### `init()` Repository Confirmation

`init()` should make the initial repository policy visible.

R API proposal:

```r
intent::init(
  path = ".",
  repos = NULL,
  confirm_repos = interactive()
)
```

Behavior:

- If `repos` is provided, write it without prompting.
- If existing `DESCRIPTION` has `Config/intent/repos/`, reuse it without
  prompting.
- If no repos are configured and `repos = NULL`, propose the default repository.
- In interactive sessions, ask the user to confirm or provide repositories.
- In non-interactive sessions, use the default, write it to `DESCRIPTION`, and
  print a message explaining how to declare repositories explicitly.

In non-interactive R usage, users declare project repositories through the
existing `repos` argument:

```r
intent::init(
  repos = c(INTERNAL = "https://r.example.com/packages/latest")
)
```

They may also pre-create or edit `DESCRIPTION`:

```dcf
Config/intent/repos/INTERNAL: https://r.example.com/packages/latest
```

The fallback default is not hidden session state. It is written into
`DESCRIPTION`, committed with the project, and can be reviewed or changed like
any other project policy.

CLI proposal:

```text
intent init
```

In an interactive terminal, show the proposed repository and ask for
confirmation. Provide flags for automation:

```text
intent init --repo CRAN=https://example.com
intent init --yes
intent init --no-default-repo
```

Use `confirm_repos` for R API clarity and `--yes`/`--repo` for CLI automation.
Do not add `repos = "ask"` unless `confirm_repos` proves insufficient.

### Provenance Checks in `status()`

Extend `intent_status` with source policy information:

```r
source_policy = list(
  mode = "warn",
  allowed_sources = c("repository", "github", "bioc", "url", "local"),
  allowed_repositories = c(RSPM = "https://packagemanager.posit.co/cran/latest"),
  exempt_packages = c("intent", "renv", "pak")
)
source_violations = data.frame(
  package = character(),
  source = character(),
  repository = character(),
  reason = character()
)
```

Human output should summarize violations:

```text
Source policy violations: 2
  - pkgA: repository URL is not declared in Config/intent/repos
  - pkgB: unknown source is not allowed
```

JSON output should include machine-readable violations.

### Provenance Checks in Mutating Commands

Mutating commands should check policy before replacing the official lockfile.

Behavior by mode:

- `off`: do nothing.
- `warn`: print warnings and continue.
- `error`: stop with a clear message before writing an invalid `renv.lock`.

The checker has two stages:

1. **Preflight requested sources.** Before installation, validate source classes
   implied by the user request and override configuration. For example, a GitHub
   override can fail immediately when `allow/github: false`.
2. **Validate a candidate lockfile.** After installation, write the next lockfile
   to a temporary path, validate its provenance, and replace `renv.lock` only if
   the candidate satisfies policy or policy mode is `warn` / `off`.

Preferred implementation for `add()` and `sync()`:

1. Validate requested package refs and overrides against source policy.
2. Install or remove packages in the project library as needed.
3. Snapshot to a temporary lockfile path.
4. Validate the temporary lockfile.
5. If valid, replace the official `renv.lock`.
6. If invalid and mode is `error`, leave the existing `renv.lock` unchanged and
   tell the user how to fix the repository policy or dependency source.

This is not fully transactional because the project library may already have
changed, but it keeps the official lockfile from recording policy-violating
state.

### Overrides and Policy

Overrides should not bypass policy. If a user writes:

```dcf
Config/intent/Imports/mypkg: myorg/mypkg@0.1.0@github
```

then one of these must be true:

```dcf
Config/intent/source-policy/allow/github: true
```

or source policy mode must be `off`.

For custom repository override URLs:

```dcf
Config/intent/Imports/pkg: pkg@1.0.0@https://example.com/cran
```

the repository URL should be allowed only if it is declared in
`Config/intent/repos/` or if a future policy explicitly allows ad hoc repository
URLs.

Default behavior: ad hoc repository URLs are allowed only in `warn` / `off`
mode. In `warn` mode they are reported as undeclared repositories. In `error`
mode they fail unless the repository is declared under `Config/intent/repos/`
with the same name and URL.

### Unknown Sources

Local hydration of `intent` can create lockfile records with unknown or local
source metadata. This is acceptable for the bootstrap lifecycle.

Recommended policy:

- Exempt `intent`, `renv`, and `pak` from provenance violations.
- Exclude the `intent` tool package from restore, as already implemented.
- Default `allow/unknown: false` for ordinary project dependencies.
- Document that users who need fully reproducible project-local `intent` should
  install it from a declared repository source.

## Implementation Steps

1. Add a repository/source policy parser in the DESCRIPTION layer.
2. Add a lockfile provenance extractor that normalizes each package into:
   package name, source class, repository name, repository URL, and raw record.
3. Add a policy checker that returns structured violations.
4. Extend `intent_status` and JSON output with source policy and violations.
5. Add warning output to human `status()` printing.
6. Add temporary-lockfile snapshot support so `add()` and `sync()` can validate
   before replacing `renv.lock`.
7. Run source preflight before installation and candidate-lockfile checks before
   replacing `renv.lock`.
8. Add `confirm_repos` to `init()` and CLI `--yes` / `--repo` behavior as
   appropriate.
9. Update README and CLI design docs.

## Test Plan

- Unit tests for parsing `Config/intent/repos/`.
- Unit tests for parsing source policy fields and defaults.
- Unit tests for lockfile provenance extraction across repository, GitHub,
  Bioconductor, URL, local, and unknown records.
- Unit tests for policy checker violations.
- Unit tests that repository name mismatch is a violation even when URLs look
  similar.
- Unit tests that `intent`, `renv`, and `pak` are exempt.
- Unit tests for `status()` JSON output containing source violations.
- Unit tests that `error` mode validates a temporary lockfile before replacing
  `renv.lock`.
- CLI tests for `init --repo`, `init --yes`, and non-interactive default
  behavior.
- Integration test using a temporary project with a package resolved from the
  declared repository.
- Sandbox end-to-end test extension for a policy warning path.

## Acceptance Criteria

- `DESCRIPTION` clearly records both repositories and source policy.
- `init()` makes the default repository choice visible and automatable.
- `status()` reports packages whose lockfile provenance violates policy.
- `sync()` and `add()` respect policy mode (`off`, `warn`, `error`) without
  writing a policy-violating official lockfile in `error` mode.
- Repository names in `renv.lock` must match names declared in
  `Config/intent/repos/`.
- `intent`, `renv`, and `pak` are exempt from source policy violations.
- Dependency overrides are checked against the same policy.
- Self-hosted repository users can declare their repository without any
  hard-coded CRAN, GitHub, or R-universe assumption.
- Documentation explains the relationship between `DESCRIPTION`,
  `Config/intent/repos/`, source policy, and `renv.lock`.

## Follow-Up: Cross-Platform Reproducibility CI

Repository policy should eventually be verified by CI, not just local tests.
Create a separate refactor for a GitHub Actions reproducibility workflow that
tests whether an `intent` project can be initialized, locked, restored, and
validated across platforms.

Candidate matrix:

- Windows latest
- macOS latest
- Ubuntu latest

Candidate workflow:

1. Install R and package system dependencies.
2. Install `intent` from the checked-out repository.
3. Run the sandbox end-to-end workflow or a CI-specific reproducibility script.
4. Archive the generated `DESCRIPTION` and `renv.lock` as artifacts on failure.
5. Verify that restore uses only repositories declared by `DESCRIPTION`.
6. Verify that source policy violations fail in strict mode.

This should be treated as a separate task because it touches CI cost, platform
support, cache strategy, and the exact definition of cross-platform equality.

## Result / Follow-Up Notes

- Result: Implemented source policy parsing from `DESCRIPTION`, lockfile
  provenance extraction, policy violation reporting in `status()` and JSON
  output, source preflight for requested packages and overrides, and
  candidate-lockfile validation before replacing the official `renv.lock`.
  Added `confirm_repos` to the R API and `--yes` / `--no-default-repo` to the
  CLI. Changed the default Posit Package Manager repository name to `RSPM` so
  default lockfile provenance matches `Config/intent/repos/RSPM`.
- Result: Added tests for policy defaults, repository-name mismatch, tool
  package exemptions, status JSON shape, strict-mode lockfile preservation, CLI
  init automation flags, and transitive-dependency-safe pruning interactions.
- Follow-up work: Add a cross-platform GitHub Actions reproducibility workflow
  once repository policy behavior is implemented.
