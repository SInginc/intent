# De-hardcode Package Constants

Status: `implemented`

## Context

Several hardcoded values in the codebase should either be dynamically
determined or user-configurable, rather than baked into the source.

## Decisions

### 1. Default repository URL — environment variable

**Current:** `c(RSPM = "https://packagemanager.posit.co/cran/latest")` hardcoded
in `R/commands.R:71` and `R/init.R:9` (roxygen).

**Decision:** Read from `INTENT_DEFAULT_REPOS` environment variable.
Format: `"NAME=URL,NAME=URL"`.

```
INTENT_DEFAULT_REPOS="CRAN=https://ppm/cran/latest"
# or:
INTENT_DEFAULT_REPOS=""   # force explicit declaration
```

```r
# R/utils.R (new function)
load_default_repos <- function() {
  raw <- Sys.getenv("INTENT_DEFAULT_REPOS", unset = NA_character_)
  if (is.na(raw)) {
    return(c(RSPM = "https://packagemanager.posit.co/cran/latest"))
  }
  if (!nzchar(trimws(raw))) {
    return(character())
  }
  parts <- strsplit(raw, "\\s*,\\s*")[[1]]
  pairs <- strsplit(parts, "=")
  repos <- vapply(pairs, function(p) p[[2]], character(1))
  names(repos) <- vapply(pairs, function(p) p[[1]], character(1))
  repos
}
```

**Files:**
- `R/utils.R` — new `load_default_repos()`
- `R/commands.R:71` — replace hardcoded default with `load_default_repos()`
- `R/init.R:9` — update roxygen docs

### 2. Base R package list — from R itself

**Current:** 15 hardcoded package names in `R/status-core.R:127-142`.

**Decision:** Query `rownames(installed.packages(priority = "base"))` at
package load time, cache in a package-level environment variable.

```r
# R/status-core.R
intent_base_packages <- local({
  pkgs <- NULL
  function() {
    if (is.null(pkgs)) {
      pkgs <- rownames(installed.packages(priority = "base"))
    }
    pkgs
  }
})
```

Usage site (`intent_lock_package_dependencies`, line 126) changes from:
```r
setdiff(unique(deps), c("R", "base", "compiler", ...))
```
to:
```r
setdiff(unique(deps), c("R", intent_base_packages()))
```

**Files:**
- `R/status-core.R:127-142` — replace hardcoded vector with dynamic lookup

### 3. Snapshot type — staying hardcoded

**Current:** `type = "explicit"` in `R/backend.R:70`.

**Decision:** `"explicit"` is a core invariant. Other snapshot types
(`implicit`, `all`, `simple`) conflict with intent's contract model:
zombie packages, missing transitive dependencies, and platform-dependent
behaviour. No configuration surface is exposed.

No code changes.

## Source class names

**Current:** `"repository"`, `"github"`, `"bioc"`, `"url"`, `"local"`,
`"unknown"` scattered across `R/status-core.R`, `R/desc.R`, `R/utils.R`.

**Decision:** Accepted as matching renv's lockfile schema. New source
classes from future renv versions fall through to `"unknown"` — no
semantic misinterpretation risk. Deferred.
