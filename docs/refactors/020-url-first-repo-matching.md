# URL-First Repository Provenance Matching

Status: `planned`

## Context

`intent::init(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))` produces source policy violations. PPM's `PACKAGES` file reports `Repository: RSPM`, which renv writes into each package's lockfile record. The user declared `CRAN` but the packages say `RSPM` — a strict name mismatch that intent flags as a violation, even though both point to the same URL.

A previous fix (`normalize_declared_repos`, unstaged) rewrites `CRAN` → `RSPM` at init time. This silently overrides the user's naming choice.

## Principle

```
URL is the canonical identifier. Name is the user's label.
```

The fix operates at three levels, not by rewriting user input:

1. **Source policy** — fallback to URL matching when names differ
2. **Lockfile normalization** — supplement `$R$Repositories` with missing repository names at snapshot time
3. **Old renv compatibility** — handle `Repository` fields that are URLs instead of names

## Scenarios

### Why names differ

PPM's `PACKAGES` file reports `Repository: RSPM` for every package. renv writes this field verbatim into lockfile package records. The user's `Config/intent/repos/<NAME>` is independent — renv never reconciles the two. Result:

```
DESCRIPTION                      renv.lock
────────────                     ─────────────────────────────────────
Config/intent/repos/CRAN:        $R$Repositories: c(CRAN = "ppm_url")  ← user's name, preserved
  https://ppm/cran/latest        $Packages$dplyr$Repository: RSPM       ← PPM server's name
```

### Matching protocol

| Condition | Verdict | Example |
|-----------|---------|---------|
| Name match + URL match | **OK** | `CRAN` → `CRAN` same URL |
| Name mismatch + URL match | **OK** (fallback) | `RSPM` in package, `CRAN` declared, both → `ppm/cran/latest` |
| Name mismatch + URL mismatch | **VIOLATION** | `RSPM` in package, not declared, no URL match |
| Name match + URL mismatch | **VIOLATION** | `CRAN` declared → PPM URL, but package's `CRAN` → different URL |

### Scenario walkthrough

**A. Standard — same name, same URL:** User declares `RSPM`, PPM reports `RSPM`. Direct match. Always OK.

**B. Different name, same URL (user intent preserved):** User declares `CRAN` pointing to PPM. Package reports `RSPM` from same PPM URL. Name doesn't match but URL does → OK. User keeps their `CRAN` name.

**C. True violation — unknown source:** User declares `CRAN` → `cran.r-project.org`. Package reports `RSPM` from PPM. No name match, no URL match → violation. Correctly flagged.

**D. Same name, different URL (name collision):** User declares `CRAN` → PPM URL. Package from real CRAN also says `Repository: CRAN`. Name matches but URL differs → violation. Intent correctly detects the mismatch.

**E. Custom repository:** User declares `MYHUB` → `https://my-org.example.com/r`. Package from that server reports `Repository: MYHUB`. Direct match. Always OK.

**F. Multiple repos pointing to same PPM:** User declares both `CRAN` and `RSPM` pointing to same or similar PPM URLs. Not a violation (URL match covers it). Verify may emit info-level notice.

### Reserved names

No reserved repo names are needed. URL is the canonical identifier; name is the user's label. `RSPM` is not semantically special — it's listed in `INTENT_KNOWN_PPM_NAMES` only so intent can recognize it as a PPM-produced name and resolve it against declared PPM URLs.

## Design

### 1. Rollback `normalize_declared_repos`

Remove the function and its call in `cmd_init()`. User repo names pass through untouched.

### 2. URL-first matching in `intent_check_repository_policy()`

Four cases for the `Repository` field:

| Case | Value | Match strategy |
|------|-------|---------------|
| URL (`https://...`) | Old renv | Normalize → compare against declared URLs |
| Name in `names(repos)` | Direct hit | Name match + URL verification |
| Name in `known_ppm_names` & PPM URL declared | Fallback | URL comparison succeeds |
| Unknown name, not PPM | True violation | Error |

### 3. Supplement `$R$Repositories` in `intent_snapshot()`

After `lock$R$Repositories <- repos`, scan package `Repository` fields. For known PPM names (e.g. `RSPM`) not already present, append them with the corresponding PPM URL from declared repos.

```
$R$Repositories before:  c(CRAN = "https://ppm/cran/latest")
$R$Repositories after:   c(CRAN = "https://ppm/cran/latest",
                            RSPM = "https://ppm/cran/latest")
```

This ensures `renv::restore()` can resolve `RSPM` to its URL.

### 4. URL-aware `intent_verify_repository_issues()`

"missing" and "extra" checks: names that differ but share the same URL are not violations.

## Files

| File | Change |
|------|--------|
| `R/commands.R` | Remove `normalize_declared_repos()` and its call |
| `R/status-core.R` | Add `is_ppm_url()`, `intent_supplement_repositories()`; rewrite `intent_check_repository_policy()`; modify `intent_verify_repository_issues()` |
| `R/utils.R` | Add supplementation call in `intent_snapshot()` |
| `R/init.R` | Revert roxygen docs |
| `README.md` | Revert + update matching semantics |
| `docs/03-cli-design.md` | Revert + update |
| `tests/testthat/test-init.R` | Remove 2 tests; restore CRAN assertions |
| `tests/testthat/test-status-dry-run.R` | Modify existing; add new URL-matching tests |

## Verification

```r
intent::init("test-proj", repos = c(CRAN = "https://ppm/cran/latest"))
intent::add("dplyr", project = "test-proj")
intent::status(project = "test-proj")    # source_violations = 0
intent::verify(project = "test-proj")    # ok = TRUE
```
