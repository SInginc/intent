# Run test coverage in a clean R session.
# Must be run OUTSIDE of renv — use:
#   Rscript --no-init-file tools/run_coverage.R
#
# This script does NOT activate renv. It installs the package and its
# test dependencies to a temporary library, then runs covr.

tmp_lib <- file.path(tempdir(), "covr_lib")
dir.create(tmp_lib, showWarnings = FALSE, recursive = TRUE)
.libPaths(c(tmp_lib, .libPaths()))

install.packages(
  c("covr", "withr", "mockery", "testthat", "jsonlite",
    "desc", "pak", "renv", "callr"),
  lib = tmp_lib,
  repos = "https://packagemanager.posit.co/cran/latest",
  quiet = TRUE
)

library(covr)

# Work around renv's .Rprofile: set env vars to prevent renv activation
# in covr's child R processes
Sys.setenv(RENV_ACTIVATE_PROJECT = "FALSE")

cv <- covr::package_coverage(
  type = "tests",
  quiet = FALSE
)

print(cv)

# Write coverage report
covr::report(cv, file = file.path("tools", "coverage-report.html"))
cat("\nCoverage report written to tools/coverage-report.html\n")
