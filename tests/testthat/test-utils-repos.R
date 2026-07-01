test_that("load_intent_repos returns repos without setting options", {
  tmp_dir <- file.path(
    Sys.getenv("R_USER_CACHE_DIR", unset = tempdir()),
    paste0("intent_test_utils_repos_", Sys.getpid())
  )
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  desc_path <- file.path(tmp_dir, "DESCRIPTION")
  writeLines(
    c(
      "Package: testpkg",
      "Config/intent/repos/TEST: https://test.repo"
    ),
    desc_path
  )

  before <- getOption("repos")
  repos <- load_intent_repos(tmp_dir)
  after <- getOption("repos")

  expect_equal(repos[["TEST"]], "https://test.repo")
  expect_equal(before, after)
})

test_that("extract_pkg_name strips user/repo and @version", {
  expect_equal(extract_pkg_name("dplyr"), "dplyr")
  expect_equal(extract_pkg_name("user/dplyr"), "dplyr")
  expect_equal(extract_pkg_name("dplyr@1.0.0"), "dplyr")
  expect_equal(extract_pkg_name("user/dplyr@0.1.0"), "dplyr")
  expect_equal(extract_pkg_name("tidyverse/dplyr@1.1.4"), "dplyr")
})

test_that("intent_get_project_deps reads DESCRIPTION dependencies", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(
    c(
      "Package: testpkg",
      "Imports:",
      "    dplyr,",
      "    glue",
      "Suggests:",
      "    testthat"
    ),
    file.path(tmp_dir, "DESCRIPTION")
  )

  deps <- intent_get_project_deps(tmp_dir)
  expect_true("dplyr" %in% deps$package)
  expect_true("glue" %in% deps$package)
  expect_true("testthat" %in% deps$package)
})

test_that("intent_sync_project calls backend snapshot and restore", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(
    c(
      "Package: testpkg",
      "Config/intent/repos/CRAN: https://example.com"
    ),
    file.path(tmp_dir, "DESCRIPTION")
  )

  snap_called <- NULL
  rest_called <- NULL

  mockery::stub(
    intent_sync_project,
    "backend_snapshot",
    function(project, repos) {
      snap_called <<- list(project = project, repos = repos)
    }
  )
  mockery::stub(
    intent_sync_project,
    "backend_restore",
    function(project, repos) {
      rest_called <<- list(project = project, repos = repos)
    }
  )

  intent_sync_project(tmp_dir)

  expect_equal(snap_called$project, tmp_dir)
  expect_equal(rest_called$project, tmp_dir)
  expect_equal(snap_called$repos[["CRAN"]], "https://example.com")
  expect_equal(rest_called$repos[["CRAN"]], "https://example.com")
})
