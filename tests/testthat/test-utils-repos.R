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
