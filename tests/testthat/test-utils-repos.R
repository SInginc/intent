test_that("load_intent_repos sets options correctly", {
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

  withr::with_options(list(repos = NULL), {
    load_intent_repos(tmp_dir)
    expect_equal(getOption("repos")[["TEST"]], "https://test.repo")
  })
})

test_that("extract_pkg_name strips user/repo and @version", {
  expect_equal(extract_pkg_name("dplyr"), "dplyr")
  expect_equal(extract_pkg_name("user/dplyr"), "dplyr")
  expect_equal(extract_pkg_name("dplyr@1.0.0"), "dplyr")
  expect_equal(extract_pkg_name("user/dplyr@0.1.0"), "dplyr")
  expect_equal(extract_pkg_name("tidyverse/dplyr@1.1.4"), "dplyr")
})
