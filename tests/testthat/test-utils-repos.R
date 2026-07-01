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
