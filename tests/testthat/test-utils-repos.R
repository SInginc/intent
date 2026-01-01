test_that("load_intent_repos sets options correctly", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  desc_path <- file.path(tmp_dir, "DESCRIPTION")

  writeLines(
    c(
      "Package: testpkg",
      "Config/intent/repos/TEST: https://test.repo"
    ),
    desc_path
  )

  # Mock renv::project() to return tmp_dir
  # We might need to set up a real renv project or mock the function
  # For now, let's assume we can mock or the function handles lack of project gracefully

  withr::with_options(list(repos = NULL), {
    # Since load_intent_repos uses renv::project(), we need to initialize it
    # or ensure we are in a project context.
    # A better way is to pass the path or mock renv::project().

    # For testing, let's temporarily mock renv::project
    if (!requireNamespace("mockery", quietly = TRUE)) {
      # Fallback: manual mock if mockery not available
      old_proj <- renv::project
      unlockBinding("project", asNamespace("renv"))
      assignInNamespace("project", function(...) tmp_dir, ns = "renv")
      on.exit({
        assignInNamespace("project", old_proj, ns = "renv")
        lockBinding("project", asNamespace("renv"))
      })
    } else {
      m <- mockery::mock(tmp_dir)
      mockery::stub(load_intent_repos, "renv::project", m)
    }

    load_intent_repos()
    expect_equal(getOption("repos")[["TEST"]], "https://test.repo")
  })
})
