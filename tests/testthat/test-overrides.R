library(testthat)

test_that("parse_override works for different formats", {
  # Standard CRAN
  p1 <- parse_override("glue@1.6.2@cran")
  expect_equal(p1$package, "glue")
  expect_equal(p1$version, "1.6.2")
  expect_equal(p1$ref, "glue@1.6.2")
  expect_null(p1$repo)

  # URL repository
  p2 <- parse_override("glue@1.6.2@https://cran.rstudio.com")
  expect_equal(p2$package, "glue")
  expect_equal(p2$version, "1.6.2")
  expect_equal(p2$ref, "glue@1.6.2")
  expect_equal(p2$repo, "https://cran.rstudio.com")

  # GitHub
  p3 <- parse_override("tidyverse/glue@1.6.2@github")
  expect_equal(p3$package, "glue")
  expect_equal(p3$version, "1.6.2")
  expect_equal(p3$ref, "github::tidyverse/glue@1.6.2")
  expect_null(p3$repo)

  # Unknown source treated as pak type
  p4 <- parse_override("my/pkg@1.0.0@local")
  expect_equal(p4$ref, "local::my/pkg@1.0.0")
})

test_that("get_intent_overrides extracts from DESCRIPTION", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  desc_path <- file.path(tmp_dir, "DESCRIPTION")

  writeLines(
    c(
      "Package: testpkg",
      "Config/intent/Imports/glue: glue@1.6.0@cran",
      "Config/intent/Suggests/rlang: rlang@1.0.0@https://example.com/repo"
    ),
    desc_path
  )

  overrides_info <- get_intent_overrides(desc_path)

  expect_named(overrides_info$overrides, c("glue", "rlang"))
  expect_equal(overrides_info$overrides$glue$version, "1.6.0")
  expect_equal(overrides_info$overrides$rlang$repo, "https://example.com/repo")
  expect_equal(
    overrides_info$extra_repos[["override_rlang"]],
    "https://example.com/repo"
  )
})

test_that("intent_install applies overrides", {
  # Mocking backend install and repository loading
  installed_pkgs <- NULL
  loaded_repos <- NULL

  # Use mockery to stub everything inside intent_install
  mockery::stub(intent_install, "get_intent_overrides", function(...) {
    list(
      overrides = list(
        glue = list(
          package = "glue",
          version = "1.6.0",
          ref = "glue@1.6.0",
          repo = NULL
        )
      ),
      extra_repos = character()
    )
  })
  mockery::stub(
    intent_install,
    "load_intent_repos",
    function(project, more_repos) {
      loaded_repos <<- more_repos
    }
  )
  mockery::stub(intent_install, "backend_install", function(project, pkgs) {
    installed_pkgs <<- pkgs
  })
  mockery::stub(intent_install, "file.exists", function(...) TRUE)

  intent_install(".", "glue")
  expect_equal(unname(installed_pkgs), "glue@1.6.0")
  expect_equal(loaded_repos, character())
})
