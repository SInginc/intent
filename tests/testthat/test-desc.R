test_that("read_intent_config parses fields correctly", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  desc_path <- file.path(tmp_dir, "DESCRIPTION")

  # Create a dummy DESCRIPTION
  writeLines(
    c(
      "Package: testpkg",
      "Config/intent/repos/cran: https://cran.rstudio.com",
      "Config/intent/repos/bioc: https://bioconductor.org/packages/3.14/bioc",
      "Config/intent/other/setting: value",
      "Config/intent/flag: TRUE"
    ),
    desc_path
  )

  config <- read_intent_config(desc_path)

  expect_equal(config$repos$cran, "https://cran.rstudio.com")
  expect_equal(config$repos$bioc, "https://bioconductor.org/packages/3.14/bioc")
  expect_equal(config$other$setting, "value")
  expect_equal(config$flag, "TRUE")
})

test_that("read_intent_config handles mandatory and permissive fields", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  desc_path <- file.path(tmp_dir, "DESCRIPTION")

  # Create a dummy DESCRIPTION with mandatory, permissive, and extra fields
  writeLines(
    c(
      "Package: testpkg",
      "Config/intent/repos/cran: https://cran.rstudio.com",
      "Config/intent/optional/setting: value",
      "Config/intent/extra/field: ignore"
    ),
    desc_path
  )

  # Test mandatory enforcement
  expect_error(
    read_intent_config(desc_path, mandatory = "missing"),
    "Mandatory Config/intent fields missing: missing"
  )

  # Test permissive filtering - should keep mandatory and permissive, filter out extra
  config <- read_intent_config(
    desc_path,
    mandatory = "repos",
    permissive = "optional"
  )
  expect_named(config, c("repos", "optional"))
  expect_equal(config$repos$cran, "https://cran.rstudio.com")
  expect_equal(config$optional$setting, "value")
  expect_false("extra" %in% names(config))

  # Test optionality: missing permissive field should NOT error
  expect_silent(
    config_no_opt <- read_intent_config(
      desc_path,
      mandatory = "repos",
      permissive = "nonexistent"
    )
  )
  expect_named(config_no_opt, "repos")
})

test_that("get_repos works as expected", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  desc_path <- file.path(tmp_dir, "DESCRIPTION")

  writeLines(
    c(
      "Package: testpkg",
      "Config/intent/repos/cran: https://cran.rstudio.com"
    ),
    desc_path
  )

  repos <- get_repos(desc_path)
  expect_equal(repos[["cran"]], "https://cran.rstudio.com")
})

test_that("get_source_policy returns defaults and parses overrides", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  desc_path <- file.path(tmp_dir, "DESCRIPTION")
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines("Package: testpkg", desc_path)
  defaults <- get_source_policy(desc_path)
  expect_equal(defaults$mode, "warn")
  expect_true(defaults$allow$repository)
  expect_false(defaults$allow$unknown)
  expect_equal(defaults$exempt_packages, c("intent", "renv", "pak"))

  writeLines(
    c(
      "Package: testpkg",
      "Config/intent/source-policy/mode: error",
      "Config/intent/source-policy/allow/github: false",
      "Config/intent/source-policy/allow/local: true"
    ),
    desc_path
  )

  policy <- get_source_policy(desc_path)
  expect_equal(policy$mode, "error")
  expect_false(policy$allow$github)
  expect_true(policy$allow$local)
})
