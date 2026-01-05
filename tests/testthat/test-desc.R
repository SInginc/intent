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
