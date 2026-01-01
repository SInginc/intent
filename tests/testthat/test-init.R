test_that("intent::init creates necessary files", {
  # Use a temp directory for the project
  tmp_dir <- file.path(
    Sys.getenv("R_USER_CACHE_DIR", unset = tempdir()),
    paste0("rv_test_init_", Sys.getpid())
  )
  on.exit(unlink(tmp_dir, recursive = TRUE))

  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive = TRUE)
  }

  # Mocking utils::check_missing_deps or ensuring environment has them
  # For this test, we assume the environment is set up (we are running in code editor agent)

  # Run init
  # We might need to mock or suppress messages
  suppressMessages(init(
    path = tmp_dir,
    repos = "https://packagemanager.posit.co/cran/latest"
  ))

  expect_true(dir.exists(tmp_dir))
  expect_true(file.exists(file.path(tmp_dir, "DESCRIPTION")))
  expect_true(file.exists(file.path(tmp_dir, "renv.lock")))
  expect_true(file.exists(file.path(tmp_dir, ".Rprofile")))
  expect_true(file.exists(file.path(tmp_dir, ".Renviron")))

  # Check content
  .desc <- desc::description$new(file.path(tmp_dir, "DESCRIPTION"))

  expect_true(.desc$has_dep("pak"))
  expect_true(.desc$has_dep("renv"))
  expect_true(.desc$has_dep("intent"))

  rprofile <- readLines(file.path(tmp_dir, ".Rprofile"))
  expect_true(any(grepl("options\\(repos", rprofile)))

  renviron <- readLines(file.path(tmp_dir, ".Renviron"))
  expect_true(any(grepl("RENV_CONFIG_PAK_ENABLED=TRUE", renviron)))

  # Check renv settings
  # verifying renv settings might require loading the project or checking renv/settings.json
  # But intent::init doesn't write settings.json directly, it calls renv::settings
  # which writes to renv/settings.dcf or json.
  expect_true(
    file.exists(file.path(tmp_dir, "renv/settings.json")) ||
      file.exists(file.path(tmp_dir, "renv/settings.dcf"))
  )
})
