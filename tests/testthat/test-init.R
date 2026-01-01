test_that("intent::init creates necessary files", {
  # Use a temp directory for the project
  tmp_dir <- file.path(
    Sys.getenv("R_USER_CACHE_DIR", unset = tempdir()),
    paste0("intent_test_init_", Sys.getpid())
  )
  on.exit(unlink(tmp_dir, recursive = TRUE))

  if (dir.exists(tmp_dir)) {
    unlink(tmp_dir, recursive = TRUE)
  }

  # Mocking utils::check_missing_deps or ensuring environment has them
  # For this test, we assume the environment is set up (we are running in code editor agent)

  # Run init
  # We might need to mock or suppress messages
  init(
    path = tmp_dir,
    repos = c(
      CRAN = "https://packagemanager.posit.co/cran/__linux__/manylinux_2_28/latest"
    )
  )

  expect_true(dir.exists(tmp_dir))
  expect_true(file.exists(file.path(tmp_dir, "DESCRIPTION")))
  expect_true(file.exists(file.path(tmp_dir, "renv.lock")))
  expect_true(file.exists(file.path(tmp_dir, ".Rprofile")))
  expect_true(file.exists(file.path(tmp_dir, ".Renviron")))

  # Check content
  ## DESCRIPTION
  rproject <- desc::description$new(file.path(tmp_dir, "DESCRIPTION"))
  expect_true(rproject$has_dep("pak"))
  expect_true(rproject$has_dep("renv"))
  expect_true(rproject$has_dep("intent"))
  ### Check repos in DESCRIPTION
  expect_equal(
    rproject$get_field("Config/intent/repos/CRAN"),
    "https://packagemanager.posit.co/cran/__linux__/manylinux_2_28/latest"
  )

  ## renv.lock
  renv_lock <- renv::lockfile_read(
    file = file.path(tmp_dir, "renv.lock"),
    project = tmp_dir
  )
  ### check repos
  renv_repos <- renv_lock$R$Repositories
  expect_equal(names(renv_repos), c("CRAN"))
  expect_equal(
    renv_repos[[1]],
    "https://packagemanager.posit.co/cran/__linux__/manylinux_2_28/latest"
  )
  expect_true("pak" %in% names(renv_lock$Packages))
  expect_true("renv" %in% names(renv_lock$Packages))

  # generated/modified by `renv`
  rprofile <- readLines(file.path(tmp_dir, ".Rprofile"))

  # generated/modified by `intent`
  renviron <- readLines(file.path(tmp_dir, ".Renviron"))
  expect_true(any(grepl("RENV_CONFIG_PAK_ENABLED = TRUE", renviron)))

  # Check renv settings
  # verifying renv settings might require loading the project or checking renv/settings.json
  # But intent::init doesn't write settings.json directly, it calls renv::settings
  # which writes to renv/settings.dcf or json.
  expect_true(
    file.exists(file.path(tmp_dir, "renv/settings.json"))
  )
})
