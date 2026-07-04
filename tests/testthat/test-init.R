test_that("init defaults to PPM when no repos provided", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  mockery::stub(cmd_init, "backend_init", function(project, repos) {
    expect_equal(repos[["CRAN"]], "https://packagemanager.posit.co/cran/latest")
  })

  cmd_init(path = tmp_dir, repos = NULL, install_self = "never")

  rproject <- desc::description$new(file.path(tmp_dir, "DESCRIPTION"))
  expect_equal(
    rproject$get_field("Config/intent/repos/CRAN"),
    "https://packagemanager.posit.co/cran/latest"
  )
})

test_that("init hydrates intent by default", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  hydrated <- FALSE

  mockery::stub(cmd_init, "backend_init", function(project, repos) {
    dir.create(file.path(project, "renv"), recursive = TRUE)
  })
  mockery::stub(
    cmd_init,
    "maybe_hydrate_intent",
    function(project, sources, install_self) {
      hydrated <<- identical(install_self, "hydrate")
    }
  )

  cmd_init(path = tmp_dir, repos = NULL)

  expect_true(hydrated)
})

test_that("init can leave intent as an external tool", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  install_self_seen <- NULL

  mockery::stub(cmd_init, "backend_init", function(project, repos) {
    dir.create(file.path(project, "renv"), recursive = TRUE)
  })
  mockery::stub(
    cmd_init,
    "maybe_hydrate_intent",
    function(project, sources, install_self) {
      install_self_seen <<- install_self
    }
  )

  cmd_init(path = tmp_dir, repos = NULL, install_self = "never")

  expect_equal(install_self_seen, "never")
})

test_that("intent hydration failure does not fail init", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  mockery::stub(cmd_init, "backend_init", function(project, repos) {
    dir.create(file.path(project, "renv"), recursive = TRUE)
  })
  mockery::stub(
    maybe_hydrate_intent,
    "backend_hydrate",
    function(project, pkgs, sources) {
      stop("not available")
    }
  )
  mockery::stub(maybe_hydrate_intent, "backend_library", function(project) {
    file.path(project, "library")
  })

  expect_message(
    cmd_init(path = tmp_dir, repos = NULL),
    "was not installed into the project library"
  )
})

test_that("intent hydration force-snapshots local tool installs", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  force_seen <- NULL
  lib <- file.path(tmp_dir, "library")
  dir.create(file.path(lib, "intent"), recursive = TRUE)

  mockery::stub(
    maybe_hydrate_intent,
    "backend_hydrate",
    function(project, pkgs, sources) {
      list(intent = "hydrated")
    }
  )
  mockery::stub(maybe_hydrate_intent, "backend_library", function(project) {
    lib
  })
  mockery::stub(
    maybe_hydrate_intent,
    "intent_snapshot",
    function(project, force) {
      force_seen <<- force
    }
  )

  maybe_hydrate_intent(tmp_dir, sources = character(), install_self = "hydrate")

  expect_true(force_seen)
})

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

  # Mocking utils::check_renv_loaded or ensuring environment has them
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
