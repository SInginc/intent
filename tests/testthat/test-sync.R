test_that("intent::sync restores environment from lockfile", {
  # Setup
  tmp_dir <- file.path(
    Sys.getenv("R_USER_CACHE_DIR", unset = tempdir()),
    paste0("intent_test_sync_", Sys.getpid())
  )
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  old_dir <- getwd()
  on.exit(
    {
      setwd(old_dir)
      unlink(tmp_dir, recursive = TRUE)
    },
    add = TRUE,
    after = FALSE
  )

  # Initialize
  pkg_to_test <- "dplyr"
  suppressMessages(init(
    path = tmp_dir,
    repos = c(CRAN = "https://packagemanager.posit.co/cran/latest")
  ))

  # delete `intent` from dependencies as unavailable on CRAN
  desc::desc_del_dep(
    "intent",
    file = file.path(tmp_dir, "DESCRIPTION")
  )

  # add new dependency to DESCRIPTION
  desc::desc_set_dep(
    pkg_to_test,
    type = "Imports",
    file = file.path(tmp_dir, "DESCRIPTION")
  )

  lib_path <- callr::r(
    function(old_dir, tmp_dir) {
      if (!requireNamespace("intent", quietly = TRUE)) {
        pkgload::load_all(old_dir)
      }

      # Set repos in current session since .Rprofile is not loaded
      setwd(tmp_dir)
      Sys.setenv(RENV_CONFIG_PAK_ENABLED = TRUE)
      Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = TRUE)
      renv::load(project = tmp_dir, quiet = TRUE)
      options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

      intent::sync()

      # Check Library
      # renv library path
      renv::paths$library()
    },
    args = list(
      old_dir = old_dir,
      tmp_dir = tmp_dir
    )
  )

  # Check Project Library
  expect_true(dir.exists(file.path(lib_path, pkg_to_test)))

  # Check Lockfile
  lock <- renv::lockfile_read(file.path(tmp_dir, "renv.lock"))
  expect_true(pkg_to_test %in% names(lock$Packages))
})

test_that("cmd_sync prune removes extra lockfile packages", {
  tmp_dir <- tempfile()
  lib_dir <- tempfile()
  dir.create(tmp_dir)
  dir.create(lib_dir)
  dir.create(file.path(lib_dir, "glue"))
  dir.create(file.path(lib_dir, "rlang"))
  on.exit(unlink(c(tmp_dir, lib_dir), recursive = TRUE))

  writeLines(
    c(
      "Package: testpkg",
      "Imports:",
      "    glue"
    ),
    file.path(tmp_dir, "DESCRIPTION")
  )
  writeLines("{}", file.path(tmp_dir, "renv.lock"))

  mockery::stub(cmd_sync, "intent_locked_packages", function(project) {
    c("glue", "rlang")
  })
  mockery::stub(cmd_sync, "backend_read_lockfile", function(project) {
    list(
      Packages = list(
        glue = list(Version = "1.0.0"),
        rlang = list(Version = "2.0.0")
      )
    )
  })
  mockery::stub(cmd_sync, "intent_install", function(project, pkgs) NULL)
  mockery::stub(cmd_sync, "intent_snapshot", function(project) NULL)
  mockery::stub(cmd_sync, "intent_restore", function(project) NULL)

  removed <- NULL
  mockery::stub(cmd_sync, "backend_remove", function(project, pkgs) {
    removed <<- pkgs
  })

  cmd_sync(project = tmp_dir, prune = TRUE)

  expect_equal(removed, "rlang")
})

test_that("cmd_sync prune=FALSE preserves extra lockfile packages", {
  tmp_dir <- tempfile()
  lib_dir <- tempfile()
  dir.create(tmp_dir)
  dir.create(lib_dir)
  on.exit(unlink(c(tmp_dir, lib_dir), recursive = TRUE))

  writeLines(
    c(
      "Package: testpkg",
      "Imports:",
      "    glue"
    ),
    file.path(tmp_dir, "DESCRIPTION")
  )
  writeLines("{}", file.path(tmp_dir, "renv.lock"))

  mockery::stub(cmd_sync, "intent_locked_packages", function(project) {
    c("glue", "rlang")
  })
  mockery::stub(cmd_sync, "backend_read_lockfile", function(project) {
    list(
      Packages = list(
        glue = list(Version = "1.0.0"),
        rlang = list(Version = "2.0.0")
      )
    )
  })
  mockery::stub(cmd_sync, "intent_install", function(project, pkgs) NULL)
  mockery::stub(cmd_sync, "intent_snapshot", function(project) NULL)
  mockery::stub(cmd_sync, "intent_restore", function(project) NULL)

  mockery::stub(cmd_sync, "backend_remove", function(project, pkgs) {
    stop("remove should not be called")
  })

  expect_error(cmd_sync(project = tmp_dir, prune = FALSE), NA)
})

test_that("cmd_sync dry-run reports prune actions", {
  tmp_dir <- tempfile()
  lib_dir <- tempfile()
  dir.create(tmp_dir)
  dir.create(lib_dir)
  on.exit(unlink(c(tmp_dir, lib_dir), recursive = TRUE))

  writeLines(
    c(
      "Package: testpkg",
      "Imports:",
      "    glue"
    ),
    file.path(tmp_dir, "DESCRIPTION")
  )
  writeLines("{}", file.path(tmp_dir, "renv.lock"))

  mockery::stub(cmd_sync, "intent_locked_packages", function(project) {
    c("glue", "rlang")
  })
  mockery::stub(cmd_sync, "backend_read_lockfile", function(project) {
    list(
      Packages = list(
        glue = list(Version = "1.0.0"),
        rlang = list(Version = "2.0.0")
      )
    )
  })
  mockery::stub(cmd_sync, "intent_install", function(project, pkgs) {
    stop("install should not run")
  })
  mockery::stub(cmd_sync, "backend_remove", function(project, pkgs) {
    stop("remove should not run")
  })
  mockery::stub(cmd_sync, "intent_snapshot", function(project) {
    stop("snapshot should not run")
  })
  mockery::stub(cmd_sync, "intent_restore", function(project) {
    stop("restore should not run")
  })

  plan <- cmd_sync(project = tmp_dir, dry_run = TRUE, prune = TRUE)

  expect_s3_class(plan, "intent_plan")
  expect_true(any(grepl("would_prune: rlang", plan$actions)))
  expect_equal(plan$packages, "rlang")
})
