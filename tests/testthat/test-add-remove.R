test_that("intent::add and intent::remove work as expected", {
  # Setup
  tmp_dir <- file.path(
    Sys.getenv("R_USER_CACHE_DIR", unset = tempdir()),
    paste0("rv_test_add_", Sys.getpid())
  )
  dir.create(tmp_dir)

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
  pkg_to_test <- "desc"
  suppressMessages(init(
    path = tmp_dir,
    repos = "https://packagemanager.posit.co/cran/latest"
  ))

  lib_path <- callr::r(
    function(old_dir, tmp_dir, pkg_to_test) {
      if (!requireNamespace("intent", quietly = TRUE)) {
        pkgload::load_all(old_dir)
      }

      # Set repos in current session since .Rprofile is not loaded
      setwd(tmp_dir)
      Sys.setenv(RENV_CONFIG_PAK_ENABLED = TRUE)
      Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = TRUE)
      renv::load(project = tmp_dir, quiet = TRUE)
      options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

      # Check Library
      # renv library path
      intent::add(pkg_to_test)
      renv::paths$library()
    },
    args = list(
      old_dir = old_dir,
      tmp_dir = tmp_dir,
      pkg_to_test = pkg_to_test
    )
  )
  .desc <- desc::description$new(file.path(tmp_dir, "DESCRIPTION"))

  # Check DESCRIPTION
  expect_true(.desc$has_dep(pkg_to_test))
  expect_true(dir.exists(file.path(lib_path, pkg_to_test)))

  # Check Lockfile
  lock <- renv::lockfile_read(file.path(tmp_dir, "renv.lock"))
  expect_true(pkg_to_test %in% names(lock$Packages))

  # Test REMOVE
  lib_path <- callr::r(
    function(old_dir, tmp_dir, pkg_to_test) {
      if (!requireNamespace("intent", quietly = TRUE)) {
        pkgload::load_all(old_dir)
      }

      setwd(tmp_dir)
      Sys.setenv(RENV_CONFIG_PAK_ENABLED = TRUE)
      Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = TRUE)
      renv::load(project = tmp_dir, quiet = TRUE)
      options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))
      intent::remove(pkg_to_test)
      renv::paths$library()
    },
    args = list(
      old_dir = old_dir,
      tmp_dir = tmp_dir,
      pkg_to_test = pkg_to_test
    )
  )

  # Check DESCRIPTION
  .desc <- desc::description$new(file.path(tmp_dir, "DESCRIPTION"))
  expect_false(.desc$has_dep(pkg_to_test))

  # Check Library
  expect_false(dir.exists(file.path(lib_path, pkg_to_test)))

  # Check Lockfile
  lock <- renv::lockfile_read(file.path(tmp_dir, "renv.lock"))
  expect_false(pkg_to_test %in% names(lock$Packages))

  # Check Zombie (R6 should be removed as it is a specific dependency of desc)
  # desc depends on R6. If desc is removed, R6 should be gone.
  expect_false("R6" %in% names(lock$Packages))
})
