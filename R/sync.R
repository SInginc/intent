#' Sync the Environment
#'
#' Ensures the `DESCRIPTION` file matches the `renv.lock` file exactly.
#' This is equivalent to `renv::restore(clean = TRUE)`.
#'
#' @export
sync <- function() {
  check_missing_deps()

  if (!file.exists("DESCRIPTION")) {
    stop(
      "No DESCRIPTION file found. Run `intent::init()` or `intent::add()` first.",
      call. = FALSE
    )
  }

  message("Syncing environment...")

  # 1. Compare Intent (DESCRIPTION) vs State (Lockfile/Library)
  # We need to ensure that what is in DESCRIPTION is installed and in Lockfile.

  desc_deps <- rv_get_project_deps()
  # Filter for relevant types
  target_types <- c("Imports", "Suggests")
  intent_pkgs <- desc_deps$package[desc_deps$type %in% target_types]
  intent_pkgs <- intent_pkgs[intent_pkgs != "R"]

  # Check against lockfile
  if (file.exists("renv.lock")) {
    lock <- renv::lockfile_read("renv.lock")
    lock_pkgs <- names(lock$Packages)
    missing_pkgs <- setdiff(intent_pkgs, lock_pkgs)
  } else {
    missing_pkgs <- intent_pkgs
  }

  if (length(missing_pkgs) > 0) {
    # Use pak for fast install of missing deps
    message(
      "Installing missing packages from DESCRIPTION: ",
      paste(missing_pkgs, collapse = ", ")
    )
    rv_install(missing_pkgs)

    message("Updating lockfile...")
    rv_snapshot()
  }

  # 2. Sync Library matches Lockfile
  # This handles removing extras and ensuring versions match lockfile
  message("Restoring library from lockfile...")
  rv_restore()

  message("Environment synchronized.")
}
