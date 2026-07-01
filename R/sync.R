#' Sync the Environment
#'
#' Ensures the `DESCRIPTION` file matches the `renv.lock` file exactly.
#' This is equivalent to `renv::restore(clean = TRUE)`.
#' @param project Path to the project directory. Defaults to the current intent project.
#'
#' @export
sync <- function(project = NULL) {
  project <- resolve_project(project)

  desc_path <- file.path(project, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    stop(
      "No DESCRIPTION file found. Run `intent::init()` or `intent::add()` first.",
      call. = FALSE
    )
  }

  message("Syncing environment...")

  # 1. Compare Intent (DESCRIPTION) vs State (Lockfile/Library)
  # We need to ensure that what is in DESCRIPTION is installed and in Lockfile.

  desc_deps <- desc::desc_get_deps(file = desc_path)
  # Filter for relevant types
  target_types <- c("Imports", "Suggests")
  intent_pkgs <- desc_deps$package[desc_deps$type %in% target_types]
  intent_pkgs <- intent_pkgs[intent_pkgs != "R"]

  # Check against lockfile
  overrides_info <- get_intent_overrides(desc_path)

  lock_path <- file.path(project, "renv.lock")
  if (file.exists(lock_path)) {
    lock <- renv::lockfile_read(lock_path)
    lock_pkgs <- names(lock$Packages)
    missing_pkgs <- setdiff(intent_pkgs, lock_pkgs)

    # Also check if any overridden packages need updating/syncing
    overridden_intent_pkgs <- intersect(
      intent_pkgs,
      names(overrides_info$overrides)
    )
    for (pkg in overridden_intent_pkgs) {
      if (pkg %in% lock_pkgs) {
        lock_ver <- lock$Packages[[pkg]]$Version
        parsed <- overrides_info$overrides[[pkg]]
        # If version doesn't match, add to missing_pkgs
        if (lock_ver != parsed$version) {
          missing_pkgs <- c(missing_pkgs, pkg)
        }
      }
    }
    missing_pkgs <- unique(missing_pkgs)
  } else {
    missing_pkgs <- intent_pkgs
  }

  # Filter out intent from sync/install
  missing_pkgs <- missing_pkgs[missing_pkgs != "intent"]

  if (length(missing_pkgs) > 0) {
    # Use pak for fast install of missing deps
    message(
      "Installing/Updating packages from DESCRIPTION: ",
      paste(missing_pkgs, collapse = ", ")
    )
    intent_install(project, missing_pkgs)

    message("Updating lockfile...")
    intent_snapshot(project)
  }

  # 2. Sync Library matches Lockfile
  # This handles removing extras and ensuring versions match lockfile
  message("Restoring library from lockfile...")
  intent_restore(project)

  message("Environment synchronized.")
}
