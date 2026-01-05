#' Remove a Dependency
#'
#' Removes a package from the `DESCRIPTION` file, uninstalls it from the library,
#' and updates the `renv.lock` file.
#'
#' @param pkgs Character vector. Names of packages to remove.
#'
#' @export
remove <- function(pkgs) {
  check_missing_deps()

  if (missing(pkgs) || length(pkgs) == 0) {
    stop("No packages specified.", call. = FALSE)
  }

  if (!file.exists("DESCRIPTION")) {
    stop(
      "No DESCRIPTION file found. Run `intent::init()` first.",
      call. = FALSE
    )
  }

  # 1. Manifest Update: Remove from DESCRIPTION
  message("Removing ", paste(pkgs, collapse = ", "), " from DESCRIPTION")
  for (pkg in pkgs) {
    intent_del_project_dep(package = pkg)
  }

  # 2. Cleanup: Remove from library
  # renv::remove handles uninstalling
  message("Removing packages from library...")
  renv::remove(pkgs)

  # 3. Locking: renv snapshot
  # Because we are in explicit mode and removed it from DESCRIPTION,
  # snapshot should remove it from lockfile.
  message("Updating lockfile...")
  intent_snapshot()

  # 4. Pruning: Remove orphans
  # Sync library with the new lockfile, removing any dependencies that are no longer needed
  message("Pruning orphan dependencies...")
  intent_restore()

  invisible(pkgs)
}
