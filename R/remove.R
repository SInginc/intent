#' Remove a Dependency
#'
#' Removes a package from the `DESCRIPTION` file, uninstalls it from the library,
#' and updates the `renv.lock` file.
#'
#' @param pkgs Character vector. Names of packages to remove.
#' @param project Path to the project directory. Defaults to the current intent project.
#'
#' @export
remove <- function(pkgs, project = NULL) {
  if (missing(pkgs) || length(pkgs) == 0) {
    stop("No packages specified.", call. = FALSE)
  }

  project <- resolve_project(project)

  # 1. Manifest Update: Remove from DESCRIPTION
  message("Removing ", paste(pkgs, collapse = ", "), " from DESCRIPTION")
  for (pkg in pkgs) {
    intent_del_project_dep(project, package = pkg)
  }

  # 2. Cleanup: Remove from library
  message("Removing packages from library...")
  backend_remove(project, pkgs)

  # 3. Locking: renv snapshot
  # Because we are in explicit mode and removed it from DESCRIPTION,
  # snapshot should remove it from lockfile.
  message("Updating lockfile...")
  intent_snapshot(project)

  # 4. Pruning: Remove orphans
  # Sync library with the new lockfile, removing any dependencies that are no longer needed
  message("Pruning orphan dependencies...")
  intent_restore(project)

  invisible(pkgs)
}
