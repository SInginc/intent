#' Remove a Dependency
#'
#' Removes a package from the `DESCRIPTION` file, uninstalls it from the library,
#' and updates the `renv.lock` file.
#'
#' @param pkgs Character vector. Names of packages to remove.
#' @param project Path to the project directory. Defaults to the current intent project.
#' @param dry_run Logical. If `TRUE`, returns the planned actions without changing files or packages.
#'
#' @export
remove <- function(pkgs, project = NULL, dry_run = FALSE) {
  cmd_remove(pkgs = pkgs, project = project, dry_run = dry_run)
}
