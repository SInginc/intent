#' Add a Dependency
#'
#' Adds a package to the `DESCRIPTION` file, installs it into the local library using `pak`,
#' and updates the `renv.lock` file.
#'
#' @param pkgs Character vector. Names of packages to add. Supports CRAN names or "user/repo" for GitHub.
#' @param dev Logical. If `TRUE`, adds packages to "Suggests". If `FALSE` (default), adds to "Imports".
#' @param project Path to the project directory. Defaults to the current intent project.
#' @param dry_run Logical. If `TRUE`, returns the planned actions without changing files or packages.
#'
#' @export
add <- function(pkgs, dev = FALSE, project = NULL, dry_run = FALSE) {
  cmd_add(pkgs = pkgs, dev = dev, project = project, dry_run = dry_run)
}
