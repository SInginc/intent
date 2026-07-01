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
  cmd_remove(pkgs = pkgs, project = project)
}
