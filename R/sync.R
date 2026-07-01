#' Sync the Environment
#'
#' Ensures the `DESCRIPTION` file matches the `renv.lock` file exactly.
#' This is equivalent to `renv::restore(clean = TRUE)`.
#' @param project Path to the project directory. Defaults to the current intent project.
#'
#' @export
sync <- function(project = NULL) {
  cmd_sync(project = project)
}
