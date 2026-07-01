#' Sync the Environment
#'
#' Ensures the `DESCRIPTION` file matches the `renv.lock` file exactly.
#' This is equivalent to `renv::restore(clean = TRUE)`.
#' @param project Path to the project directory. Defaults to the current intent project.
#' @param dry_run Logical. If `TRUE`, returns the planned actions without changing files or packages.
#'
#' @export
sync <- function(project = NULL, dry_run = FALSE) {
  cmd_sync(project = project, dry_run = dry_run)
}
