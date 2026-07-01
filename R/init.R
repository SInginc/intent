#' Initialize an intent Project
#'
#' Sets up a directory as an `intent` project by creating a `DESCRIPTION` file (if missing),
#' initializing a bare `renv` environment, setting the snapshot type to "explicit",
#' and configuring `.Rprofile` and `.Renviron`.
#'
#' @param path Character string. Path to the new project directory. Defaults to current working directory.
#' @param repos Character vector. Repositories to use. Defaults to `getOption("repos")`.
#'
#' @export
init <- function(
  path = ".",
  repos = NULL
) {
  cmd_init(path = path, repos = repos)
}
