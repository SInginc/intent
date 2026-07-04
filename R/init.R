#' Initialize an intent Project
#'
#' Sets up a directory as an `intent` project by creating a `DESCRIPTION` file (if missing),
#' initializing a bare `renv` environment, setting the snapshot type to "explicit",
#' and configuring `.Rprofile` and `.Renviron`.
#'
#' @param path Character string. Path to the new project directory. Defaults to current working directory.
#' @param repos Character vector. Named repositories to use. Defaults to
#'   [Posit Package Manager](https://packagemanager.posit.co/cran/latest)
#'   when neither this argument nor an existing DESCRIPTION file provides
#'   repository configuration.
#' @param install_self Character string. `"hydrate"` attempts to copy the
#'   currently installed `intent` package from the active library paths into the
#'   new project library. `"never"` leaves `intent` as an external tool only.
#' @param confirm_repos Logical. If `TRUE`, ask before writing the default
#'   repository when no repository is configured. Defaults to `interactive()`.
#'
#' @export
init <- function(
  path = ".",
  repos = NULL,
  install_self = "hydrate",
  confirm_repos = interactive()
) {
  cmd_init(
    path = path,
    repos = repos,
    install_self = install_self,
    confirm_repos = confirm_repos
  )
}
