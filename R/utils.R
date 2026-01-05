#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Check for Missing Dependencies
#'
#' Checks if `renv` are installed. If not, stops with an informative message.
#' @keywords internal
check_missing_deps <- function() {
  missing <- c()
  if (!requireNamespace("renv", quietly = TRUE)) {
    missing <- c(missing, "renv")
  }
  if (!requireNamespace("pak", quietly = TRUE)) {
    missing <- c(missing, "pak")
  }

  if (length(missing) > 0) {
    stop(
      "The following required packages are missing: ",
      paste(missing, collapse = ", "),
      ".\nPlease install them before using `intent`.",
      call. = FALSE
    )
  }
}

#' Load Repositories from DESCRIPTION
#'
#' Reads the repositories defined in `Config/intent/repos/` and sets them
#' in the global `options(repos)`.
#' @keywords internal
load_intent_repos <- function() {
  # Find DESCRIPTION. This might be tricky if not in project root,
  # but intent usually operates on the current project.
  path_to_desc <- file.path(renv::project(), "DESCRIPTION")
  if (file.exists(path_to_desc)) {
    repos <- get_repos(path_to_desc)
    options(repos = repos)
  }
}

#' @keywords internal
intent_install <- function(pkgs) {
  load_intent_repos()
  lib_loc <- renv::paths$library()
  message("Installing packages into ", lib_loc, "...")
  pak::pkg_install(pkgs, lib = lib_loc, ask = FALSE)
}

#' @keywords internal
intent_snapshot <- function() {
  load_intent_repos()
  renv::snapshot(dev = TRUE, prompt = FALSE)
}

#' @keywords internal
intent_restore <- function() {
  load_intent_repos()
  renv::restore(clean = TRUE, prompt = FALSE)
}

#' @keywords internal
intent_set_project_dep <- function(package, type, version = "*") {
  desc::desc_set_dep(
    package = package,
    type = type,
    version = version,
    file = file.path(renv::project(), "DESCRIPTION"),
    normalize = TRUE
  )
}

#' @keywords internal
intent_del_project_dep <- function(package) {
  desc::desc_del_dep(
    package = package,
    file = file.path(renv::project(), "DESCRIPTION"),
    normalize = TRUE
  )
}

#' @keywords internal
intent_get_project_deps <- function() {
  desc::desc_get_deps(
    file = file.path(renv::project(), "DESCRIPTION")
  )
}
