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

#' @keywords internal
rv_install <- function(pkgs) {
  lib_loc <- renv::paths$library()
  message("Installing packages into ", lib_loc, "...")
  renv::install(pkgs, library = lib_loc, lock = TRUE)
}

#' @keywords internal
rv_snapshot <- function() {
  renv::snapshot(dev = TRUE, prompt = FALSE)
}

#' @keywords internal
rv_restore <- function() {
  renv::restore(clean = TRUE, prompt = FALSE)
}

#' @keywords internal
rv_set_project_dep <- function(package, type, version = "*") {
  desc::desc_set_dep(
    package = package,
    type = type,
    version = version,
    file = file.path(renv::project(), "DESCRIPTION"),
    normalize = TRUE
  )
}

#' @keywords internal
rv_del_project_dep <- function(package) {
  desc::desc_del_dep(
    package = package,
    file = file.path(renv::project(), "DESCRIPTION"),
    normalize = TRUE
  )
}

#' @keywords internal
rv_get_project_deps <- function() {
  desc::desc_get_deps(
    file = file.path(renv::project(), "DESCRIPTION")
  )
}
