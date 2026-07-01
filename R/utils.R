#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @keywords internal
normalize_project_path <- function(project) {
  normalizePath(path.expand(project), winslash = "/", mustWork = FALSE)
}

#' @keywords internal
find_project_from <- function(path) {
  current <- normalize_project_path(path)
  repeat {
    if (file.exists(file.path(current, "DESCRIPTION"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      return(NULL)
    }
    current <- parent
  }
}

#' @keywords internal
resolve_project <- function(project = NULL) {
  if (!is.null(project)) {
    project <- normalize_project_path(project)
    if (!dir.exists(project)) {
      stop("Project path does not exist: ", project, call. = FALSE)
    }
    if (!file.exists(file.path(project, "DESCRIPTION"))) {
      stop(
        "No DESCRIPTION file found in project: ",
        project,
        ". Run `intent::init()` first.",
        call. = FALSE
      )
    }
    return(project)
  }

  project <- find_project_from(getwd())
  if (!is.null(project)) {
    return(project)
  }

  active_project <- tryCatch(
    renv::project(),
    error = function(e) NULL
  )
  if (!is.null(active_project)) {
    active_project <- normalize_project_path(active_project)
    if (file.exists(file.path(active_project, "DESCRIPTION"))) {
      return(active_project)
    }
  }

  stop(
    "No intent project found. Run `intent::init()` or pass `project =`.",
    call. = FALSE
  )
}

#' Load Repositories from DESCRIPTION
#'
#' Reads the repositories defined in `Config/intent/repos/` and sets them
#' in the global `options(repos)`.
#' @param project Path to the project directory.
#' @param more_repos Optional character vector of additional repositories.
#' @keywords internal
load_intent_repos <- function(project, more_repos = NULL) {
  path_to_desc <- file.path(project, "DESCRIPTION")
  repos <- character()
  if (file.exists(path_to_desc)) {
    repos <- get_repos(path_to_desc)
  }

  if (length(more_repos) > 0) {
    # Combine with existing repos, prioritizing more_repos
    repos <- c(more_repos, repos)
  }

  if (length(repos) > 0) {
    options(repos = repos)
  }
}

#' @keywords internal
intent_install <- function(project, pkgs) {
  path_to_desc <- file.path(project, "DESCRIPTION")
  overrides_info <- list(overrides = character(), extra_repos = character())
  if (file.exists(path_to_desc)) {
    overrides_info <- get_intent_overrides(path_to_desc)
  }

  load_intent_repos(project, more_repos = overrides_info$extra_repos)

  # Apply overrides to pkgs
  resolved_pkgs <- vapply(
    pkgs,
    function(pkg) {
      if (pkg %in% names(overrides_info$overrides)) {
        overrides_info$overrides[[pkg]]$ref
      } else {
        pkg
      }
    },
    character(1)
  )

  lib_loc <- renv::paths$library(project = project)
  message("Installing packages into ", lib_loc, "...")
  pak::pkg_install(resolved_pkgs, lib = lib_loc, ask = FALSE)
}

#' @keywords internal
intent_snapshot <- function(project) {
  load_intent_repos(project)
  renv::snapshot(
    project = project,
    library = renv::paths$library(project = project),
    lockfile = file.path(project, "renv.lock"),
    dev = TRUE,
    prompt = FALSE
  )
}

#' @keywords internal
intent_restore <- function(project) {
  load_intent_repos(project)
  renv::restore(
    project = project,
    lockfile = file.path(project, "renv.lock"),
    clean = TRUE,
    prompt = FALSE
  )
}

#' @keywords internal
intent_set_project_dep <- function(project, package, type, version = "*") {
  desc::desc_set_dep(
    package = package,
    type = type,
    version = version,
    file = file.path(project, "DESCRIPTION"),
    normalize = TRUE
  )
}

#' @keywords internal
intent_del_project_dep <- function(project, package) {
  desc::desc_del_dep(
    package = package,
    file = file.path(project, "DESCRIPTION"),
    normalize = TRUE
  )
}

#' @keywords internal
intent_get_project_deps <- function(project) {
  desc::desc_get_deps(
    file = file.path(project, "DESCRIPTION")
  )
}
