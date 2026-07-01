#' Show Project Dependency Status
#'
#' Reports drift between the project manifest, lockfile, and local library
#' without changing project state.
#'
#' @param project Path to the project directory. Defaults to the current intent project.
#'
#' @return An `intent_status` object containing structured drift information.
#' @export
status <- function(project = NULL) {
  cmd_status(project = project)
}

#' @export
print.intent_status <- function(x, ...) {
  cat("Project: ", x$project, "\n", sep = "")
  cat("Library: ", x$library_path, "\n\n", sep = "")

  print_status_count("Manifest packages", x$manifest_packages)
  print_status_count("Locked packages", x$locked_packages)
  print_status_count("Missing from lockfile", x$missing_from_lockfile)
  print_status_count("Extra in lockfile", x$extra_in_lockfile)
  print_status_count("Missing from library", x$missing_from_library)

  invisible(x)
}

#' @export
print.intent_plan <- function(x, ...) {
  cat("Project: ", x$project, "\n", sep = "")
  cat("Command: ", x$command, "\n", sep = "")

  if (length(x$packages) > 0) {
    cat("Packages: ", paste(x$packages, collapse = ", "), "\n", sep = "")
  }

  cat("\nPlanned actions:\n")
  if (length(x$actions) == 0) {
    cat("  none\n")
  } else {
    for (action in x$actions) {
      cat("  - ", action, "\n", sep = "")
    }
  }

  invisible(x)
}

print_status_count <- function(label, values) {
  cat(label, ": ", length(values), sep = "")
  if (length(values) > 0) {
    cat(" (", paste(values, collapse = ", "), ")", sep = "")
  }
  cat("\n")
}

new_intent_status <- function(
  project,
  manifest_packages,
  locked_packages,
  missing_from_lockfile,
  extra_in_lockfile,
  library_path,
  missing_from_library
) {
  structure(
    list(
      project = project,
      manifest_packages = manifest_packages,
      locked_packages = locked_packages,
      missing_from_lockfile = missing_from_lockfile,
      extra_in_lockfile = extra_in_lockfile,
      library_path = library_path,
      missing_from_library = missing_from_library
    ),
    class = "intent_status"
  )
}

new_intent_plan <- function(
  project,
  command,
  actions,
  packages = character()
) {
  structure(
    list(
      project = project,
      command = command,
      actions = actions,
      packages = packages
    ),
    class = "intent_plan"
  )
}

intent_manifest_packages <- function(project) {
  desc_deps <- desc::desc_get_deps(file = file.path(project, "DESCRIPTION"))
  target_types <- c("Imports", "Suggests")
  packages <- desc_deps$package[desc_deps$type %in% target_types]
  sort(unique(packages[packages != "R"]))
}

intent_locked_packages <- function(project) {
  lock_path <- file.path(project, "renv.lock")
  if (!file.exists(lock_path)) {
    return(character())
  }

  lock <- backend_read_lockfile(project)
  sort(names(lock$Packages %||% list()))
}

#' @export
as.character.intent_status <- function(x, ...) {
  jsonlite::toJSON(unclass(x), auto_unbox = TRUE, pretty = FALSE)
}

#' @export
as.character.intent_plan <- function(x, ...) {
  jsonlite::toJSON(unclass(x), auto_unbox = TRUE, pretty = FALSE)
}

intent_library_packages <- function(project) {
  library_path <- backend_library(project)
  if (!dir.exists(library_path)) {
    return(character())
  }

  sort(basename(list.dirs(library_path, full.names = TRUE, recursive = FALSE)))
}
