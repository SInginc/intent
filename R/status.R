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

#' @export
as.character.intent_status <- function(x, ...) {
  jsonlite::toJSON(unclass(x), auto_unbox = TRUE, pretty = FALSE)
}

#' @export
as.character.intent_plan <- function(x, ...) {
  jsonlite::toJSON(unclass(x), auto_unbox = TRUE, pretty = FALSE)
}
