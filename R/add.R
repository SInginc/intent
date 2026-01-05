#' Add a Dependency
#'
#' Adds a package to the `DESCRIPTION` file, installs it into the local library using `pak`,
#' and updates the `renv.lock` file.
#'
#' @param pkgs Character vector. Names of packages to add. Supports CRAN names or "user/repo" for GitHub.
#' @param dev Logical. If `TRUE`, adds packages to "Suggests". If `FALSE` (default), adds to "Imports".
#'
#' @export
add <- function(pkgs, dev = FALSE) {
  check_missing_deps()

  if (missing(pkgs) || length(pkgs) == 0) {
    stop("No packages specified.", call. = FALSE)
  }

  if (!file.exists(file.path(renv::project(), "DESCRIPTION"))) {
    stop(
      "No DESCRIPTION file found. Run `intent::init()` first.",
      call. = FALSE
    )
  }

  # 1. Manifest Update: Add to DESCRIPTION
  # We use the desc package for this
  desc_type <- if (dev) "Suggests" else "Imports"

  message("Adding ", paste(pkgs, collapse = ", "), " to ", desc_type)

  # 2. Installation: Use renv to install into the project library
  intent_install(pkgs)

  # Helper to add deps one by one
  # desc::desc_set_dep handles adding or updating dependencies
  for (pkg in pkgs) {
    # Extract package name if it's a remote like user/repo
    # This is a simplification; for complex remotes, we might need more logic.
    # But for now, we assume standard usage.
    # Note: desc_set_dep expects just the package name for the 'package' arg.
    pkg_name <- basename(pkg) # rough heuristic for user/repo -> repo
    intent_set_project_dep(package = pkg_name, type = desc_type)
  }

  # 3. Locking: renv snapshot
  # Because we set snapshot.type logic in init, this should only snapshot what is in DESCRIPTION.
  # But to be safe and forceful (per spec):
  message("Updating lockfile...")
  intent_snapshot()

  invisible(pkgs)
}
