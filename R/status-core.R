# Core domain functions for project status inspection.
# These are called by the command layer (cmd_status) and by the public R API.

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

intent_lock_dependency_closure <- function(lock, roots) {
  packages <- lock$Packages %||% list()
  roots <- intersect(unique(roots), names(packages))
  seen <- character()
  queue <- roots

  while (length(queue) > 0) {
    pkg <- queue[[1]]
    queue <- queue[-1]

    if (pkg %in% seen) {
      next
    }

    seen <- c(seen, pkg)
    deps <- intent_lock_package_dependencies(packages[[pkg]])
    deps <- intersect(deps, names(packages))
    queue <- unique(c(queue, setdiff(deps, seen)))
  }

  sort(seen)
}

intent_lock_package_dependencies <- function(record) {
  fields <- c("Depends", "Imports", "LinkingTo")
  deps <- unlist(record[intersect(fields, names(record))], use.names = FALSE)
  deps <- gsub("\\s*\\(.*\\)$", "", deps)
  deps <- trimws(deps)
  deps <- deps[nzchar(deps)]
  sort(setdiff(
    unique(deps),
    c(
      "R",
      "base",
      "compiler",
      "datasets",
      "graphics",
      "grDevices",
      "grid",
      "methods",
      "parallel",
      "splines",
      "stats",
      "stats4",
      "tcltk",
      "tools",
      "utils"
    )
  ))
}

intent_library_packages <- function(project) {
  library_path <- backend_library(project)
  if (!dir.exists(library_path)) {
    return(character())
  }

  sort(basename(list.dirs(library_path, full.names = TRUE, recursive = FALSE)))
}
