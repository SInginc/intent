# Core domain functions for project status inspection.
# These are called by the command layer (cmd_status) and by the public R API.

new_intent_status <- function(
  project,
  manifest_packages,
  locked_packages,
  missing_from_lockfile,
  extra_in_lockfile,
  library_path,
  missing_from_library,
  source_policy = NULL,
  source_violations = intent_source_violations_empty()
) {
  structure(
    list(
      project = project,
      manifest_packages = manifest_packages,
      locked_packages = locked_packages,
      missing_from_lockfile = missing_from_lockfile,
      extra_in_lockfile = extra_in_lockfile,
      library_path = library_path,
      missing_from_library = missing_from_library,
      source_policy = source_policy,
      source_violations = source_violations
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

intent_source_violations_empty <- function() {
  data.frame(
    package = character(),
    source = character(),
    repository = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )
}

intent_lockfile_provenance <- function(lock) {
  packages <- lock$Packages %||% list()
  rows <- lapply(names(packages), function(pkg) {
    record <- packages[[pkg]]
    source <- intent_lock_record_source(record)
    data.frame(
      package = pkg,
      source = source,
      repository = record[["Repository"]] %||% NA_character_,
      repository_url = intent_lock_record_repository_url(record),
      stringsAsFactors = FALSE
    )
  })

  if (length(rows) == 0) {
    return(data.frame(
      package = character(),
      source = character(),
      repository = character(),
      repository_url = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

intent_lock_record_source <- function(record) {
  source <- tolower(record[["Source"]] %||% "")

  if (identical(source, "repository") || !is.null(record[["Repository"]])) {
    return("repository")
  }
  if (source %in% c("github", "bioconductor", "bioc", "url", "local")) {
    return(if (identical(source, "bioconductor")) "bioc" else source)
  }
  if (grepl("github", source, fixed = TRUE)) {
    return("github")
  }
  if (grepl("bioconductor|bioc", source)) {
    return("bioc")
  }
  if (grepl("^https?://", record[["RemoteUrl"]] %||% "")) {
    return("url")
  }
  if (!is.null(record[["RemoteType"]])) {
    remote_type <- tolower(record[["RemoteType"]])
    if (remote_type %in% c("github", "bioc", "url", "local")) {
      return(remote_type)
    }
  }

  "unknown"
}

intent_lock_record_repository_url <- function(record) {
  candidates <- c(
    record[["RemoteRepos"]],
    record[["RepositoryURL"]],
    record[["RepoURL"]]
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  candidates[[1]]
}

intent_check_source_policy <- function(lock, repos, source_policy) {
  if (is.null(source_policy) || identical(source_policy$mode, "off")) {
    return(intent_source_violations_empty())
  }

  provenance <- intent_lockfile_provenance(lock)
  if (nrow(provenance) == 0) {
    return(intent_source_violations_empty())
  }

  provenance <- provenance[
    !provenance$package %in% source_policy$exempt_packages,
    ,
    drop = FALSE
  ]
  if (nrow(provenance) == 0) {
    return(intent_source_violations_empty())
  }

  violations <- intent_source_violations_empty()
  for (i in seq_len(nrow(provenance))) {
    row <- provenance[i, , drop = FALSE]
    source <- row$source
    if (!isTRUE(source_policy$allow[[source]])) {
      violations <- rbind(
        violations,
        intent_source_violation(
          row$package,
          source,
          row$repository,
          sprintf("source class '%s' is not allowed", source)
        )
      )
      next
    }

    if (identical(source, "repository")) {
      violations <- rbind(
        violations,
        intent_check_repository_policy(row, repos)
      )
    }
  }

  violations
}

intent_source_violation <- function(package, source, repository, reason) {
  data.frame(
    package = package,
    source = source,
    repository = repository %||% NA_character_,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

intent_check_repository_policy <- function(row, repos) {
  repository <- row$repository
  if (is.na(repository) || !nzchar(repository)) {
    return(intent_source_violation(
      row$package,
      row$source,
      repository,
      "repository source has no repository name"
    ))
  }

  if (!repository %in% names(repos)) {
    return(intent_source_violation(
      row$package,
      row$source,
      repository,
      sprintf(
        "repository '%s' is not declared in Config/intent/repos",
        repository
      )
    ))
  }

  repository_url <- row$repository_url
  if (
    !is.na(repository_url) &&
      nzchar(repository_url) &&
      !identical(
        normalize_repo_url(repository_url),
        normalize_repo_url(repos[[repository]])
      )
  ) {
    return(intent_source_violation(
      row$package,
      row$source,
      repository,
      sprintf(
        "repository '%s' URL does not match Config/intent/repos",
        repository
      )
    ))
  }

  intent_source_violations_empty()
}

normalize_repo_url <- function(url) {
  sub("/+$", "", trimws(url))
}

intent_library_packages <- function(project) {
  library_path <- backend_library(project)
  if (!dir.exists(library_path)) {
    return(character())
  }

  sort(basename(list.dirs(library_path, full.names = TRUE, recursive = FALSE)))
}
