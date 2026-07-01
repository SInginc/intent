cmd_init <- function(path = ".", repos = NULL) {
  project_dir <- normalizePath(path, mustWork = FALSE)

  if (!dir.exists(path)) {
    dir.create(project_dir, recursive = TRUE)
  }

  desc_path <- file.path(project_dir, "DESCRIPTION")

  pkg_name <- gsub("[^[:alnum:].]", ".", basename(project_dir))
  if (grepl("^[0-9.]", pkg_name)) {
    pkg_name <- paste0("pkg.", pkg_name)
  }

  if (!file.exists(desc_path)) {
    message("Creating DESCRIPTION file...")
    rproject <- desc::description$new("!new")
    rproject$set("Package", pkg_name)
  } else {
    rproject <- desc::description$new(desc_path)
  }

  if (!rproject$has_dep("pak")) {
    rproject$set_dep("pak", type = "Suggests")
  }
  if (!rproject$has_dep("renv")) {
    rproject$set_dep("renv", type = "Suggests")
  }
  if (!rproject$has_dep("intent")) {
    rproject$set_dep("intent", type = "Suggests")
  }

  current_repos <- get_repos(desc_path)
  has_repos <- length(current_repos) > 0

  if (has_repos && length(repos) > 0) {
    warning("Repositories already exist in DESCRIPTION. Skipping.")
  } else if (!has_repos && length(repos) > 0) {
    repo_names <- names(repos)
    if (is.null(repo_names) || any(repo_names == "")) {
      stop(
        "Repositories must be provided as a named vector (e.g., c(CRAN = 'url'))."
      )
    }

    for (i in seq_along(repos)) {
      rproject$set(
        sprintf("Config/intent/repos/%s", repo_names[[i]]),
        repos[[i]]
      )
    }
  } else if (has_repos && (is.null(repos) || length(repos) == 0)) {
    repos <- get_repos(desc_path)
  } else if (!has_repos && (is.null(repos) || length(repos) == 0)) {
    stop("No repositories provided.")
  }

  rproject$write(desc_path)

  if (!dir.exists(file.path(project_dir, "renv"))) {
    backend_init(project_dir, repos)
  } else {
    cmd_sync(project = project_dir)
  }

  renviron_path <- file.path(project_dir, ".Renviron")
  renviron_lines <- if (file.exists(renviron_path)) {
    readLines(renviron_path)
  } else {
    character()
  }

  if (!any(grepl("RENV_CONFIG_PAK_ENABLED", renviron_lines))) {
    write(
      "# intent modification: start",
      file = renviron_path,
      append = TRUE
    )
    write("RENV_CONFIG_PAK_ENABLED = TRUE", file = renviron_path, append = TRUE)
    write("# intent modification: end", file = renviron_path, append = TRUE)
  }

  message("intent project initialized successfully in ", project_dir)
  message("Please restart your R session for changes to take effect.")
  invisible(project_dir)
}

cmd_add <- function(pkgs, dev = FALSE, project = NULL, dry_run = FALSE) {
  if (missing(pkgs) || length(pkgs) == 0) {
    stop("No packages specified.", call. = FALSE)
  }

  project <- resolve_project(project)
  desc_type <- if (dev) "Suggests" else "Imports"

  if (dry_run) {
    return(new_intent_plan(
      project = project,
      command = "add",
      packages = pkgs,
      actions = c(
        paste0("would_install: ", paste(pkgs, collapse = ", ")),
        paste0("would_update_manifest: ", desc_type),
        "would_snapshot"
      )
    ))
  }

  message("Adding ", paste(pkgs, collapse = ", "), " to ", desc_type)

  intent_install(project, pkgs)

  for (pkg in pkgs) {
    pkg_name <- basename(pkg)
    intent_set_project_dep(project, package = pkg_name, type = desc_type)
  }

  message("Updating lockfile...")
  intent_snapshot(project)

  invisible(pkgs)
}

cmd_remove <- function(pkgs, project = NULL, dry_run = FALSE) {
  if (missing(pkgs) || length(pkgs) == 0) {
    stop("No packages specified.", call. = FALSE)
  }

  project <- resolve_project(project)

  if (dry_run) {
    return(new_intent_plan(
      project = project,
      command = "remove",
      packages = pkgs,
      actions = c(
        paste0("would_remove_from_manifest: ", paste(pkgs, collapse = ", ")),
        paste0("would_remove_from_library: ", paste(pkgs, collapse = ", ")),
        "would_snapshot",
        "would_restore"
      )
    ))
  }

  message("Removing ", paste(pkgs, collapse = ", "), " from DESCRIPTION")
  for (pkg in pkgs) {
    intent_del_project_dep(project, package = pkg)
  }

  message("Removing packages from library...")
  backend_remove(project, pkgs)

  message("Updating lockfile...")
  intent_snapshot(project)

  message("Pruning orphan dependencies...")
  intent_restore(project)

  invisible(pkgs)
}

cmd_sync <- function(project = NULL, dry_run = FALSE) {
  project <- resolve_project(project)

  desc_path <- file.path(project, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    stop(
      "No DESCRIPTION file found. Run `intent::init()` or `intent::add()` first.",
      call. = FALSE
    )
  }

  message("Syncing environment...")

  desc_deps <- desc::desc_get_deps(file = desc_path)
  target_types <- c("Imports", "Suggests")
  intent_pkgs <- desc_deps$package[desc_deps$type %in% target_types]
  intent_pkgs <- intent_pkgs[intent_pkgs != "R"]

  overrides_info <- get_intent_overrides(desc_path)

  lock_path <- file.path(project, "renv.lock")
  if (file.exists(lock_path)) {
    lock <- backend_read_lockfile(project)
    lock_pkgs <- names(lock$Packages)
    missing_pkgs <- setdiff(intent_pkgs, lock_pkgs)

    overridden_intent_pkgs <- intersect(
      intent_pkgs,
      names(overrides_info$overrides)
    )
    for (pkg in overridden_intent_pkgs) {
      if (pkg %in% lock_pkgs) {
        lock_ver <- lock$Packages[[pkg]]$Version
        parsed <- overrides_info$overrides[[pkg]]
        if (lock_ver != parsed$version) {
          missing_pkgs <- c(missing_pkgs, pkg)
        }
      }
    }
    missing_pkgs <- unique(missing_pkgs)
  } else {
    missing_pkgs <- intent_pkgs
  }

  missing_pkgs <- missing_pkgs[missing_pkgs != "intent"]

  if (dry_run) {
    actions <- "would_restore"
    if (length(missing_pkgs) > 0) {
      actions <- c(
        paste0("would_install: ", paste(missing_pkgs, collapse = ", ")),
        "would_snapshot",
        actions
      )
    }

    return(new_intent_plan(
      project = project,
      command = "sync",
      actions = actions,
      packages = missing_pkgs
    ))
  }

  if (length(missing_pkgs) > 0) {
    message(
      "Installing/Updating packages from DESCRIPTION: ",
      paste(missing_pkgs, collapse = ", ")
    )
    intent_install(project, missing_pkgs)

    message("Updating lockfile...")
    intent_snapshot(project)
  }

  message("Restoring library from lockfile...")
  intent_restore(project)

  message("Environment synchronized.")
}

cmd_status <- function(project = NULL) {
  project <- resolve_project(project)

  manifest_packages <- intent_manifest_packages(project)
  locked_packages <- intent_locked_packages(project)
  library_path <- backend_library(project)
  library_packages <- intent_library_packages(project)

  new_intent_status(
    project = project,
    manifest_packages = manifest_packages,
    locked_packages = locked_packages,
    missing_from_lockfile = setdiff(manifest_packages, locked_packages),
    extra_in_lockfile = setdiff(locked_packages, manifest_packages),
    library_path = library_path,
    missing_from_library = setdiff(locked_packages, library_packages)
  )
}
