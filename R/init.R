#' Initialize an intent Project
#'
#' Sets up a directory as an `intent` project by creating a `DESCRIPTION` file (if missing),
#' initializing a bare `renv` environment, setting the snapshot type to "explicit",
#' and configuring `.Rprofile` and `.Renviron`.
#'
#' @param path Character string. Path to the new project directory. Defaults to current working directory.
#' @param repos Character vector. Repositories to use. Defaults to `getOption("repos")`.
#'
#' @export
init <- function(
  path = ".",
  repos = if (Sys.info()[["sysname"]] == "Linux") {
    c(
      "CRAN" = "https://packagemanager.posit.co/cran/__linux__/manylinux_2_28/latest"
    )
  } else {
    c(
      "CRAN" = "https://packagemanager.posit.co/cran/latest"
    )
  }
) {
  check_missing_deps()

  project_dir <- normalizePath(path, mustWork = FALSE)

  if (!dir.exists(path)) {
    dir.create(project_dir, recursive = TRUE)
  }

  # 1. Infrastructure: Ensure DESCRIPTION exists
  desc_path <- file.path(project_dir, "DESCRIPTION")

  # Sanitize package name: replace non-alphanumeric chars (except .) with .
  pkg_name <- gsub("[^[:alnum:].]", ".", basename(project_dir))
  # Ensure it doesn't start with a number or dot (best effort)
  if (grepl("^[0-9.]", pkg_name)) {
    pkg_name <- paste0("pkg.", pkg_name)
  }

  if (!file.exists(desc_path)) {
    message("Creating DESCRIPTION file...")
    d <- desc::description$new("!new")
    d$set("Package", pkg_name)
  } else {
    d <- desc::description$new(desc_path)
  }

  d$set_dep("pak", type = "Suggests")
  d$set_dep("renv", type = "Suggests")
  d$set_dep("intent", type = "Suggests")

  # Add repositories as custom Config/intent fields
  if (length(repos) > 0) {
    repo_names <- names(repos)
    if (is.null(repo_names) || any(repo_names == "")) {
      stop(
        "Repositories must be provided as a named vector (e.g., c(CRAN = 'url'))."
      )
    }

    for (i in seq_along(repos)) {
      d$set(
        sprintf("Config/intent/repos/%s", repo_names[i]),
        repos[i]
      )
    }
  }

  d$write(desc_path)

  # 2. State Init: renv::init(bare = FALSE)
  # We let renv initialize fully to avoid path aliasing issues during independent snapshot
  # And we set explicit snapshot type immediately via settings.
  callr::r(
    function(project_dir, repos) {
      renv::init(
        project = project_dir,
        bare = TRUE,
        restart = FALSE,
        settings = list(
          snapshot.type = "explicit"
        ),
        repos = repos,
        load = TRUE
      )
      renv::install(
        "pak",
        repos = repos,
        library = renv::paths$library(project = project_dir),
        lock = TRUE,
        prompt = FALSE
      )
      renv::snapshot(
        lockfile = file.path(project_dir, "renv.lock"),
        type = "explicit",
        repos = repos,
        prompt = FALSE,
        dev = TRUE
      )
    },
    args = list(
      project_dir = project_dir,
      repos = repos
    )
  )

  # 3. Bootstrapping: Configure .Rprofile
  # No longer Injecting options(repos) here as it's managed via DESCRIPTION/intent

  # 4. Configure .Renviron for PAK
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
    write("RENV_CONFIG_PAK_ENABLED=TRUE", file = renviron_path, append = TRUE)
    write("# intent modification: end", file = renviron_path, append = TRUE)
  }

  message("intent project initialized successfully in ", project_dir)
  message("Please restart your R session for changes to take effect.")
  invisible(project_dir)
}
