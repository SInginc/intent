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
  repos = c(
    "CRAN" = "https://packagemanager.posit.co/cran/latest"
  )
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
    rproject <- desc::description$new("!new")
    rproject$set("Package", pkg_name)
  } else {
    rproject <- desc::description$new(desc_path)
  }

  rproject$set_dep("pak", type = "Suggests")
  rproject$set_dep("renv", type = "Suggests")
  rproject$set_dep("intent", type = "Suggests")

  # Add repositories as custom Config/intent fields
  if (length(repos) > 0) {
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
  }

  rproject$write(desc_path)

  # 2. State Init: renv::init(bare = TRUE)
  # We let renv initialize fully to avoid path aliasing issues during independent snapshot
  # And we set explicit snapshot type immediately via settings.
  renv::init(
    project = project_dir,
    bare = TRUE,
    restart = FALSE,
    settings = list(
      snapshot.type = "explicit"
    ),
    repos = repos,
    load = FALSE
  )
  utils::install.packages(
    pkgs = c("pak", "renv"),
    lib = renv::paths$library(project = project_dir),
    repos = repos
  )
  renv::snapshot(
    project = project_dir,
    library = renv::paths$library(project = project_dir),
    lockfile = file.path(project_dir, "renv.lock"),
    packages = c("pak", "renv"),
    exclude = c("intent"),
    repos = repos,
    prompt = FALSE
  )
  # HACK
  renv_lock <- renv::lockfile_read(file.path(project_dir, "renv.lock"))
  renv_lock$R$Repositories <- repos
  renv::lockfile_write(
    renv_lock,
    file = file.path(project_dir, "renv.lock"),
    project = project_dir
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
    write("RENV_CONFIG_PAK_ENABLED = TRUE", file = renviron_path, append = TRUE)
    write("# intent modification: end", file = renviron_path, append = TRUE)
  }

  message("intent project initialized successfully in ", project_dir)
  message("Please restart your R session for changes to take effect.")
  invisible(project_dir)
}
