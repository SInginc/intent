backend_init <- function(project, repos) {
  renv::init(
    project = project,
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
    lib = backend_library(project),
    repos = repos
  )

  renv::snapshot(
    project = project,
    library = backend_library(project),
    lockfile = file.path(project, "renv.lock"),
    packages = c("pak", "renv"),
    exclude = c("intent"),
    repos = repos,
    prompt = FALSE
  )

  lockfile <- backend_read_lockfile(project)
  lockfile$R$Repositories <- repos
  backend_write_lockfile(lockfile, project)
}

backend_hydrate <- function(project, pkgs, sources = NULL) {
  renv::hydrate(
    packages = pkgs,
    library = backend_library(project),
    sources = sources,
    repos = character(),
    prompt = FALSE,
    report = FALSE,
    project = project
  )
}

backend_install <- function(project, pkgs, repos = NULL) {
  lib_loc <- backend_library(project)
  message("Installing packages into ", lib_loc, "...")
  if (length(repos) > 0) {
    old_repos <- options(repos = repos)
    on.exit(options(old_repos), add = TRUE)
  }
  pak::pkg_install(pkgs, lib = lib_loc, ask = FALSE)
}

backend_remove <- function(project, pkgs) {
  renv::remove(pkgs, project = project, library = backend_library(project))
}

backend_snapshot <- function(
  project,
  repos = NULL,
  force = FALSE,
  lockfile = file.path(project, "renv.lock")
) {
  sn_args <- list(
    project = project,
    library = backend_library(project),
    lockfile = lockfile,
    dev = TRUE,
    prompt = FALSE,
    force = force
  )
  if (length(repos) > 0) {
    sn_args$repos <- repos
  }
  do.call(renv::snapshot, sn_args)
}

backend_restore <- function(project, repos = NULL) {
  res_args <- list(
    project = project,
    library = backend_library(project),
    lockfile = file.path(project, "renv.lock"),
    clean = TRUE,
    exclude = "intent",
    prompt = FALSE
  )
  if (length(repos) > 0) {
    res_args$repos <- repos
  }
  do.call(renv::restore, res_args)
}

backend_read_lockfile <- function(project) {
  renv::lockfile_read(file.path(project, "renv.lock"))
}

backend_write_lockfile <- function(lockfile, project) {
  renv::lockfile_write(
    lockfile,
    file = file.path(project, "renv.lock"),
    project = project
  )
}

backend_library <- function(project) {
  renv::paths$library(project = project)
}
