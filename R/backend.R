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

backend_install <- function(project, pkgs) {
  lib_loc <- backend_library(project)
  message("Installing packages into ", lib_loc, "...")
  pak::pkg_install(pkgs, lib = lib_loc, ask = FALSE)
}

backend_remove <- function(project, pkgs) {
  renv::remove(pkgs, project = project)
}

backend_snapshot <- function(project) {
  renv::snapshot(
    project = project,
    library = backend_library(project),
    lockfile = file.path(project, "renv.lock"),
    dev = TRUE,
    prompt = FALSE
  )
}

backend_restore <- function(project) {
  renv::restore(
    project = project,
    lockfile = file.path(project, "renv.lock"),
    clean = TRUE,
    prompt = FALSE
  )
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
