test_that("cmd_status reports manifest, lockfile, and library drift", {
  tmp_dir <- tempfile()
  lib_dir <- tempfile()
  dir.create(tmp_dir)
  dir.create(lib_dir)
  dir.create(file.path(lib_dir, "glue"))
  on.exit(unlink(c(tmp_dir, lib_dir), recursive = TRUE))

  writeLines(
    c(
      "Package: testpkg",
      "Imports:",
      "    glue,",
      "    R6"
    ),
    file.path(tmp_dir, "DESCRIPTION")
  )
  writeLines("{}", file.path(tmp_dir, "renv.lock"))

  mockery::stub(cmd_status, "intent_locked_packages", function(project) {
    c("glue", "rlang")
  })
  mockery::stub(cmd_status, "backend_library", function(project) lib_dir)
  mockery::stub(cmd_status, "intent_library_packages", function(project) {
    "glue"
  })

  current_status <- cmd_status(project = tmp_dir)

  expect_s3_class(current_status, "intent_status")
  expect_setequal(current_status$manifest_packages, c("glue", "R6"))
  expect_setequal(current_status$locked_packages, c("glue", "rlang"))
  expect_equal(current_status$missing_from_lockfile, "R6")
  expect_equal(current_status$extra_in_lockfile, "rlang")
  expect_equal(current_status$missing_from_library, "rlang")
  expect_equal(nrow(current_status$source_violations), 0)
})

test_that("source policy flags repository name mismatches", {
  lock <- list(
    Packages = list(
      glue = list(
        Version = "1.0.0",
        Source = "Repository",
        Repository = "RSPM"
      )
    )
  )
  policy <- intent_default_source_policy()

  violations <- intent_check_source_policy(
    lock,
    repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"),
    source_policy = policy
  )

  expect_equal(violations$package, "glue")
  expect_match(violations$reason, "RSPM")
})

test_that("source policy exempts tool packages", {
  lock <- list(
    Packages = list(
      intent = list(Version = "0.0.1", Source = "unknown"),
      renv = list(Version = "1.0.0", Source = "unknown"),
      pak = list(Version = "1.0.0", Source = "unknown")
    )
  )
  policy <- intent_default_source_policy()

  violations <- intent_check_source_policy(
    lock,
    repos = character(),
    source_policy = policy
  )

  expect_equal(nrow(violations), 0)
})

test_that("status delegates to command layer", {
  expected <- new_intent_status(
    project = "project",
    manifest_packages = character(),
    locked_packages = character(),
    missing_from_lockfile = character(),
    extra_in_lockfile = character(),
    library_path = "library",
    missing_from_library = character()
  )

  mockery::stub(status, "cmd_status", function(project) expected)

  expect_identical(status(project = "project"), expected)
})

test_that("add dry-run returns a plan without mutating DESCRIPTION", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  desc_path <- file.path(tmp_dir, "DESCRIPTION")
  writeLines("Package: testpkg", desc_path)
  before <- readLines(desc_path)

  mockery::stub(cmd_add, "intent_install", function(...) {
    stop("install should not run")
  })
  mockery::stub(cmd_add, "intent_set_project_dep", function(...) {
    stop("manifest update should not run")
  })
  mockery::stub(cmd_add, "intent_snapshot", function(...) {
    stop("snapshot should not run")
  })

  plan <- cmd_add("glue", project = tmp_dir, dry_run = TRUE)

  expect_s3_class(plan, "intent_plan")
  expect_equal(plan$command, "add")
  expect_equal(plan$packages, "glue")
  expect_equal(readLines(desc_path), before)
})

test_that("remove dry-run returns a plan without mutating DESCRIPTION", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  desc_path <- file.path(tmp_dir, "DESCRIPTION")
  writeLines(
    c(
      "Package: testpkg",
      "Imports:",
      "    glue"
    ),
    desc_path
  )
  before <- readLines(desc_path)

  mockery::stub(cmd_remove, "intent_del_project_dep", function(...) {
    stop("manifest update should not run")
  })
  mockery::stub(cmd_remove, "backend_remove", function(...) {
    stop("remove should not run")
  })
  mockery::stub(cmd_remove, "intent_snapshot", function(...) {
    stop("snapshot should not run")
  })
  mockery::stub(cmd_remove, "intent_restore", function(...) {
    stop("restore should not run")
  })

  plan <- cmd_remove("glue", project = tmp_dir, dry_run = TRUE)

  expect_s3_class(plan, "intent_plan")
  expect_equal(plan$command, "remove")
  expect_equal(plan$packages, "glue")
  expect_equal(readLines(desc_path), before)
})

test_that("sync dry-run returns planned work without backend mutation", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  writeLines(
    c(
      "Package: testpkg",
      "Imports:",
      "    glue"
    ),
    file.path(tmp_dir, "DESCRIPTION")
  )

  mockery::stub(cmd_sync, "intent_install", function(...) {
    stop("install should not run")
  })
  mockery::stub(cmd_sync, "intent_snapshot", function(...) {
    stop("snapshot should not run")
  })
  mockery::stub(cmd_sync, "intent_restore", function(...) {
    stop("restore should not run")
  })

  plan <- cmd_sync(project = tmp_dir, dry_run = TRUE)

  expect_s3_class(plan, "intent_plan")
  expect_equal(plan$command, "sync")
  expect_equal(plan$packages, "glue")
  expect_true("would_restore" %in% plan$actions)
})

test_that("as.character.intent_status returns valid JSON", {
  obj <- new_intent_status(
    project = "/path/to/project",
    manifest_packages = c("dplyr", "glue"),
    locked_packages = c("dplyr", "glue", "R6"),
    missing_from_lockfile = character(),
    extra_in_lockfile = "R6",
    library_path = "/path/to/library",
    missing_from_library = "R6"
  )

  json_str <- as.character(obj)
  parsed <- jsonlite::fromJSON(json_str)

  expect_equal(parsed$project, "/path/to/project")
  expect_equal(parsed$manifest_packages, c("dplyr", "glue"))
  expect_length(parsed$missing_from_lockfile, 0)
  expect_equal(parsed$extra_in_lockfile, "R6")
  expect_named(
    parsed,
    c(
      "project",
      "manifest_packages",
      "locked_packages",
      "missing_from_lockfile",
      "extra_in_lockfile",
      "library_path",
      "missing_from_library",
      "source_policy",
      "source_violations"
    )
  )
})

test_that("as.character.intent_plan returns valid JSON", {
  obj <- new_intent_plan(
    project = "/path/to/project",
    command = "add",
    actions = c(
      "would_install: dplyr",
      "would_update_manifest: Imports",
      "would_snapshot"
    ),
    packages = "dplyr"
  )

  json_str <- as.character(obj)
  parsed <- jsonlite::fromJSON(json_str)

  expect_equal(parsed$project, "/path/to/project")
  expect_equal(parsed$command, "add")
  expect_equal(parsed$packages, "dplyr")
  expect_length(parsed$actions, 3)
})
