test_that("cli_main dispatches status command", {
  called <- NULL
  mockery::stub(cli_main, "cli_status", function(args) {
    called <<- args
  })

  cli_main(c("status", "--project", "proj"))

  expect_equal(called, c("--project", "proj"))
})

test_that("cli status parser calls command layer", {
  called <- NULL
  mockery::stub(cli_status, "cmd_status", function(project) {
    called <<- list(project = project)
  })

  cli_status(c("--project", "proj"))

  expect_equal(called$project, "proj")
})

test_that("cli dispatches sync dry-run", {
  called <- NULL
  mockery::stub(cli_sync, "cmd_sync", function(project, dry_run) {
    called <<- list(project = project, dry_run = dry_run)
  })

  cli_sync(c("--project", "proj", "--dry-run"))

  expect_equal(called$project, "proj")
  expect_true(called$dry_run)
})

test_that("cli dispatches add with dev and dry-run flags", {
  called <- NULL
  mockery::stub(cli_add, "cmd_add", function(pkgs, dev, project, dry_run) {
    called <<- list(
      pkgs = pkgs,
      dev = dev,
      project = project,
      dry_run = dry_run
    )
  })

  cli_add(c("--project", "proj", "--dev", "--dry-run", "glue", "rlang"))

  expect_equal(called$pkgs, c("glue", "rlang"))
  expect_true(called$dev)
  expect_equal(called$project, "proj")
  expect_true(called$dry_run)
})

test_that("cli dispatches remove", {
  called <- NULL
  mockery::stub(cli_remove, "cmd_remove", function(pkgs, project, dry_run) {
    called <<- list(pkgs = pkgs, project = project, dry_run = dry_run)
  })

  cli_remove(c("--project", "proj", "glue"))

  expect_equal(called$pkgs, "glue")
  expect_equal(called$project, "proj")
  expect_false(called$dry_run)
})

test_that("cli dispatches init repos", {
  called <- NULL
  mockery::stub(cli_init, "cmd_init", function(path, repos) {
    called <<- list(path = path, repos = repos)
  })

  cli_init(c("proj", "--repo", "CRAN=https://example.test"))

  expect_equal(called$path, "proj")
  expect_equal(called$repos[["CRAN"]], "https://example.test")
})

test_that("cli reports invalid input", {
  expect_error(cli_main("unknown"), "Unknown command")
  expect_error(cli_main("add"), "No packages specified")
  expect_error(cli_main(c("init", "--repo", "broken")), "NAME=URL")
  expect_error(cli_main(c("sync", "glue")), "does not accept package")
})
