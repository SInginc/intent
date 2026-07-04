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
  mockery::stub(cli_sync, "cmd_sync", function(project, dry_run, prune) {
    called <<- list(project = project, dry_run = dry_run, prune = prune)
  })

  cli_sync(c("--project", "proj", "--dry-run"))

  expect_equal(called$project, "proj")
  expect_true(called$dry_run)
  expect_true(called$prune)
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
  mockery::stub(
    cli_init,
    "cmd_init",
    function(path, repos, confirm_repos, use_default_repo) {
      called <<- list(
        path = path,
        repos = repos,
        confirm_repos = confirm_repos,
        use_default_repo = use_default_repo
      )
    }
  )

  cli_init(c("proj", "--repo", "CRAN=https://example.test", "--yes"))

  expect_equal(called$path, "proj")
  expect_equal(called$repos[["CRAN"]], "https://example.test")
  expect_false(called$confirm_repos)
  expect_true(called$use_default_repo)
})

test_that("cli dispatches init without default repo", {
  called <- NULL
  mockery::stub(
    cli_init,
    "cmd_init",
    function(path, repos, confirm_repos, use_default_repo) {
      called <<- list(
        path = path,
        repos = repos,
        confirm_repos = confirm_repos,
        use_default_repo = use_default_repo
      )
    }
  )

  cli_init(c("proj", "--no-default-repo"))

  expect_equal(called$path, "proj")
  expect_null(called$repos)
  expect_false(called$use_default_repo)
})

test_that("cli_collect_repos parses init automation flags", {
  parsed <- cli_collect_repos(c(
    "--repo",
    "CRAN=https://example.test",
    "--yes",
    "--no-default-repo",
    "proj"
  ))

  expect_equal(parsed$args, "proj")
  expect_equal(parsed$repos[["CRAN"]], "https://example.test")
  expect_true(parsed$yes)
  expect_true(parsed$no_default_repo)
})

test_that("cmd_init can reject implicit default repository", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(
    cmd_init(
      path = tmp_dir,
      repos = NULL,
      install_self = "never",
      confirm_repos = FALSE,
      use_default_repo = FALSE
    ),
    "No repositories configured"
  )
})

test_that("cli reports invalid input", {
  expect_error(cli_main("unknown"), "Unknown command")
  expect_error(cli_main("add"), "No packages specified")
  expect_error(cli_main(c("init", "--repo", "broken")), "NAME=URL")
  expect_error(cli_main(c("sync", "glue")), "does not accept package")
})

test_that("cli_parse_common handles --json flag", {
  parsed <- cli_parse_common(c("--json", "--dry-run", "pkg"))
  expect_true(parsed$json)
  expect_true(parsed$dry_run)
  expect_equal(parsed$args, "pkg")
})

test_that("cli_parse_common handles --prune and --no-prune", {
  parsed_prune <- cli_parse_common(c("--prune", "pkg"))
  expect_false(isTRUE(parsed_prune$flags[["no_prune"]]))

  parsed_no <- cli_parse_common(c("--no-prune", "pkg"))
  expect_true(parsed_no$flags[["no_prune"]])
})

test_that("cli_print_help includes all commands and flags", {
  output <- capture.output(cli_print_help())
  text <- paste(output, collapse = "\n")

  expect_match(text, "intent init")
  expect_match(text, "intent add")
  expect_match(text, "intent remove")
  expect_match(text, "intent sync")
  expect_match(text, "intent status")
  expect_match(text, "--project")
  expect_match(text, "--dry-run")
  expect_match(text, "--json")
  expect_match(text, "--dev")
  expect_match(text, "--no-prune")
  expect_match(text, "--repo")
  expect_match(text, "--yes")
  expect_match(text, "--no-default-repo")
})
