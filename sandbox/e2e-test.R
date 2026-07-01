# intent End-to-End Test
#
# Run from the sandbox/ directory:
#   cd sandbox
#   Rscript e2e-test.R
#
# Or from repo root:
#   Rscript sandbox/e2e-test.R
#
# This script exercises every intent command against a real project.

# Resolve the repo root regardless of CWD.
repo_root <- if (dir.exists("../R")) normalizePath("..") else normalizePath(".")
setwd(repo_root)

if (requireNamespace("intent", quietly = TRUE)) {
  library(intent)
} else {
  pkgload::load_all()
}

# Work inside sandbox/
sandbox_dir <- file.path(repo_root, "sandbox")
project_dir <- file.path(sandbox_dir, "test-project")
unlink(project_dir, recursive = TRUE)

halt <- function(msg) stop(msg, call. = FALSE)

# ── 1. init (default repo) ──────────────────────────────────────────────
cat("\n=== 1. init (default repo) ===\n")
intent::init(project_dir)
stopifnot(file.exists(file.path(project_dir, "DESCRIPTION")))
stopifnot(file.exists(file.path(project_dir, "renv.lock")))
stopifnot(file.exists(file.path(project_dir, ".Rprofile")))
stopifnot(file.exists(file.path(project_dir, ".Renviron")))
cat("✔ init created all required files\n")

# ── 2. status (empty project) ───────────────────────────────────────────
cat("\n=== 2. status (empty project) ===\n")
s <- intent::status(project_dir)
print(s)
stopifnot(inherits(s, "intent_status"))
cat("✔ status returns intent_status object\n")

# ── 3. add (dry-run) ────────────────────────────────────────────────────
cat("\n=== 3. add (dry-run) ===\n")
plan <- intent::add("glue", project = project_dir, dry_run = TRUE)
print(plan)
stopifnot(inherits(plan, "intent_plan"))
stopifnot(plan$command == "add")
cat("✔ add dry-run returns plan without installing\n")

# ── 4. add (real) ───────────────────────────────────────────────────────
cat("\n=== 4. add (real) ===\n")
intent::add("glue", project = project_dir)
stopifnot("glue" %in% intent::status(project_dir)$manifest_packages)
cat("✔ glue added to manifest, installed, and locked\n")

# ── 5. add dev dependency ───────────────────────────────────────────────
cat("\n=== 5. add (dev) ===\n")
intent::add("testthat", dev = TRUE, project = project_dir)
deps <- desc::desc_get_deps(file.path(project_dir, "DESCRIPTION"))
stopifnot("testthat" %in% deps$package[deps$type == "Suggests"])
cat("✔ testthat added to Suggests\n")

# ── 6. status (after adds) ──────────────────────────────────────────────
cat("\n=== 6. status (after adds) ===\n")
print(intent::status(project_dir))
cat("✔ status reports correct drift\n")

# ── 7. sync (dry-run) ───────────────────────────────────────────────────
cat("\n=== 7. sync (dry-run) ===\n")
plan <- intent::sync(project = project_dir, dry_run = TRUE)
print(plan)
stopifnot(inherits(plan, "intent_plan"))
cat("✔ sync dry-run returns plan\n")

# ── 8. sync (real) ──────────────────────────────────────────────────────
cat("\n=== 8. sync (real) ===\n")
intent::sync(project = project_dir)
cat("✔ sync completed\n")

# ── 9. JSON output ──────────────────────────────────────────────────────
cat("\n=== 9. JSON output ===\n")
json_status <- as.character(intent::status(project_dir))
stopifnot(grepl('"project"', json_status))
cat("✔ status as.character() returns valid JSON\n")

json_plan <- as.character(intent::add("cli", project = project_dir, dry_run = TRUE))
stopifnot(grepl('"command"', json_plan))
cat("✔ plan as.character() returns valid JSON\n")

# ── 10. remove (dry-run) ────────────────────────────────────────────────
cat("\n=== 10. remove (dry-run) ===\n")
plan <- intent::remove("cli", project = project_dir, dry_run = TRUE)
print(plan)
stopifnot(inherits(plan, "intent_plan"))
cat("✔ remove dry-run returns plan\n")

# ── 11. remove (real) ───────────────────────────────────────────────────
cat("\n=== 11. remove (real) ===\n")
intent::remove("testthat", project = project_dir)
s <- intent::status(project_dir)
stopifnot(!"testthat" %in% s$manifest_packages)
cat("✔ testthat removed from manifest and lockfile\n")

# ── 12. sync prune ──────────────────────────────────────────────────────
cat("\n=== 12. sync prune ===\n")
lock <- renv::lockfile_read(file.path(project_dir, "renv.lock"))
lock$Packages$orphan_test <- list(Package = "orphan_test", Version = "1.0.0")
renv::lockfile_write(lock, file = file.path(project_dir, "renv.lock"),
                     project = project_dir)

plan <- intent::sync(project = project_dir, dry_run = TRUE)
stopifnot(any(grepl("would_prune.*orphan_test", plan$actions)))
cat("✔ dry-run detects orphan in lockfile\n")

intent::sync(project = project_dir, prune = TRUE)
lock_after <- renv::lockfile_read(file.path(project_dir, "renv.lock"))
stopifnot(is.null(lock_after$Packages$orphan_test))
cat("✔ sync pruned orphan from lockfile\n")

# ── Cleanup ─────────────────────────────────────────────────────────────
unlink(project_dir, recursive = TRUE)

cat("\n═══════════════════════════════════════════════════\n")
cat("All 12 end-to-end checks passed.\n")
cat("═══════════════════════════════════════════════════\n")
