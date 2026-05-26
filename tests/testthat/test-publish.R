make_git_library <- function(dir, env = parent.frame()) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # Isolate from user's global git config (e.g. GPG signing) for the
  # duration of the calling test.
  withr::local_envvar(
    c(GIT_CONFIG_NOSYSTEM = "1", GIT_CONFIG_GLOBAL = "/dev/null"),
    .local_envir = env
  )
  system2("git", c("-C", dir, "init"), stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dir, "config", "user.email", "test@test.com"),
          stdout = FALSE, stderr = FALSE)
  system2("git", c("-C", dir, "config", "user.name", "Test"),
          stdout = FALSE, stderr = FALSE)
  invisible(dir)
}

test_that("crio_publish copies advocate-phenotype.yaml to library", {
  tmp         <- withr::local_tempdir()
  project_dir <- file.path(tmp, "project")
  library_dir <- file.path(tmp, "library")
  dir.create(project_dir, recursive = TRUE)
  make_git_library(library_dir)
  write_schema(project_dir)

  crio_publish(project_dir, library_dir)

  schema <- yaml::read_yaml(file.path(project_dir, "advocate-phenotype.yaml"))
  dest   <- file.path(library_dir, "projects", schema$project$id)
  expect_true(dir.exists(dest))
  expect_true(file.exists(file.path(dest, "advocate-phenotype.yaml")))
})

test_that("crio_publish updates registry with correct entry fields", {
  tmp         <- withr::local_tempdir()
  project_dir <- file.path(tmp, "project")
  library_dir <- file.path(tmp, "library")
  dir.create(project_dir, recursive = TRUE)
  make_git_library(library_dir)
  write_schema(project_dir)

  crio_publish(project_dir, library_dir)

  schema  <- yaml::read_yaml(file.path(project_dir, "advocate-phenotype.yaml"))
  reg     <- yaml::read_yaml(file.path(library_dir, "registry.yaml"))
  ids     <- sapply(reg$projects, `[[`, "id")
  expect_true(schema$project$id %in% ids)

  entry <- Filter(function(p) p$id == schema$project$id, reg$projects)[[1]]
  expect_equal(entry$phenotype_name, "Heart Failure with Reduced EF")
  expect_equal(entry$pi_name, "Dr. Alice Smith")
  expect_equal(entry$domain, "condition")
  expect_equal(entry$validation_status, "draft")
})

test_that("crio_publish creates a git commit with [crio] prefix", {
  tmp         <- withr::local_tempdir()
  project_dir <- file.path(tmp, "project")
  library_dir <- file.path(tmp, "library")
  dir.create(project_dir, recursive = TRUE)
  make_git_library(library_dir)
  write_schema(project_dir)

  crio_publish(project_dir, library_dir, message = "initial")

  log <- system2("git", c("-C", library_dir, "log", "--oneline"), stdout = TRUE)
  expect_true(length(log) >= 1)
  expect_true(any(grepl("\\[crio\\]", log)))
  expect_true(any(grepl("Heart Failure", log)))
})

test_that("crio_publish upserts registry — second publish does not duplicate entry", {
  tmp         <- withr::local_tempdir()
  project_dir <- file.path(tmp, "project")
  library_dir <- file.path(tmp, "library")
  dir.create(project_dir, recursive = TRUE)
  make_git_library(library_dir)
  write_schema(project_dir)

  crio_publish(project_dir, library_dir)
  crio_publish(project_dir, library_dir)

  reg <- yaml::read_yaml(file.path(library_dir, "registry.yaml"))
  expect_equal(length(reg$projects), 1L)
})

test_that("crio_publish regenerates README with correct content", {
  tmp         <- withr::local_tempdir()
  project_dir <- file.path(tmp, "project")
  library_dir <- file.path(tmp, "library")
  dir.create(project_dir, recursive = TRUE)
  make_git_library(library_dir)
  write_schema(project_dir)

  crio_publish(project_dir, library_dir)

  schema <- yaml::read_yaml(file.path(project_dir, "advocate-phenotype.yaml"))
  dest   <- file.path(library_dir, "projects", schema$project$id)
  readme <- paste(readLines(file.path(dest, "README.md"), warn = FALSE), collapse = "\n")
  expect_true(grepl("Heart Failure with Reduced EF", readme))
  expect_true(grepl("Dr. Alice Smith", readme))
  expect_true(grepl("condition", readme))
})

test_that("crio_publish updates project timestamp in schema", {
  tmp         <- withr::local_tempdir()
  project_dir <- file.path(tmp, "project")
  library_dir <- file.path(tmp, "library")
  dir.create(project_dir, recursive = TRUE)
  make_git_library(library_dir)
  write_schema(project_dir)

  before <- yaml::read_yaml(file.path(project_dir, "advocate-phenotype.yaml"))$project$updated
  crio_publish(project_dir, library_dir)
  after  <- yaml::read_yaml(file.path(project_dir, "advocate-phenotype.yaml"))$project$updated
  expect_false(identical(before, after))
})

test_that("crio_publish aborts on invalid schema and writes nothing to library", {
  tmp         <- withr::local_tempdir()
  project_dir <- file.path(tmp, "project")
  library_dir <- file.path(tmp, "library")
  dir.create(project_dir, recursive = TRUE)
  make_git_library(library_dir)
  write_schema(project_dir, overrides = list(phenotype = list(name = NULL)))

  out <- capture.output(crio_publish(project_dir, library_dir))
  expect_true(any(grepl("invalid", out, ignore.case = TRUE)))
  expect_false(file.exists(file.path(library_dir, "registry.yaml")))
})

test_that("crio_publish sandbox mode skips push (no error without remote)", {
  tmp         <- withr::local_tempdir()
  project_dir <- file.path(tmp, "project")
  library_dir <- file.path(tmp, "library")
  dir.create(project_dir, recursive = TRUE)
  make_git_library(library_dir)
  write_schema(project_dir)

  out <- capture.output(crio_publish(project_dir, library_dir, sandbox = TRUE))
  expect_true(any(grepl("Sandbox mode", out, ignore.case = TRUE)))
  log <- system2("git", c("-C", library_dir, "log", "--oneline"), stdout = TRUE)
  expect_true(length(log) >= 1)
})

test_that(".update_registry creates registry when none exists", {
  tmp    <- withr::local_tempdir()
  schema <- list(
    project      = list(id = "test-uuid-1234", updated = "2026-01-01T00:00:00+00:00"),
    investigator = list(pi_name = "Dr. Test", pi_orcid = "0000-0001-2345-6789"),
    phenotype    = list(name = "Test Phenotype", domain = "condition",
                        validation_status = "draft"),
    compute      = list(sce_tier = 2L),
    contribution = list(deposit_eligible = FALSE, credit_granted = FALSE)
  )
  .update_registry(tmp, schema)
  reg <- yaml::read_yaml(file.path(tmp, "registry.yaml"))
  expect_equal(length(reg$projects), 1L)
  expect_equal(reg$projects[[1]]$id, "test-uuid-1234")
  expect_equal(reg$projects[[1]]$phenotype_name, "Test Phenotype")
})
