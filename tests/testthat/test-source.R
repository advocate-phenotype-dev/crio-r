test_that("crio_source sets CRIO_* environment variables", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  withr::local_envvar(c(
    CRIO_PROJECT_ID  = NA_character_,
    CRIO_SCE_TIER    = NA_character_,
    CRIO_ENVIRONMENT = NA_character_,
    CRIO_SANDBOX     = NA_character_,
    CRIO_PI_ORCID    = NA_character_
  ))
  crio_source(tmp, sandbox = TRUE)
  expect_equal(Sys.getenv("CRIO_SCE_TIER"), "3")
  expect_equal(Sys.getenv("CRIO_ENVIRONMENT"), "azure_tre")
  expect_equal(Sys.getenv("CRIO_SANDBOX"), "true")
  expect_equal(Sys.getenv("CRIO_PI_ORCID"), "0000-0002-1234-5678")
  expect_false(nchar(Sys.getenv("CRIO_PROJECT_ID")) == 0)
})

test_that("crio_source CRIO_PROJECT_ID matches schema UUID", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  withr::local_envvar(c(CRIO_PROJECT_ID = NA_character_))
  crio_source(tmp, sandbox = TRUE)
  schema <- yaml::read_yaml(file.path(tmp, "advocate-phenotype.yaml"))
  expect_equal(Sys.getenv("CRIO_PROJECT_ID"), schema$project$id)
})

test_that("crio_source writes valid JSON session lock file", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  crio_source(tmp, sandbox = TRUE)
  lock_path <- file.path(tmp, ".advocate", "session.lock")
  expect_true(file.exists(lock_path))
  session <- jsonlite::fromJSON(paste(readLines(lock_path, warn = FALSE), collapse = "\n"))
  expect_equal(session$mode, "sandbox")
  expect_equal(session$sce_tier, 3L)
  expect_equal(session$environment, "azure_tre")
  expect_false(is.null(session$issued_at))
})

test_that("crio_source returns session list invisibly", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  result <- crio_source(tmp, sandbox = TRUE)
  expect_equal(result$mode, "sandbox")
  expect_equal(result$sce_tier, 3L)
  expect_equal(result$environment, "azure_tre")
  expect_false(is.null(result$issued_at))
})

test_that("crio_source creates .advocate directory if absent", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  expect_false(dir.exists(file.path(tmp, ".advocate")))
  crio_source(tmp, sandbox = TRUE)
  expect_true(dir.exists(file.path(tmp, ".advocate")))
})

test_that("crio_source aborts and returns empty list on invalid schema", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(phenotype = list(name = NULL)))
  out <- capture.output(result <- crio_source(tmp, sandbox = TRUE))
  expect_equal(result, list())
  expect_true(any(grepl("invalid", out, ignore.case = TRUE)))
})

test_that("crio_source walks up to find project root from subdirectory", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  subdir <- file.path(tmp, "src", "cohort_definition")
  dir.create(subdir, recursive = TRUE)
  result <- crio_source(subdir, sandbox = TRUE)
  expect_equal(result$mode, "sandbox")
  expect_equal(result$sce_tier, 3L)
})

test_that("crio_source errors when no advocate-phenotype.yaml in tree", {
  tmp <- withr::local_tempdir()
  expect_error(crio_source(tmp, sandbox = TRUE), "No advocate-phenotype.yaml found")
})

test_that("crio_source sandbox=FALSE errors (production not configured)", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  expect_error(crio_source(tmp, sandbox = FALSE), "not yet configured")
})
