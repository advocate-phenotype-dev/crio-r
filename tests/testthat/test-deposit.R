test_that("crio_deposit sandbox marks deposit_submitted in schema file", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(
    phenotype = list(validation_status = "internal_validated")
  ))
  result <- crio_deposit(tmp, sandbox = TRUE)
  expect_true(result)
  schema <- yaml::read_yaml(file.path(tmp, "advocate-phenotype.yaml"))
  expect_true(isTRUE(schema$contribution$deposit_submitted))
  expect_false(is.null(schema$contribution$deposit_date))
})

test_that("crio_deposit sandbox updates project updated timestamp", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(
    phenotype = list(validation_status = "internal_validated")
  ))
  before <- yaml::read_yaml(file.path(tmp, "advocate-phenotype.yaml"))$project$updated
  crio_deposit(tmp, sandbox = TRUE)
  after <- yaml::read_yaml(file.path(tmp, "advocate-phenotype.yaml"))$project$updated
  expect_false(identical(before, after))
})

test_that("crio_deposit sandbox prints confirmation", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(
    phenotype = list(validation_status = "internal_validated")
  ))
  out <- capture.output(crio_deposit(tmp, sandbox = TRUE))
  combined <- paste(out, collapse = "\n")
  expect_true(grepl("Heart Failure with Reduced EF", combined))
  expect_true(grepl("sandbox", combined, ignore.case = TRUE))
})

test_that("crio_deposit returns FALSE for draft status (not eligible)", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)  # validation_status = "draft" → not eligible
  out <- capture.output(result <- crio_deposit(tmp, sandbox = TRUE))
  expect_false(result)
  expect_true(any(grepl("not eligible", out, ignore.case = TRUE)))
})

test_that("crio_deposit returns FALSE when already submitted", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(
    phenotype     = list(validation_status = "internal_validated"),
    contribution  = list(deposit_submitted = TRUE,
                         deposit_date      = "2026-01-01T00:00:00+00:00")
  ))
  out <- capture.output(result <- crio_deposit(tmp, sandbox = TRUE))
  expect_false(result)
  expect_true(any(grepl("already submitted", out, ignore.case = TRUE)))
})

test_that("crio_deposit aborts on invalid schema", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(phenotype = list(name = NULL)))
  out <- capture.output(result <- crio_deposit(tmp, sandbox = TRUE))
  expect_false(result)
  expect_true(any(grepl("invalid", out, ignore.case = TRUE)))
})

test_that("crio_deposit aborts when file missing", {
  tmp <- withr::local_tempdir()
  out <- capture.output(result <- crio_deposit(tmp, sandbox = TRUE))
  expect_false(result)
  expect_true(any(grepl("not found", out, ignore.case = TRUE)))
})

test_that("crio_deposit sandbox=FALSE errors when eligible", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(
    phenotype = list(validation_status = "internal_validated")
  ))
  expect_error(crio_deposit(tmp, sandbox = FALSE), "not yet configured")
})

test_that("crio_deposit peer_reviewed phenotype is also eligible", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(
    phenotype = list(validation_status = "peer_reviewed")
  ))
  result <- crio_deposit(tmp, sandbox = TRUE)
  expect_true(result)
})
