test_that("valid schema passes", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  result <- crio_validate(tmp)
  expect_true(result$valid)
  expect_length(result$errors, 0)
})

test_that("missing required field is caught", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(phenotype = list(name = NULL)))
  result <- crio_validate(tmp)
  expect_false(result$valid)
  expect_true(any(grepl("phenotype.name", result$errors)))
})

test_that("non-institutional email is rejected", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(investigator = list(pi_email = "bad@gmail.com")))
  result <- crio_validate(tmp)
  expect_false(result$valid)
  expect_true(any(grepl("institutional", result$errors)))
})

test_that("invalid ORCID is rejected", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(investigator = list(pi_orcid = "not-an-orcid")))
  result <- crio_validate(tmp)
  expect_false(result$valid)
  expect_true(any(grepl("ORCID", result$errors)))
})

test_that("bad semver version is rejected", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(phenotype = list(version = "1.0")))
  result <- crio_validate(tmp)
  expect_false(result$valid)
  expect_true(any(grepl("semantic", result$errors)))
})

test_that("SCE tier 3 without IRB fails", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(institution = list(irb_number = NULL, irb_status = NULL)))
  result <- crio_validate(tmp)
  expect_false(result$valid)
  expect_true(any(grepl("irb", result$errors)))
})

test_that("omop_aligned=TRUE without concept IDs fails", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(phenotype = list(omop_aligned = TRUE, target_concept_ids = list())))
  result <- crio_validate(tmp)
  expect_false(result$valid)
  expect_true(any(grepl("target_concept_ids", result$errors)))
})

test_that("chart_review without ppv fails", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(phenotype = list(validation_method = "chart_review", ppv = NULL)))
  result <- crio_validate(tmp)
  expect_false(result$valid)
  expect_true(any(grepl("ppv", result$errors)))
})

test_that("deposit eligibility is computed correctly", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(
    phenotype = list(validation_status = "internal_validated")
  ))
  result <- crio_validate(tmp)
  expect_true(result$valid)
  expect_true(isTRUE(result$schema$contribution$deposit_eligible))
})

test_that("missing schema file returns error", {
  tmp <- withr::local_tempdir()
  result <- crio_validate(tmp)
  expect_false(result$valid)
  expect_true(any(grepl("not found", result$errors)))
})
