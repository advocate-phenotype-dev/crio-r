test_that("crio_export ohdsi_pl returns correct top-level fields", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  result <- crio_export(tmp, target = "ohdsi_pl")
  expect_equal(result$cohortDefinitionName, "Heart Failure with Reduced EF")
  expect_equal(result$inclusionCriteria, "EF < 40% on echo; ICD-10 I50.x")
  expect_equal(result$exclusionCriteria, "Age < 18")
  expect_equal(result$conceptDomain, "condition")
  expect_equal(result$version, "0.1.0")
  expect_equal(result$validationStatus, "draft")
  expect_equal(result$sourceRegistry, "Advocate Health CRIO Phenotype Library")
})

test_that("crio_export ohdsi_pl includes PI as contributor", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  result <- crio_export(tmp, target = "ohdsi_pl")
  expect_length(result$contributors, 1)
  expect_equal(result$contributors[[1]]$name, "Dr. Alice Smith")
  expect_equal(result$contributors[[1]]$orcid, "0000-0002-1234-5678")
})

test_that("crio_export ohdsi_pl prints JSON to stdout", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  out <- capture.output(crio_export(tmp, target = "ohdsi_pl"))
  combined <- paste(out, collapse = "\n")
  expect_true(grepl("cohortDefinitionName", combined))
  expect_true(grepl("Heart Failure with Reduced EF", combined))
})

test_that("crio_export phekb returns correct fields", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  result <- crio_export(tmp, target = "phekb")
  expect_equal(result$title, "Heart Failure with Reduced EF")
  expect_equal(result$description, "Identifies patients with HFrEF.")
  expect_equal(result$inclusionCriteria, "EF < 40% on echo; ICD-10 I50.x")
  expect_equal(result$exclusionCriteria, "Age < 18")
  expect_equal(result$institution, "Research Informatics")
  expect_equal(result$validationStatus, "draft")
  expect_equal(result$version, "0.1.0")
})

test_that("crio_export phekb author fields are correct", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  result <- crio_export(tmp, target = "phekb")
  expect_equal(result$author$name, "Dr. Alice Smith")
  expect_equal(result$author$orcid, "0000-0002-1234-5678")
  expect_equal(result$author$email, "alice@wakehealth.edu")
})

test_that("crio_export unknown target errors", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  expect_error(crio_export(tmp, target = "unknown"), "Unknown export target")
})

test_that("crio_export aborts and returns empty list on invalid schema", {
  tmp <- withr::local_tempdir()
  write_schema(tmp, overrides = list(phenotype = list(name = NULL)))
  out <- capture.output(result <- crio_export(tmp))
  expect_equal(result, list())
  expect_true(any(grepl("invalid", out, ignore.case = TRUE)))
})

test_that("crio_export aborts and returns empty list when file missing", {
  tmp <- withr::local_tempdir()
  out <- capture.output(result <- crio_export(tmp))
  expect_equal(result, list())
  expect_true(any(grepl("not found", out, ignore.case = TRUE)))
})

test_that("crio_export ohdsi_pl sourceId matches project UUID", {
  tmp <- withr::local_tempdir()
  write_schema(tmp)
  result <- crio_export(tmp, target = "ohdsi_pl")
  schema <- yaml::read_yaml(file.path(tmp, "advocate-phenotype.yaml"))
  expect_equal(result$sourceId, schema$project$id)
  expect_equal(result$cohortDefinitionId, schema$project$id)
})
