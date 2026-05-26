make_registry_and_project <- function(tmp, uuid) {
  registry_path <- file.path(tmp, "registry.yaml")
  write_registry(tmp, list(list(
    id                = uuid,
    pi_name           = "Dr. Upstream",
    phenotype_name    = "Upstream Phenotype",
    domain            = "condition",
    validation_status = "internal_validated",
    sce_tier          = 3L
  )))

  project_dir <- file.path(tmp, "projects", uuid)
  dir.create(project_dir, recursive = TRUE)

  schema <- list(
    project = list(id = uuid, created = "2026-01-01T00:00:00+00:00",
                   updated = "2026-01-01T00:00:00+00:00"),
    investigator = list(pi_name = "Dr. Upstream",
                        pi_email = "upstream@advocatehealth.com"),
    institution = list(department = "Research"),
    compute = list(sce_tier = 3L, data_tier = "B", environment = "azure_tre",
                   requested_at = "2026-01-01T00:00:00+00:00"),
    phenotype = list(
      name = "Upstream Phenotype", version = "1.2.0",
      domain = "condition", omop_aligned = FALSE, clarity_required = FALSE,
      inclusion_criteria = "Patients with X", exclusion_criteria = "Patients under 18",
      validation_status = "internal_validated", ppv = 0.92,
      description = "A phenotype.", validation_method = "chart_review",
      target_concept_ids = list(), icd_codes = list()
    )
  )
  yaml_str <- gsub(":\\s*\\{\\}", ": []", yaml::as.yaml(schema, indent = 2))
  writeLines(yaml_str, file.path(project_dir, "advocate-phenotype.yaml"))

  list(registry_path = registry_path, project_dir = project_dir)
}

test_that(".load_upstream reads full project YAML", {
  tmp <- withr::local_tempdir()
  uuid <- "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  paths <- make_registry_and_project(tmp, uuid)

  result <- .load_upstream(uuid, paths$registry_path)

  expect_equal(result$phenotype_name, "Upstream Phenotype")
  expect_equal(result$version, "1.2.0")
  expect_equal(result$domain, "condition")
  expect_equal(result$sce_tier, 3L)
  expect_equal(result$data_tier, "B")
  expect_equal(result$environment, "azure_tre")
  expect_equal(result$inclusion_criteria, "Patients with X")
  expect_equal(result$ppv, 0.92, tolerance = 1e-9)
})

test_that(".load_upstream falls back to registry when project YAML absent", {
  tmp <- withr::local_tempdir()
  uuid <- "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  registry_path <- file.path(tmp, "registry.yaml")
  write_registry(tmp, list(list(
    id = uuid, pi_name = "Dr. Test", phenotype_name = "Test",
    domain = "condition", validation_status = "draft", sce_tier = 2L
  )))

  result <- .load_upstream(uuid, registry_path)
  expect_equal(result$phenotype_name, "Test")
  expect_equal(result$sce_tier, 2L)
  expect_null(result$version)
  expect_null(result$inclusion_criteria)
})

test_that(".load_upstream errors when UUID not in registry", {
  tmp <- withr::local_tempdir()
  write_registry(tmp, list(list(id = "other-uuid", phenotype_name = "Other")))
  expect_error(
    .load_upstream("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee",
                   file.path(tmp, "registry.yaml")),
    "not found in registry"
  )
})

test_that(".load_upstream errors when registry file missing", {
  expect_error(
    .load_upstream("some-uuid", "/nonexistent/registry.yaml"),
    "Registry not found"
  )
})

test_that("crio_init creates project structure", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "project")

  withr::with_options(list(crio.test_responses = list(
    "Dr. Test",           # pi_name
    "test@wakehealth.edu", # pi_email
    TRUE,                 # has ORCID
    "0000-0002-1234-5678", # pi_orcid
    "researcher",         # pi_role
    "Research",           # department
    "Heart Failure",      # phenotype_name
    "condition",          # domain
    "Identifies HF.",     # description
    "EF < 40%",           # inclusion
    "Age < 18",           # exclusion
    FALSE,                # omop_aligned
    FALSE,                # clarity_required
    "3",                  # sce_tier
    "B",                  # data_tier
    "azure_tre",          # environment
    "IRB-2026-001",       # irb_number
    "active",             # irb_status
    ""                    # funding_source (skip)
  )), {
    result_dir <- crio_init(output_dir = out)
  })

  expect_true(dir.exists(result_dir))
  expect_true(file.exists(file.path(result_dir, "advocate-phenotype.yaml")))
  expect_true(file.exists(file.path(result_dir, "README.md")))
  expect_true(file.exists(file.path(result_dir, ".gitignore")))
  expect_true(dir.exists(file.path(result_dir, "src", "cohort_definition")))
  expect_true(dir.exists(file.path(result_dir, ".advocate")))
})

test_that("crio_init derived_from writes lineage fields", {
  tmp <- withr::local_tempdir()
  uuid <- "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  paths <- make_registry_and_project(tmp, uuid)

  upstream_inc <- "Patients with X"
  upstream_exc <- "Patients under 18"
  out <- file.path(tmp, "derived_project")

  withr::with_options(list(crio.test_responses = list(
    "New Researcher",             # pi_name
    "new@wakehealth.edu",         # pi_email
    TRUE,                         # has ORCID
    "0000-0002-9999-8888",        # pi_orcid
    "researcher",                 # pi_role
    "Research Informatics",       # department
    "Derived Phenotype",          # phenotype_name
    "condition",                  # domain
    "A derived version.",         # description
    upstream_inc,                 # inclusion (same as upstream -> "inherited")
    "Modified exclusion",         # exclusion (different -> "modified")
    FALSE,                        # omop_aligned
    FALSE,                        # clarity_required
    "3",                          # sce_tier
    "B",                          # data_tier
    "azure_tre",                  # environment
    "IRB-2026-002",               # irb_number
    "active",                     # irb_status
    "",                           # funding_source
    "Extending to pediatrics."    # derivation_rationale
  )), {
    result_dir <- crio_init(output_dir = out, derive_from = uuid,
                            registry_path = paths$registry_path)
  })

  schema <- yaml::read_yaml(file.path(result_dir, "advocate-phenotype.yaml"))

  expect_equal(schema$project$derived_from, uuid)
  expect_equal(schema$project$derived_version, "1.2.0")
  expect_equal(schema$project$derivation_rationale, "Extending to pediatrics.")

  inherited <- schema$phenotype$inherited_criteria
  expect_equal(length(inherited), 2)

  inc_entry <- Filter(function(x) x$field == "inclusion_criteria", inherited)[[1]]
  exc_entry <- Filter(function(x) x$field == "exclusion_criteria", inherited)[[1]]
  expect_equal(inc_entry$status, "inherited")
  expect_equal(exc_entry$status, "modified")
})

test_that("crio_init new project inherited_criteria is NULL when no derive_from", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "fresh")

  withr::with_options(list(crio.test_responses = list(
    "Dr. New",
    "new@wakehealth.edu",
    TRUE,
    "0000-0002-1111-2222",
    "researcher",
    "Research",
    "Fresh Phenotype",
    "condition",
    "A fresh phenotype.",
    "Inclusion here",
    "Exclusion here",
    FALSE,
    FALSE,
    "2",  # tier 2 — no IRB prompts
    "A",
    "local",
    ""
  )), {
    result_dir <- crio_init(output_dir = out)
  })

  schema <- yaml::read_yaml(file.path(result_dir, "advocate-phenotype.yaml"))
  expect_null(schema$phenotype$inherited_criteria)
  expect_null(schema$project$derived_from)
})
