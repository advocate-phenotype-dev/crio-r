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

# ── .load_upstream ────────────────────────────────────────────────────────────

test_that(".load_upstream reads full project YAML", {
  tmp  <- withr::local_tempdir()
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
  tmp  <- withr::local_tempdir()
  uuid <- "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  write_registry(tmp, list(list(
    id = uuid, pi_name = "Dr. Test", phenotype_name = "Test",
    domain = "condition", validation_status = "draft", sce_tier = 2L
  )))

  result <- .load_upstream(uuid, file.path(tmp, "registry.yaml"))
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

# ── crio_init (non-interactive) ───────────────────────────────────────────────

test_that("crio_init creates project structure", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "project")

  result_dir <- crio_init(
    pi_name = "Dr. Test", pi_email = "test@wakehealth.edu",
    department = "Research", phenotype_name = "Heart Failure",
    domain = "condition", sce_tier = 3L, data_tier = "B",
    environment = "azure_tre", description = "Identifies HF.",
    inclusion_criteria = "EF < 40%", exclusion_criteria = "Age < 18",
    pi_orcid = "0000-0002-1234-5678", irb_number = "IRB-2026-001",
    irb_status = "active", output_dir = out
  )

  expect_true(dir.exists(result_dir))
  expect_true(file.exists(file.path(result_dir, "advocate-phenotype.yaml")))
  expect_true(file.exists(file.path(result_dir, "README.md")))
  expect_true(file.exists(file.path(result_dir, ".gitignore")))
  expect_true(dir.exists(file.path(result_dir, "src", "cohort_definition")))
  expect_true(dir.exists(file.path(result_dir, ".advocate")))
})

test_that("crio_init writes correct schema fields", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "project")

  crio_init(
    pi_name = "Dr. Test", pi_email = "test@wakehealth.edu",
    department = "Research", phenotype_name = "Heart Failure",
    domain = "condition", sce_tier = 3L, data_tier = "B",
    environment = "azure_tre", description = "Identifies HF.",
    inclusion_criteria = "EF < 40%", exclusion_criteria = "Age < 18",
    pi_orcid = "0000-0002-1234-5678", irb_number = "IRB-2026-001",
    irb_status = "active", output_dir = out
  )

  schema <- yaml::read_yaml(file.path(out, "advocate-phenotype.yaml"))
  expect_equal(schema$investigator$pi_name, "Dr. Test")
  expect_equal(schema$investigator$pi_email, "test@wakehealth.edu")
  expect_equal(schema$investigator$pi_orcid, "0000-0002-1234-5678")
  expect_equal(schema$phenotype$name, "Heart Failure")
  expect_equal(schema$phenotype$domain, "condition")
  expect_equal(schema$phenotype$version, "0.1.0")
  expect_equal(schema$phenotype$validation_status, "draft")
  expect_equal(schema$compute$sce_tier, 3L)
  expect_equal(schema$compute$data_tier, "B")
  expect_equal(schema$compute$environment, "azure_tre")
  expect_equal(schema$institution$irb_number, "IRB-2026-001")
  expect_equal(schema$institution$irb_status, "active")
  expect_false(is.null(schema$project$id))
})

test_that("crio_init derive_from writes lineage fields", {
  tmp  <- withr::local_tempdir()
  uuid <- "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  paths <- make_registry_and_project(tmp, uuid)
  out   <- file.path(tmp, "derived")

  crio_init(
    pi_name = "New Researcher", pi_email = "new@wakehealth.edu",
    department = "Research Informatics", phenotype_name = "Derived Phenotype",
    domain = "condition", sce_tier = 3L, data_tier = "B",
    environment = "azure_tre", description = "A derived version.",
    inclusion_criteria = "Patients with X",
    exclusion_criteria = "Modified exclusion",
    pi_orcid = "0000-0002-9999-8888", irb_number = "IRB-2026-002",
    irb_status = "active", output_dir = out,
    derive_from = uuid, derived_version = "1.2.0",
    derivation_rationale = "Extending to pediatrics.",
    upstream_inclusion_criteria = "Patients with X",
    upstream_exclusion_criteria = "Patients under 18"
  )

  schema <- yaml::read_yaml(file.path(out, "advocate-phenotype.yaml"))
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

test_that("crio_init no derive_from — inherited_criteria NULL", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "fresh")

  crio_init(
    pi_name = "Dr. New", pi_email = "new@wakehealth.edu",
    department = "Research", phenotype_name = "Fresh Phenotype",
    domain = "condition", sce_tier = 2L, data_tier = "A",
    environment = "gcp", description = "A fresh phenotype.",
    inclusion_criteria = "Inclusion here", exclusion_criteria = "Exclusion here",
    pi_orcid = "0000-0002-1111-2222", output_dir = out
  )

  schema <- yaml::read_yaml(file.path(out, "advocate-phenotype.yaml"))
  expect_null(schema$phenotype$inherited_criteria)
  expect_null(schema$project$derived_from)
})

test_that("crio_init staff_id used when no ORCID", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "project")

  crio_init(
    pi_name = "Dr. Staff", pi_email = "staff@wfusm.edu",
    department = "Ops", phenotype_name = "Ops Phenotype",
    domain = "observation", sce_tier = 2L, data_tier = "A",
    environment = "gcp", description = "An ops phenotype.",
    inclusion_criteria = "All patients", exclusion_criteria = "None",
    staff_id = "WF-12345", output_dir = out
  )

  schema <- yaml::read_yaml(file.path(out, "advocate-phenotype.yaml"))
  expect_equal(schema$investigator$staff_id, "WF-12345")
  expect_null(schema$investigator$pi_orcid)

  readme <- paste(readLines(file.path(out, "README.md"), warn = FALSE), collapse = "\n")
  expect_true(grepl("WF-12345", readme))
})

test_that("crio_init README contains key metadata", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "project")

  crio_init(
    pi_name = "Dr. Test", pi_email = "test@wakehealth.edu",
    department = "Research", phenotype_name = "Heart Failure",
    domain = "condition", sce_tier = 3L, data_tier = "B",
    environment = "azure_tre", description = "Identifies HF.",
    inclusion_criteria = "EF < 40%", exclusion_criteria = "Age < 18",
    pi_orcid = "0000-0002-1234-5678", output_dir = out
  )

  readme <- paste(readLines(file.path(out, "README.md"), warn = FALSE), collapse = "\n")
  expect_true(grepl("Heart Failure", readme))
  expect_true(grepl("Dr. Test", readme))
  expect_true(grepl("condition", readme))
  expect_true(grepl("0000-0002-1234-5678", readme))
})

# ── crio_init_interactive ─────────────────────────────────────────────────────

test_that("crio_init_interactive creates project via interview (tier 3 path)", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "project")

  withr::with_options(list(crio.test_responses = list(
    # .collect_investigator
    "Dr. Test",              # pi_name
    "test@wakehealth.edu",   # pi_email
    TRUE,                    # has ORCID?
    "0000-0002-1234-5678",   # pi_orcid
    # pi_role, department
    "researcher",
    "Research",
    # phenotype
    "Heart Failure",         # phenotype_name
    "condition",             # domain
    "Identifies HF.",        # description
    "EF < 40%",              # inclusion_criteria
    "Age < 18",              # exclusion_criteria
    # .infer_sce_tier: NOT identified -> YES EHR -> tier=3,B,azure,omop=TRUE
    FALSE,                   # identified records?
    TRUE,                    # EHR/OMOP?
    FALSE,                   # override?
    # .infer_irb: tier=3, is_research=TRUE
    TRUE,                    # is_research?
    "IRB-2026-001",          # irb_number
    "active",                # irb_status
    ""                       # funding_source (skip)
  )), {
    result_dir <- crio_init_interactive(output_dir = out)
  })

  expect_true(dir.exists(result_dir))
  schema <- yaml::read_yaml(file.path(result_dir, "advocate-phenotype.yaml"))
  expect_equal(schema$compute$sce_tier, 3L)
  expect_equal(schema$compute$data_tier, "B")
  expect_equal(schema$compute$environment, "azure_tre")
  expect_true(isTRUE(schema$phenotype$omop_aligned))
  expect_false(isTRUE(schema$phenotype$clarity_required))
  expect_equal(schema$institution$irb_number, "IRB-2026-001")
})

test_that("crio_init_interactive tier 2 path — no IRB prompts", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "project")

  withr::with_options(list(crio.test_responses = list(
    "Dr. Two", "two@wakehealth.edu", TRUE, "0000-0002-0000-0002",
    "researcher", "Research",
    "Tier Two Phenotype", "condition", "A phenotype.", "Includes.", "Excludes.",
    # .infer_sce_tier: NOT identified -> NOT EHR -> prompt omop -> tier=2,A,gcp
    FALSE,   # identified records?
    FALSE,   # EHR/OMOP?
    FALSE,   # OMOP aligned?
    FALSE,   # override?
    # tier=2 → no IRB section
    ""       # funding_source
  )), {
    result_dir <- crio_init_interactive(output_dir = out)
  })

  schema <- yaml::read_yaml(file.path(result_dir, "advocate-phenotype.yaml"))
  expect_equal(schema$compute$sce_tier, 2L)
  expect_equal(schema$compute$data_tier, "A")
  expect_equal(schema$compute$environment, "gcp")
  expect_null(schema$institution$irb_number)
})

test_that("crio_init_interactive tier 5 path — clarity_required TRUE", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "project")

  withr::with_options(list(crio.test_responses = list(
    "Dr. Five", "five@wakehealth.edu", TRUE, "0000-0002-0000-0005",
    "researcher", "Research",
    "Clarity Phenotype", "condition", "Uses Clarity.", "Includes.", "Excludes.",
    # .infer_sce_tier: YES identified -> YES clarity/CUI -> tier=5,D,azure
    TRUE,    # identified records?
    TRUE,    # unstructured/Clarity?
    FALSE,   # override?
    # .infer_irb: tier=5, is_research=TRUE
    TRUE,    # is_research?
    "IRB-2026-005",
    "active",
    ""       # funding_source
  )), {
    result_dir <- crio_init_interactive(output_dir = out)
  })

  schema <- yaml::read_yaml(file.path(result_dir, "advocate-phenotype.yaml"))
  expect_equal(schema$compute$sce_tier, 5L)
  expect_equal(schema$compute$data_tier, "D")
  expect_true(isTRUE(schema$phenotype$clarity_required))
  expect_false(isTRUE(schema$phenotype$omop_aligned))
})

test_that("crio_init_interactive derive_from writes lineage fields", {
  tmp  <- withr::local_tempdir()
  uuid <- "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
  paths <- make_registry_and_project(tmp, uuid)
  out   <- file.path(tmp, "derived")

  withr::with_options(list(crio.test_responses = list(
    "New Researcher", "new@wakehealth.edu", TRUE, "0000-0002-9999-8888",
    "researcher", "Research Informatics",
    "Derived Phenotype", "condition", "A derived version.",
    "Patients with X",       # inclusion (same as upstream → inherited)
    "Modified exclusion",    # exclusion (different → modified)
    # .infer_sce_tier: NOT identified -> YES EHR -> tier=3
    FALSE, TRUE, FALSE,
    # .infer_irb
    TRUE, "IRB-2026-002", "active",
    "",                      # funding_source
    "Extending to pediatrics."  # derivation_rationale
  )), {
    result_dir <- crio_init_interactive(
      output_dir    = out,
      derive_from   = uuid,
      registry_path = paths$registry_path
    )
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

test_that("crio_init_interactive tier override changes tier", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "project")

  withr::with_options(list(crio.test_responses = list(
    "Dr. Override", "override@wakehealth.edu", TRUE, "0000-0002-0000-0099",
    "researcher", "Research",
    "Override Phenotype", "condition", "Override test.", "Includes.", "Excludes.",
    # infer: NOT identified -> YES EHR -> tier=3 (inferred)
    FALSE, TRUE,
    # override=TRUE -> pick tier 4, C, azure_tre, omop=FALSE, clarity=FALSE
    TRUE,         # override?
    "4",          # SCE tier
    "C",          # data tier
    "azure_tre",  # environment
    FALSE,        # omop aligned
    FALSE,        # clarity required
    # .infer_irb: tier=4, is_research=TRUE
    TRUE, "IRB-2026-099", "active",
    ""
  )), {
    result_dir <- crio_init_interactive(output_dir = out)
  })

  schema <- yaml::read_yaml(file.path(result_dir, "advocate-phenotype.yaml"))
  expect_equal(schema$compute$sce_tier, 4L)
  expect_equal(schema$compute$data_tier, "C")
})
