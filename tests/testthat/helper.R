library(yaml)
library(withr)

# Write a minimal valid advocate-phenotype.yaml to a temp directory.
write_schema <- function(dir, overrides = list()) {
  schema <- list(
    project = list(
      id      = "aaaaaaaa-1111-2222-3333-bbbbbbbbbbbb",
      created = "2026-01-01T00:00:00+00:00",
      updated = "2026-01-01T00:00:00+00:00"
    ),
    investigator = list(
      pi_name  = "Dr. Alice Smith",
      pi_email = "alice@wakehealth.edu",
      pi_role  = "researcher",
      pi_orcid = "0000-0002-1234-5678",
      staff_id = NULL,
      contributors = list()
    ),
    institution = list(
      department = "Research Informatics",
      irb_number = "IRB-2026-001",
      irb_status = "active",
      funding_source = NULL
    ),
    compute = list(
      sce_tier     = 3L,
      data_tier    = "B",
      environment  = "azure_tre",
      requested_at = "2026-01-01T00:00:00+00:00",
      approved_at  = NULL,
      expires_at   = NULL
    ),
    phenotype = list(
      name               = "Heart Failure with Reduced EF",
      version            = "0.1.0",
      domain             = "condition",
      omop_aligned       = FALSE,
      clarity_required   = FALSE,
      target_concept_ids = list(),
      icd_codes          = list(),
      description        = "Identifies patients with HFrEF.",
      inclusion_criteria = "EF < 40% on echo; ICD-10 I50.x",
      exclusion_criteria = "Age < 18",
      validation_status  = "draft",
      validation_method  = "none",
      ppv                = NULL,
      phekb_id           = NULL,
      ohdsi_pl_id        = NULL,
      inherited_criteria = NULL
    ),
    outputs = list(
      reusable_assets     = list(),
      publications        = list(),
      downstream_projects = list()
    ),
    contribution = list(
      deposit_eligible  = FALSE,
      deposit_submitted = FALSE,
      deposit_date      = NULL,
      credit_granted    = FALSE
    )
  )

  # Apply overrides (top-level or nested)
  for (nm in names(overrides)) {
    if (is.list(overrides[[nm]]) && is.list(schema[[nm]])) {
      schema[[nm]] <- modifyList(schema[[nm]], overrides[[nm]])
    } else {
      schema[[nm]] <- overrides[[nm]]
    }
  }

  yaml_str <- yaml::as.yaml(schema, indent = 2)
  yaml_str <- gsub(":\\s*\\{\\}", ": []", yaml_str)
  writeLines(yaml_str, file.path(dir, "advocate-phenotype.yaml"))
  invisible(schema)
}

write_registry <- function(dir, entries) {
  registry <- list(
    generated = "2026-05-23T00:00:00+00:00",
    projects  = entries
  )
  yaml_str <- yaml::as.yaml(registry, indent = 2)
  yaml_str <- gsub(":\\s*\\{\\}", ": []", yaml_str)
  writeLines(yaml_str, file.path(dir, "registry.yaml"))
  invisible(registry)
}

SAMPLE_PROJECTS <- list(
  list(
    id                = "aaaaaaaa-1111-2222-3333-bbbbbbbbbbbb",
    pi_name           = "Dr. Alice Smith",
    phenotype_name    = "Acute MI Phenotype",
    domain            = "condition",
    validation_status = "internal_validated",
    deposit_eligible  = TRUE,
    credit_granted    = FALSE,
    sce_tier          = 3L,
    updated           = "2026-05-01T00:00:00+00:00"
  ),
  list(
    id                = "cccccccc-4444-5555-6666-dddddddddddd",
    pi_name           = "Dr. Bob Jones",
    phenotype_name    = "Type 2 Diabetes Screening",
    domain            = "condition",
    validation_status = "draft",
    deposit_eligible  = FALSE,
    credit_granted    = FALSE,
    sce_tier          = 2L,
    updated           = "2026-04-15T00:00:00+00:00"
  ),
  list(
    id                = "eeeeeeee-7777-8888-9999-ffffffffffff",
    pi_name           = "Dr. Alice Smith",
    phenotype_name    = "Metformin Adherence Study",
    domain            = "drug",
    validation_status = "internal_validated",
    deposit_eligible  = TRUE,
    credit_granted    = TRUE,
    sce_tier          = 3L,
    updated           = "2026-03-20T00:00:00+00:00"
  )
)
