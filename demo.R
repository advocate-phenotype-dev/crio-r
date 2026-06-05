################################################################################
#  crio-r  —  package demo
#
#  Walks through the full phenotype lifecycle:
#    1. Initialize a project (non-interactive)
#    2. Start a session
#    3. Validate the schema
#    4. Export to external registries (OHDSI PL, PheKB)
#    5. Publish to a local phenotype library
#    6. Browse the library
#    7. Derive a child phenotype
#    8. Deposit to the clearinghouse (sandbox)
#
#  All work is done in temp directories so you can run this script
#  repeatedly without side-effects.
################################################################################

# Install from GitHub if needed:
# remotes::install_github("advocate-phenotype-dev/crio-r")

library(crio)

cat("\n========== crio-r demo ==========\n\n")

# ---------------------------------------------------------------------------
# 0. Interactive init with Azure SSO (optional — requires az login)
# ---------------------------------------------------------------------------
# crio_init_interactive() runs a guided interview. When an active Azure CLI
# session is present it resolves the signed-in user's display name and
# institutional email automatically, pre-filling those prompts so the
# researcher only has to hit Enter to accept:
#
#   ── Investigator ─────────────────────────────────────
#     Resolved from Azure SSO: Huang, Erich S <Erich.Huang@Advocatehealth.org>
#     PI full name [Huang, Erich S]:
#     Institutional email [Erich.Huang@Advocatehealth.org]:
#
# Uncomment to try it (runs interactively — cannot be scripted):
# crio_init_interactive(output_dir = file.path(tempdir(), "my-phenotype"))

# ---------------------------------------------------------------------------
# 1. Initialize a phenotype project (non-interactive)
# ---------------------------------------------------------------------------
cat("── 1. crio_init() ──────────────────────────────────────────────────────\n")

project_dir <- file.path(tempdir(), "heart-failure-phenotype")
dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)

project_dir <- crio_init(
  pi_name            = "Sarah Reyes",
  pi_email           = "sreyes@wakehealth.edu",
  pi_orcid           = "0000-0002-1234-5678",
  department         = "Cardiology",
  phenotype_name     = "Heart Failure with Reduced Ejection Fraction",
  domain             = "condition",
  sce_tier           = 3L,
  data_tier          = "B",
  environment        = "azure_tre",
  description        = paste(
    "OMOP-aligned phenotype for heart failure with reduced ejection fraction",
    "(HFrEF, EF < 40%) in adults. Includes incident and prevalent cases",
    "identified from structured EHR data."
  ),
  inclusion_criteria = paste(
    "Age >= 18 at index date.",
    "At least one OMOP condition occurrence for HF (concept set below).",
    "Left ventricular ejection fraction < 40% on echocardiography or",
    "equivalent imaging within 12 months of the index HF diagnosis."
  ),
  exclusion_criteria = paste(
    "Congenital heart disease recorded prior to age 18.",
    "Valvular disease as primary etiology (TAVR / MVR within 6 months",
    "prior to index date).",
    "Transient HF in the setting of acute MI without prior HF diagnosis."
  ),
  omop_aligned       = TRUE,
  clarity_required   = FALSE,
  irb_number         = "IRB-2024-0312",
  irb_status         = "active",
  funding_source     = "NHLBI R01 HL123456",
  output_dir         = project_dir
)

# crio_init() scaffolds the schema but does not collect concept IDs interactively.
# Add them directly to the YAML before the next step.
cat("\nAdding OMOP concept IDs to advocate-phenotype.yaml:\n")
schema_path <- file.path(project_dir, "advocate-phenotype.yaml")
raw <- yaml::read_yaml(schema_path)
raw$phenotype$target_concept_ids <- list(
  316139L,   # Heart failure
  4229440L,  # Chronic systolic heart failure
  4215218L,  # Acute systolic heart failure
  46273022L, # Heart failure with reduced ejection fraction
  44782429L  # Acute-on-chronic systolic heart failure
)
raw$phenotype$icd_codes <- list("I50.20", "I50.22", "I50.42", "I50.82")
yaml::write_yaml(raw, schema_path)
cat("  target_concept_ids:",
    paste(unlist(raw$phenotype$target_concept_ids), collapse = ", "), "\n")
cat("  icd_codes:",
    paste(unlist(raw$phenotype$icd_codes), collapse = ", "), "\n")

cat("\nFiles created:\n")
invisible(lapply(
  list.files(project_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
  function(f) cat("  ", f, "\n")
))

# ---------------------------------------------------------------------------
# 2. Start a session
# ---------------------------------------------------------------------------
cat("\n── 2. crio_source() ─────────────────────────────────────────────────────\n")

# Sandbox mode: uses a mock token — no Azure login required.
session <- crio_source(project_dir, sandbox = TRUE)

cat("\nSession environment variables:\n")
cat("  CRIO_PROJECT_ID :", Sys.getenv("CRIO_PROJECT_ID"), "\n")
cat("  CRIO_SCE_TIER   :", Sys.getenv("CRIO_SCE_TIER"),   "\n")
cat("  CRIO_ENVIRONMENT:", Sys.getenv("CRIO_ENVIRONMENT"), "\n")
cat("  CRIO_SANDBOX    :", Sys.getenv("CRIO_SANDBOX"),     "\n")

# Production mode: resolves a real Bearer token from the active Azure CLI
# session (`az login`). Writes mode="production" + token + expires_at to
# .advocate/session.lock.
tryCatch({
  cat("\nProduction session (az login detected):\n")
  crio_source(project_dir, sandbox = FALSE)
}, error = function(e) {
  cat("\nSkipping production session —", conditionMessage(e), "\n")
})

# ---------------------------------------------------------------------------
# 3. Validate the schema
# ---------------------------------------------------------------------------
cat("\n── 3. crio_validate() ───────────────────────────────────────────────────\n")

result <- crio_validate(project_dir)

# Demonstrate that validation catches errors — temporarily break the schema
cat("\nManually introducing a bad email to show validation errors:\n")
raw <- yaml::read_yaml(schema_path)
raw$investigator$pi_email <- "sreyes@gmail.com"   # not institutional
yaml::write_yaml(raw, schema_path)

bad_result <- crio_validate(project_dir)

# Restore the correct email
raw$investigator$pi_email <- "sreyes@wakehealth.edu"
yaml::write_yaml(raw, schema_path)
cat("(Email restored)\n")

# ---------------------------------------------------------------------------
# 4. Export to external registries
# ---------------------------------------------------------------------------
cat("\n── 4. crio_export() ─────────────────────────────────────────────────────\n")

cat("\n--- OHDSI Phenotype Library format ---\n")
ohdsi_export <- crio_export(project_dir, target = "ohdsi_pl")

cat("\n--- PheKB format ---\n")
phekb_export <- crio_export(project_dir, target = "phekb")

# ---------------------------------------------------------------------------
# 5. Publish to a local phenotype library
# ---------------------------------------------------------------------------
cat("\n── 5. crio_publish() ────────────────────────────────────────────────────\n")

library_dir <- file.path(tempdir(), "phenotype-library")
dir.create(library_dir, recursive = TRUE, showWarnings = FALSE)

crio_publish(
  project_dir = project_dir,
  library_dir = library_dir,
  message     = "initial submission",
  sandbox     = TRUE
)

cat("\nLibrary contents:\n")
invisible(lapply(
  list.files(library_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE),
  function(f) cat("  ", f, "\n")
))

# ---------------------------------------------------------------------------
# 6. Browse the library (non-interactive, no_pager = TRUE)
# ---------------------------------------------------------------------------
cat("\n── 6. crio_list() ───────────────────────────────────────────────────────\n")

Sys.setenv(CRIO_LIBRARY_DIR = library_dir)

cat("All phenotypes:\n")
entries <- crio_list(no_pager = TRUE)

# Filter by domain
cat("Condition-domain phenotypes only:\n")
crio_list(domain = "condition", no_pager = TRUE)

# Filter by PI name
cat("Phenotypes by Reyes:\n")
crio_list(pi = "reyes", no_pager = TRUE)

# ---------------------------------------------------------------------------
# 7. Derive a child phenotype
# ---------------------------------------------------------------------------
cat("\n── 7. Derived phenotype (crio_init with derive_from) ───────────────────\n")

# Get the UUID of the parent from the registry
registry    <- yaml::read_yaml(file.path(library_dir, "registry.yaml"))
parent_uuid <- registry$projects[[1]]$id
cat("Parent UUID:", parent_uuid, "\n\n")

child_dir <- file.path(tempdir(), "hfref-ischemic-phenotype")
dir.create(child_dir, recursive = TRUE, showWarnings = FALSE)

child_dir <- crio_init(
  pi_name            = "Marcus Webb",
  pi_email           = "mwebb@wakehealth.edu",
  staff_id           = "WF-88821",           # no ORCID — uses staff ID
  department         = "Cardiology",
  phenotype_name     = "Ischemic HFrEF",
  domain             = "condition",
  sce_tier           = 3L,
  data_tier          = "B",
  environment        = "azure_tre",
  description        = paste(
    "Subset of HFrEF restricted to ischemic etiology (prior MI or",
    "ischemic cardiomyopathy recorded within 24 months of index HF date)."
  ),
  inclusion_criteria = paste(
    "Meets all Reyes HFrEF inclusion criteria.",
    "Additionally: OMOP condition occurrence for MI or ischemic",
    "cardiomyopathy within 24 months prior to or at index HF date."
  ),
  exclusion_criteria = paste(
    "Meets all Reyes HFrEF exclusion criteria.",
    "Non-ischemic etiology documented by treating cardiologist."
  ),
  omop_aligned       = TRUE,
  clarity_required   = FALSE,
  irb_number         = "IRB-2024-0312",    # same IRB protocol
  irb_status         = "active",
  output_dir         = child_dir,
  derive_from        = parent_uuid,
  derived_version    = "0.1.0",
  derivation_rationale = paste(
    "Restricts Reyes et al. HFrEF cohort to ischemic subtype for",
    "evaluation of SGLT2i outcomes in ischemic HF."
  ),
  upstream_inclusion_criteria = paste(
    "Age >= 18 at index date.",
    "At least one OMOP condition occurrence for HF (concept set below).",
    "Left ventricular ejection fraction < 40% on echocardiography or",
    "equivalent imaging within 12 months of the index HF diagnosis."
  )
)

# Add concept IDs for the child (inherits parent set, adds ischemic MI codes)
child_schema_path <- file.path(child_dir, "advocate-phenotype.yaml")
child_raw <- yaml::read_yaml(child_schema_path)
child_raw$phenotype$target_concept_ids <- list(
  316139L,   # Heart failure
  4229440L,  # Chronic systolic heart failure
  46273022L, # Heart failure with reduced ejection fraction
  4329847L,  # Myocardial infarction
  4108832L   # Ischemic cardiomyopathy
)
child_raw$phenotype$icd_codes <- list("I50.20", "I50.22", "I25.5", "I21.9")
yaml::write_yaml(child_raw, child_schema_path)

crio_validate(child_dir)

# ---------------------------------------------------------------------------
# 8. Deposit to the clearinghouse (requires validated status — show both paths)
# ---------------------------------------------------------------------------
cat("\n── 8. crio_deposit() ────────────────────────────────────────────────────\n")

cat("Attempting deposit in 'draft' status (should be ineligible):\n")
crio_deposit(project_dir, sandbox = TRUE)

cat("\nUpdating validation_status to 'internal_validated' to unlock deposit:\n")
raw <- yaml::read_yaml(schema_path)
raw$phenotype$validation_status  <- "internal_validated"
raw$phenotype$validation_method  <- "chart_review"
raw$phenotype$ppv                <- 0.91
yaml::write_yaml(raw, schema_path)

cat("\nRe-validating after status update:\n")
# Note: crio_validate() displays deposit_eligible from the on-disk YAML, which
# was initialized to FALSE by crio_init(). crio_deposit() recomputes eligibility
# fresh and will correctly find this phenotype eligible.
crio_validate(project_dir)

cat("\nDepositing:\n")
crio_deposit(project_dir, sandbox = TRUE)

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------
cat("\n========== demo complete ==========\n\n")
cat("Temp project:  ", project_dir,  "\n")
cat("Temp library:  ", library_dir,  "\n")
cat("Child project: ", child_dir,    "\n\n")
cat("Call unlink(c(project_dir, library_dir, child_dir), recursive = TRUE)",
    "to clean up.\n\n")
