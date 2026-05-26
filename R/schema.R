SCE_TIERS     <- 1:5
DATA_TIERS    <- c("A", "B", "C", "D")
ENVIRONMENTS  <- c("azure_tre", "gcp", "local")
IRB_STATUSES  <- c("active", "exempt", "pending")
DOMAINS       <- c("condition", "drug", "procedure", "measurement", "observation")
VAL_STATUSES  <- c("draft", "internal_validated", "peer_reviewed", "deprecated")
VAL_METHODS   <- c("cohort_diagnostics", "phevaluator", "chart_review", "none")
CONTRIBUTOR_ROLES <- c("researcher", "clinical_ops", "data_scientist",
                       "quality_analyst", "informaticist")
ALLOWED_DOMAINS <- c("wakehealth.edu", "wfusm.edu", "advocatehealth.com", "advocatehealth.org")

.is_valid_orcid <- function(x) {
  grepl("^\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]$", x)
}

.is_institutional_email <- function(x) {
  domain <- tolower(sub(".*@", "", x))
  domain %in% ALLOWED_DOMAINS
}

.is_semver <- function(x) grepl("^\\d+\\.\\d+\\.\\d+$", x)

# Returns list(valid, errors, schema) where schema is the raw list on success.
.validate_schema <- function(raw) {
  errors <- character(0)

  add_err <- function(loc, msg) {
    errors <<- c(errors, paste0("[", loc, "] ", msg))
  }

  proj <- raw$project %||% list()
  inv  <- raw$investigator %||% list()
  inst <- raw$institution %||% list()
  comp <- raw$compute %||% list()
  phen <- raw$phenotype %||% list()
  contr <- raw$contribution %||% list()

  # project block
  if (is.null(proj$id))      add_err("project.id", "required")
  if (is.null(proj$created)) add_err("project.created", "required")
  if (is.null(proj$updated)) add_err("project.updated", "required")

  # investigator block
  if (is.null(inv$pi_name))  add_err("investigator.pi_name", "required")
  if (is.null(inv$pi_email)) add_err("investigator.pi_email", "required")
  if (!is.null(inv$pi_email) && !.is_institutional_email(inv$pi_email)) {
    add_err("investigator.pi_email",
            paste0("must be an institutional address: ",
                   paste(sort(ALLOWED_DOMAINS), collapse = ", ")))
  }
  if (!is.null(inv$pi_orcid) && !.is_valid_orcid(inv$pi_orcid)) {
    add_err("investigator.pi_orcid", "ORCID must match format 0000-0000-0000-0000")
  }
  if (is.null(inv$pi_orcid) && is.null(inv$staff_id)) {
    add_err("investigator", "must have either pi_orcid or staff_id")
  }

  # institution block
  if (is.null(inst$department)) add_err("institution.department", "required")
  if (!is.null(inst$irb_status) && !inst$irb_status %in% IRB_STATUSES) {
    add_err("institution.irb_status",
            paste0("must be one of: ", paste(IRB_STATUSES, collapse = ", ")))
  }

  # compute block
  if (is.null(comp$sce_tier))    add_err("compute.sce_tier", "required")
  if (is.null(comp$data_tier))   add_err("compute.data_tier", "required")
  if (is.null(comp$environment)) add_err("compute.environment", "required")
  if (!is.null(comp$sce_tier) && !comp$sce_tier %in% SCE_TIERS) {
    add_err("compute.sce_tier", paste0("must be one of: ", paste(SCE_TIERS, collapse = ", ")))
  }
  if (!is.null(comp$data_tier) && !comp$data_tier %in% DATA_TIERS) {
    add_err("compute.data_tier", paste0("must be one of: ", paste(DATA_TIERS, collapse = ", ")))
  }
  if (!is.null(comp$environment) && !comp$environment %in% ENVIRONMENTS) {
    add_err("compute.environment", paste0("must be one of: ", paste(ENVIRONMENTS, collapse = ", ")))
  }

  # phenotype block
  if (is.null(phen$name))               add_err("phenotype.name", "required")
  if (is.null(phen$version))            add_err("phenotype.version", "required")
  if (!is.null(phen$version) && !.is_semver(phen$version)) {
    add_err("phenotype.version", "must be semantic: MAJOR.MINOR.PATCH")
  }
  if (is.null(phen$domain))             add_err("phenotype.domain", "required")
  if (!is.null(phen$domain) && !phen$domain %in% DOMAINS) {
    add_err("phenotype.domain", paste0("must be one of: ", paste(DOMAINS, collapse = ", ")))
  }
  if (is.null(phen$validation_status))  add_err("phenotype.validation_status", "required")
  if (!is.null(phen$validation_status) && !phen$validation_status %in% VAL_STATUSES) {
    add_err("phenotype.validation_status",
            paste0("must be one of: ", paste(VAL_STATUSES, collapse = ", ")))
  }
  if (!is.null(phen$validation_method) && !phen$validation_method %in% VAL_METHODS) {
    add_err("phenotype.validation_method",
            paste0("must be one of: ", paste(VAL_METHODS, collapse = ", ")))
  }
  if (is.null(phen$description))        add_err("phenotype.description", "required")
  if (is.null(phen$inclusion_criteria)) add_err("phenotype.inclusion_criteria", "required")
  if (is.null(phen$exclusion_criteria)) add_err("phenotype.exclusion_criteria", "required")

  # conditional: omop_aligned requires concept IDs
  if (isTRUE(phen$omop_aligned)) {
    ids <- phen$target_concept_ids %||% list()
    if (length(ids) == 0) {
      add_err("phenotype.target_concept_ids",
              "required when omop_aligned is true")
    }
  }
  # conditional: chart_review requires ppv
  if (identical(phen$validation_method, "chart_review") && is.null(phen$ppv)) {
    add_err("phenotype.ppv", "required when validation_method is chart_review")
  }

  # cross-block: SCE tier 3+ requires IRB
  if (!is.null(comp$sce_tier) && comp$sce_tier >= 3) {
    if (is.null(inst$irb_number) || is.null(inst$irb_status)) {
      add_err("institution",
              "irb_number and irb_status required for SCE tier 3+")
    }
  }

  valid <- length(errors) == 0

  # compute deposit eligibility
  eligible <- FALSE
  if (valid || length(errors) == 0) {
    has_validated <- !is.null(phen$validation_status) &&
      phen$validation_status %in% c("internal_validated", "peer_reviewed")
    has_id <- !is.null(inv$pi_orcid) || !is.null(inv$staff_id)
    has_fields <- nchar(phen$description %||% "") > 0 &&
                  nchar(phen$inclusion_criteria %||% "") > 0 &&
                  nchar(phen$exclusion_criteria %||% "") > 0
    eligible <- has_validated && has_id && has_fields
  }

  raw$contribution$deposit_eligible <- eligible

  list(valid = valid, errors = errors, schema = if (valid) raw else NULL)
}
