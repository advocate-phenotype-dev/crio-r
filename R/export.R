.to_ohdsi_pl <- function(schema) {
  phen <- schema$phenotype    %||% list()
  inv  <- schema$investigator %||% list()
  proj <- schema$project      %||% list()

  contributors <- list(list(name = inv$pi_name, orcid = inv$pi_orcid))
  for (c in inv$contributors %||% list()) {
    contributors <- c(contributors, list(list(name = c$name, orcid = c$orcid)))
  }

  list(
    cohortDefinitionId          = as.character(proj$id),
    cohortDefinitionName        = phen$name,
    cohortDefinitionDescription = phen$description,
    contributors                = contributors,
    clinicalDescription         = phen$description,
    inclusionCriteria           = phen$inclusion_criteria,
    exclusionCriteria           = phen$exclusion_criteria,
    conceptDomain               = phen$domain,
    omopConceptIds              = phen$target_concept_ids %||% list(),
    icdCodes                    = phen$icd_codes %||% list(),
    validationStatus            = phen$validation_status,
    validationMethod            = phen$validation_method,
    ppv                         = phen$ppv,
    version                     = phen$version,
    sourceRegistry              = "Advocate Health CRIO Phenotype Library",
    sourceId                    = as.character(proj$id),
    tags                        = list(phen$domain),
    recommendedStudyApplications = list(),
    externalLinks               = list(
      phekb   = phen$phekb_id,
      ohdsi_pl = phen$ohdsi_pl_id
    ),
    createdDate = as.character(proj$created),
    updatedDate = as.character(proj$updated)
  )
}

.to_phekb <- function(schema) {
  phen <- schema$phenotype    %||% list()
  inv  <- schema$investigator %||% list()
  inst <- schema$institution  %||% list()
  proj <- schema$project      %||% list()

  list(
    title              = phen$name,
    description        = phen$description,
    inclusionCriteria  = phen$inclusion_criteria,
    exclusionCriteria  = phen$exclusion_criteria,
    dataModalities     = list("EHR"),
    icdCodes           = phen$icd_codes %||% list(),
    author             = list(
      name  = inv$pi_name,
      orcid = inv$pi_orcid,
      email = inv$pi_email
    ),
    institution        = inst$department,
    validationStatus   = phen$validation_status,
    ppv                = phen$ppv,
    version            = phen$version,
    createdDate        = as.character(proj$created)
  )
}

#' Export a phenotype schema to a standard format
#'
#' @param project_dir Path to the project directory. Defaults to `getwd()`.
#' @param target      Export format: "ohdsi_pl" (default) or "phekb".
#' @return Invisibly returns the export list; also prints as JSON.
#' @export
crio_export <- function(project_dir = NULL, target = "ohdsi_pl") {
  project_dir <- project_dir %||% getwd()
  schema_path <- file.path(project_dir, "advocate-phenotype.yaml")

  if (!file.exists(schema_path)) {
    cat(cli::col_red("Export aborted — advocate-phenotype.yaml not found.\n"))
    return(invisible(list()))
  }

  raw    <- yaml::read_yaml(schema_path)
  result <- .validate_schema(raw)

  if (!result$valid) {
    cat(cli::col_red("Export aborted — schema invalid:\n"))
    for (e in result$errors) cat(" ", e, "\n")
    return(invisible(list()))
  }

  schema <- result$schema

  output <- if (target == "ohdsi_pl") {
    .to_ohdsi_pl(schema)
  } else if (target == "phekb") {
    .to_phekb(schema)
  } else {
    stop("Unknown export target: ", target, ". Use 'ohdsi_pl' or 'phekb'.")
  }

  cat(jsonlite::toJSON(output, pretty = TRUE, auto_unbox = TRUE, null = "null"), "\n")
  invisible(output)
}
