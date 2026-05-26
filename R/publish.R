.update_timestamp <- function(schema_path) {
  raw <- yaml::read_yaml(schema_path)
  raw$project$updated <- .crio_now()
  .write_schema(raw, schema_path)
  raw
}

.regenerate_readme <- function(project_dir, schema) {
  phen <- schema$phenotype    %||% list()
  inv  <- schema$investigator %||% list()
  comp <- schema$compute      %||% list()
  writeLines(
    .readme_content(
      name               = phen$name %||% "",
      pi_name            = inv$pi_name %||% "",
      identifier         = inv$pi_orcid %||% inv$staff_id %||% "",
      pi_role            = inv$pi_role %||% "researcher",
      domain             = phen$domain %||% "",
      sce_tier           = comp$sce_tier %||% "",
      version            = phen$version %||% "",
      validation_status  = phen$validation_status %||% "",
      description        = phen$description %||% "",
      inclusion_criteria = phen$inclusion_criteria %||% "",
      exclusion_criteria = phen$exclusion_criteria %||% ""
    ),
    file.path(project_dir, "README.md")
  )
}

.update_registry <- function(library_dir, schema) {
  registry_path <- file.path(library_dir, "registry.yaml")

  registry <- if (file.exists(registry_path)) {
    yaml::read_yaml(registry_path) %||% list()
  } else {
    list()
  }

  projects   <- registry$projects %||% list()
  project_id <- schema$project$id

  entry <- list(
    id                = project_id,
    pi_orcid          = schema$investigator$pi_orcid,
    pi_name           = schema$investigator$pi_name,
    phenotype_name    = schema$phenotype$name,
    domain            = schema$phenotype$domain,
    validation_status = schema$phenotype$validation_status,
    deposit_eligible  = isTRUE(schema$contribution$deposit_eligible),
    credit_granted    = isTRUE(schema$contribution$credit_granted),
    sce_tier          = schema$compute$sce_tier,
    updated           = schema$project$updated
  )

  projects <- Filter(function(p) !identical(as.character(p$id), as.character(project_id)), projects)
  projects <- c(projects, list(entry))
  projects <- projects[order(sapply(projects, function(p) p$updated %||% ""), decreasing = TRUE)]

  updated_registry <- list(
    generated = .crio_now(),
    projects  = projects
  )

  .write_schema(updated_registry, registry_path)
}

#' Publish a phenotype project to the library
#'
#' Validates the schema, updates the timestamp, regenerates README, copies
#' project files to the library, updates registry.yaml, and commits via git.
#'
#' @param project_dir Path to the project directory.
#' @param library_dir Path to the phenotype library directory.
#' @param message     Commit message.
#' @param sandbox     If TRUE (default), skip git push.
#' @return Invisibly NULL.
#' @export
crio_publish <- function(project_dir = NULL, library_dir = NULL,
                          message = "update", sandbox = TRUE) {
  project_dir <- normalizePath(project_dir %||% getwd(), mustWork = TRUE)
  library_dir <- normalizePath(library_dir, mustWork = FALSE)
  schema_path <- file.path(project_dir, "advocate-phenotype.yaml")

  result <- .validate_schema(yaml::read_yaml(schema_path))
  if (!result$valid) {
    cat(cli::col_red("Publish aborted — schema invalid:\n"))
    for (e in result$errors) cat(" ", e, "\n")
    return(invisible(NULL))
  }

  schema     <- .update_timestamp(schema_path)
  .regenerate_readme(project_dir, schema)

  project_id <- as.character(schema$project$id)
  dest       <- normalizePath(file.path(library_dir, "projects", project_id), mustWork = FALSE)

  if (!identical(project_dir, dest)) {
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    items <- list.files(project_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    skip  <- c(".git", ".venv", "__pycache__", ".advocate")
    for (item in items) {
      base <- basename(item)
      if (base %in% skip) next
      target <- file.path(dest, base)
      if (file.info(item)$isdir) {
        if (dir.exists(target)) unlink(target, recursive = TRUE)
        file.copy(item, dest, recursive = TRUE)
      } else {
        file.copy(item, target, overwrite = TRUE)
      }
    }
  }

  .update_registry(library_dir, schema)

  # git commit
  git <- function(...) system2("git", c("-C", library_dir, ...), stdout = TRUE, stderr = TRUE)
  git("init")
  git(c("add", "-A"))

  status_out <- git(c("status", "--porcelain"))
  if (length(status_out) > 0 && any(nchar(status_out) > 0)) {
    commit_msg <- sprintf(
      "[crio] %s v%s (%s) — %s",
      schema$phenotype$name, schema$phenotype$version,
      substr(project_id, 1, 8), message
    )
    git(c("commit", "-m", commit_msg))
    cat(cli::col_green("✓"), "Committed:", message, "\n")
  } else {
    cat("Nothing to commit — no changes detected\n")
  }

  if (!sandbox) {
    git(c("push", "origin", "main"))
    cat(cli::col_green("✓"), "Pushed to remote\n")
  } else {
    cat("  Sandbox mode — skipping push\n")
    cat("  Library updated at:", library_dir, "\n")
  }

  invisible(NULL)
}
