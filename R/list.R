ENRICHMENT_CACHE <- new.env(parent = emptyenv())

#' Find registry.yaml in standard locations
#'
#' Search order: CRIO_LIBRARY_DIR env var, then
#' ~/Documents/code/crio-dev/phenotype-library/registry.yaml, then walk upward
#' from the working directory.
#'
#' @return Absolute path string, or NULL if not found.
find_registry <- function() {
  env_dir <- Sys.getenv("CRIO_LIBRARY_DIR", unset = NA_character_)
  if (!is.na(env_dir)) {
    p <- file.path(env_dir, "registry.yaml")
    if (file.exists(p)) return(normalizePath(p))
    return(NULL)
  }

  default <- file.path(path.expand("~"), "Documents", "code", "crio-dev",
                       "phenotype-library", "registry.yaml")
  if (file.exists(default)) return(normalizePath(default))

  current <- normalizePath(getwd())
  for (i in seq_len(12)) {
    candidate <- file.path(current, "phenotype-library", "registry.yaml")
    if (file.exists(candidate)) return(normalizePath(candidate))
    parent <- dirname(current)
    if (parent == current) break
    current <- parent
  }

  NULL
}

#' Parse registry.yaml and return the projects list
#'
#' @param path Path to registry.yaml.
#' @return List of project entry lists.
load_registry <- function(path) {
  data <- tryCatch(
    yaml::read_yaml(path),
    error = function(e) stop("Malformed registry YAML at ", path, ": ", conditionMessage(e))
  )
  if (!is.list(data)) return(list())
  projects <- data$projects %||% list()
  Filter(is.list, projects)
}

.load_enrichment <- function(registry_dir, uuid) {
  key <- paste0(registry_dir, "|", uuid)
  if (exists(key, envir = ENRICHMENT_CACHE)) return(get(key, envir = ENRICHMENT_CACHE))

  project_yaml <- file.path(registry_dir, "projects", uuid, "advocate-phenotype.yaml")
  if (!file.exists(project_yaml)) {
    assign(key, list(), envir = ENRICHMENT_CACHE)
    return(list())
  }

  result <- tryCatch({
    schema   <- yaml::read_yaml(project_yaml)
    phenotype   <- schema$phenotype    %||% list()
    project_blk <- schema$project      %||% list()
    institution <- schema$institution  %||% list()
    list(
      version      = phenotype$version,
      ppv          = phenotype$ppv,
      derived_from = project_blk$derived_from,
      irb_number   = institution$irb_number
    )
  }, error = function(e) list())

  assign(key, result, envir = ENRICHMENT_CACHE)
  result
}

#' Filter a list of project entries
#'
#' @param projects List of project entry lists from load_registry().
#' @param domain   Filter by exact domain (case-insensitive).
#' @param status   Filter by exact validation_status (case-insensitive).
#' @param pi       Substring match against pi_name (case-insensitive).
#' @param search   Substring match against phenotype_name or pi_name.
#' @return Filtered list.
apply_filters <- function(projects, domain = NULL, status = NULL, pi = NULL, search = NULL) {
  if (!is.null(domain)) {
    projects <- Filter(function(p) tolower(p$domain %||% "") == tolower(domain), projects)
  }
  if (!is.null(status)) {
    projects <- Filter(function(p) tolower(p$validation_status %||% "") == tolower(status), projects)
  }
  if (!is.null(pi)) {
    projects <- Filter(function(p) grepl(tolower(pi), tolower(p$pi_name %||% ""), fixed = TRUE), projects)
  }
  if (!is.null(search)) {
    s <- tolower(search)
    projects <- Filter(function(p) {
      grepl(s, tolower(p$phenotype_name %||% ""), fixed = TRUE) ||
      grepl(s, tolower(p$pi_name %||% ""), fixed = TRUE)
    }, projects)
  }
  projects
}

.pad_to <- function(text, width) {
  plain <- gsub("\033\\[[0-9;]*m", "", text)
  n_pad <- max(0L, width - nchar(plain))
  paste0(text, strrep(" ", n_pad))
}

#' Render a single registry card as a character string
#'
#' @param entry        Named list (one project entry from load_registry()).
#' @param idx          1-based index of this entry in the current page.
#' @param total        Total number of entries being displayed.
#' @param registry_dir Directory containing registry.yaml (for enrichment).
#' @param narrow       If TRUE, return a compact single-line format.
#' @return Character scalar.
render_card <- function(entry, idx, total, registry_dir = NULL, narrow = FALSE) {
  INNER    <- 63L
  content_w <- INNER - 2L

  if (narrow) {
    name   <- substr(entry$phenotype_name %||% "?", 1, 40)
    status <- entry$validation_status %||% "?"
    uid    <- substr(as.character(entry$id %||% ""), 1, 8)
    return(paste0(
      sprintf("  %3d. ", idx),
      cli::style_bold(name),
      "  ",
      .status_color(status),
      cli::style_dim(paste0("  ", uid, "…"))
    ))
  }

  uuid_str <- as.character(entry$id %||% "")
  enriched <- list()
  if (!is.null(registry_dir) && nchar(uuid_str) > 0) {
    enriched <- .load_enrichment(registry_dir, uuid_str)
  }

  name      <- entry$phenotype_name %||% "Unknown"
  pi_name   <- entry$pi_name %||% ""
  sce_tier  <- entry$sce_tier
  domain    <- entry$domain %||% ""
  status    <- entry$validation_status %||% ""
  deposit   <- isTRUE(entry$deposit_eligible)
  updated   <- substr(as.character(entry$updated %||% ""), 1, 10)
  version      <- enriched$version
  ppv          <- enriched$ppv
  derived_from <- enriched$derived_from

  .card_line <- function(styled) {
    plain  <- gsub("\033\\[[0-9;]*m", "", styled)
    spaces <- max(0L, content_w - nchar(plain))
    paste0("  │  ", styled, strrep(" ", spaces), "│")
  }

  # Line 1: name (left) + page position (right)
  page_str  <- paste0(idx, " / ", total)
  name_max  <- content_w - nchar(page_str) - 1L
  name_trunc <- substr(name, 1, name_max)
  l1 <- cli::style_bold(sprintf(paste0("%-", name_max, "s %s"), name_trunc, page_str))

  # Line 2: PI
  l2 <- substr(pi_name, 1, content_w)

  # Line 3: UUID · version · date
  short_uuid <- if (nchar(uuid_str) > 0) paste0(substr(uuid_str, 1, 8), "…") else "?"
  parts3 <- c(paste0("UUID: ", short_uuid))
  if (!is.null(version)) parts3 <- c(parts3, paste0("v", version))
  if (nchar(updated) > 0) parts3 <- c(parts3, updated)
  l3 <- cli::style_dim(substr(paste(parts3, collapse = "  ·  "), 1, content_w))

  # Line 4: PPV · status · SCE tier
  parts4 <- character(0)
  if (!is.null(ppv) && !is.na(ppv)) parts4 <- c(parts4, sprintf("PPV %.1f%%", ppv * 100))
  parts4 <- c(parts4, .status_color(status))
  if (!is.null(sce_tier)) parts4 <- c(parts4, paste0("SCE tier ", sce_tier))
  l4 <- paste(parts4, collapse = "  ·  ")

  # Line 5: domain · deposit_eligible [· derived from]
  dep_str  <- if (deposit) cli::col_green("true") else cli::style_dim("false")
  base5    <- paste0("domain: ", domain, "  ·  deposit_eligible: ", dep_str)
  base5_plain <- paste0("domain: ", domain, "  ·  deposit_eligible: ",
                        if (deposit) "true" else "false")
  if (!is.null(derived_from)) {
    short_df  <- paste0(substr(as.character(derived_from), 1, 8), "…")
    derived_seg <- paste0("  ·  ↳ ", short_df)
    if (nchar(base5_plain) + nchar(derived_seg) <= content_w) {
      base5 <- paste0(base5, cli::col_cyan(derived_seg))
    }
  }
  l5 <- base5

  border <- paste0("  ┌", strrep("─", INNER), "┐")
  bottom <- paste0("  └", strrep("─", INNER), "┘")

  paste(c(border, .card_line(l1), .card_line(l2), .card_line(l3),
          .card_line(l4), .card_line(l5), bottom), collapse = "\n")
}

#' Browse the phenotype registry
#'
#' @param domain    Filter by domain.
#' @param status    Filter by validation_status.
#' @param pi        Filter by PI name substring.
#' @param search    Search phenotype_name and pi_name.
#' @param page_size Number of cards per page (default 10).
#' @param no_pager  If TRUE, print all cards without interactive paging.
#' @return Invisibly returns the filtered list of project entries.
#' @export
crio_list <- function(domain = NULL, status = NULL, pi = NULL, search = NULL,
                       page_size = 10L, no_pager = FALSE) {
  reg_path <- find_registry()
  if (is.null(reg_path)) {
    stop("Registry not found. Set CRIO_LIBRARY_DIR or clone the phenotype library.")
  }

  all_projects <- load_registry(reg_path)
  registry_dir <- dirname(reg_path)

  filtered <- apply_filters(all_projects, domain = domain, status = status,
                             pi = pi, search = search)

  if (length(all_projects) == 0) {
    cat("No phenotypes found in registry.\n")
    return(invisible(list()))
  }

  if (length(filtered) == 0) {
    cat("No phenotypes match the given filters.\n")
    return(invisible(list()))
  }

  narrow <- getOption("width", 80L) < 72L

  if (no_pager) {
    for (i in seq_along(filtered)) {
      cat(render_card(filtered[[i]], i, length(filtered), registry_dir, narrow), "\n\n")
    }
    return(invisible(filtered))
  }

  page     <- 1L
  n_pages  <- max(1L, ceiling(length(filtered) / page_size))

  repeat {
    start <- (page - 1L) * page_size + 1L
    end   <- min(page * page_size, length(filtered))
    slice <- filtered[start:end]

    cat("\014")  # form feed — clear screen
    for (j in seq_along(slice)) {
      cat(render_card(slice[[j]], start + j - 1L, length(filtered), registry_dir, narrow), "\n\n")
    }

    cat(cli::style_bold(sprintf(
      "Page %d / %d  (%d phenotypes)  [n] next  [p] prev  [q] quit  [f] filter\n",
      page, n_pages, length(filtered)
    )))

    key <- tolower(trimws(.crio_prompt("")))

    if (key %in% c("q", "quit", "")) break
    if (key %in% c("n", "next")) {
      if (page < n_pages) page <- page + 1L
    } else if (key %in% c("p", "prev")) {
      if (page > 1L) page <- page - 1L
    } else if (key == "f") {
      cat("Filter options: domain, status, pi, search\n")
    } else {
      n_key <- suppressWarnings(as.integer(key))
      if (!is.na(n_key) && n_key >= 1L && n_key <= n_pages) page <- n_key
    }
  }

  invisible(filtered)
}
