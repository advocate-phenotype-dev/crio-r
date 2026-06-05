# CARDINAL-2 NSTEMI Eligibility — From Registry to Enrollment Projection
# NCCT Clinical Trials Methods Center · Derived from RADAR Phenotype Library
# Dr. Amara Nwosu, NCCT Clinical Trials Methods Center
#
# Run interactively or with: Rscript ncct_r_demo.R

# ── Preflight: install missing packages ───────────────────────────────────────
# pak ships with RStudio / renv toolchains; only installs what's missing.
needed  <- c("duckdb", "DBI", "dplyr", "ggplot2")
missing <- needed[!vapply(needed, requireNamespace, logical(1L), quietly = TRUE)]
if (length(missing)) pak::pkg_install(missing, ask = FALSE)

# ── Section 1: Setup ──────────────────────────────────────────────────────────
# Dr. Amara Nwosu is developing CARDINAL-2 eligibility screening criteria.
# Before writing any SQL, she searches the institutional phenotype registry.

pkgload::load_all("~/Documents/code/crio-dev/crio-r", quiet = TRUE)

library(DBI)
library(duckdb)
library(yaml)
library(dplyr)
library(ggplot2)

# Base-R label wrapper for ggplot axis text
wrap_label <- function(width = 18) {
  function(x) vapply(x, function(s) paste(strwrap(s, width), collapse = "\n"),
                     character(1L))
}

DB_PATH <- path.expand("~/Documents/code/crio-dev/demo/mock_nexus.duckdb")

# Build the database inline if it does not exist.
# Schema: five Advocate / Atrium sites, OMOP concept 314666 (NSTEMI),
# troponin concept 3016502, ULN 34 ng/L, ~71% troponin coverage.
if (!file.exists(DB_PATH)) {
  message("mock_nexus.duckdb not found — building from scratch …")

  con_build <- dbConnect(duckdb(), DB_PATH)

  dbExecute(con_build, "
    CREATE TABLE care_site (
      care_site_id      INTEGER PRIMARY KEY,
      care_site_name    VARCHAR,
      place_of_service_concept_id INTEGER DEFAULT 8717
    )")

  dbExecute(con_build, "
    CREATE TABLE person (
      person_id              INTEGER PRIMARY KEY,
      gender_concept_id      INTEGER,
      year_of_birth          INTEGER,
      month_of_birth         INTEGER,
      race_concept_id        INTEGER,
      ethnicity_concept_id   INTEGER,
      care_site_id           INTEGER,
      person_source_value    VARCHAR
    )")

  dbExecute(con_build, "
    CREATE TABLE condition_occurrence (
      condition_occurrence_id   INTEGER PRIMARY KEY,
      person_id                 INTEGER,
      condition_concept_id      INTEGER,
      condition_start_date      DATE,
      condition_start_datetime  TIMESTAMP,
      condition_end_date        DATE,
      condition_type_concept_id INTEGER,
      condition_source_value    VARCHAR,
      condition_source_concept_id INTEGER,
      visit_occurrence_id       INTEGER
    )")

  dbExecute(con_build, "
    CREATE TABLE measurement (
      measurement_id              INTEGER PRIMARY KEY,
      person_id                   INTEGER,
      measurement_concept_id      INTEGER,
      measurement_date            DATE,
      measurement_datetime        TIMESTAMP,
      measurement_type_concept_id INTEGER,
      value_as_number             DOUBLE,
      unit_concept_id             INTEGER,
      range_low                   DOUBLE,
      range_high                  DOUBLE,
      measurement_source_value    VARCHAR,
      visit_occurrence_id         INTEGER
    )")

  SITES <- data.frame(
    site_id    = 1:5,
    site_name  = c(
      "Wake Forest Baptist Medical Center",
      "Advocate Christ Medical Center",
      "Advocate Lutheran General",
      "Atrium Health Carolinas Medical Center",
      "Atrium Health Pineville"
    ),
    n_patients = c(180L, 140L, 95L, 110L, 65L)
  )

  for (i in seq_len(nrow(SITES))) {
    dbExecute(con_build,
      "INSERT INTO care_site VALUES (?, ?, 8717)",
      list(SITES$site_id[i], SITES$site_name[i]))
  }

  set.seed(42L)
  NSTEMI_CONCEPT   <- 314666L
  TROPONIN_CONCEPT <- 3016502L
  TROPONIN_UNIT    <- 9029L
  TROPONIN_ULN     <- 34.0
  COHORT_START     <- as.Date("2023-01-01")
  DATE_RANGE       <- as.integer(as.Date("2024-12-31") - COHORT_START)

  person_id <- 1L; condition_id <- 1L; meas_id <- 1L; visit_id <- 1L

  for (i in seq_len(nrow(SITES))) {
    n_pts  <- SITES$n_patients[i]
    n_trop <- round(n_pts * 0.71)
    for (j in seq_len(n_pts)) {
      yob      <- sample(1940:1968, 1)
      admit_dt <- COHORT_START + sample(0:DATE_RANGE, 1)
      disch_dt <- admit_dt + sample(3:8, 1)

      dbExecute(con_build,
        "INSERT INTO person VALUES (?,?,?,?,8527,38003564,?,?)",
        list(person_id, ifelse(runif(1) < .61, 8507L, 8532L),
             yob, sample(1:12, 1), SITES$site_id[i],
             sprintf("SITE%d-%06d", SITES$site_id[i], person_id)))

      dbExecute(con_build,
        "INSERT INTO condition_occurrence VALUES (?,?,?,?,?,?,32817,'I21.4',45889742,?)",
        list(condition_id, person_id, NSTEMI_CONCEPT,
             format(admit_dt), format(admit_dt, "%Y-%m-%d 08:00:00"),
             format(disch_dt), visit_id))

      if (j <= n_trop) {
        trop_val <- if (runif(1) < .12) runif(1, 500, 4000) else runif(1, 70, 480)
        dbExecute(con_build,
          "INSERT INTO measurement VALUES (?,?,?,?,?,32856,?,?,0.0,?,?,?)",
          list(meas_id, person_id, TROPONIN_CONCEPT,
               format(admit_dt), format(admit_dt, "%Y-%m-%d 06:00:00"),
               trop_val, TROPONIN_UNIT, TROPONIN_ULN,
               "LOINC:10839-9", visit_id))
        meas_id <- meas_id + 1L
      }

      person_id <- person_id + 1L; condition_id <- condition_id + 1L
      visit_id  <- visit_id  + 1L
    }
  }

  dbDisconnect(con_build, shutdown = TRUE)
  message("Database built: ", DB_PATH)
}

con <- dbConnect(duckdb(), DB_PATH, read_only = TRUE)

cat("Connected to mock_nexus.duckdb\n")
cat("Tables:", paste(dbListTables(con), collapse = ", "), "\n")
cat("Persons:", dbGetQuery(con, "SELECT COUNT(*) AS n FROM person")$n, "\n")
cat("NSTEMI occurrences:",
    dbGetQuery(con, "SELECT COUNT(*) AS n FROM condition_occurrence
                     WHERE condition_concept_id = 314666")$n, "\n")
cat("Troponin results:",
    dbGetQuery(con, "SELECT COUNT(*) AS n FROM measurement
                     WHERE measurement_concept_id = 3016502")$n, "\n")

# ── Section 2: Finding prior work ─────────────────────────────────────────────
# Before building anything, Amara searches the institutional registry.
# Two validated NSTEMI definitions exist: RADAR's RWE incident NSTEMI and
# Maya Chen's quality measure. She will derive from RADAR's.

reg_path <- find_registry()
cat("\nRegistry:", reg_path, "\n")

all_projects <- load_registry(reg_path)
cat("Total phenotypes in registry:", length(all_projects), "\n")

nstemi_all <- apply_filters(all_projects, domain = "condition", search = "nstemi")

nstemi_validated <- Filter(
  function(p) (p$validation_status %||% "") %in% c("internal_validated", "peer_reviewed"),
  nstemi_all
)

cat("NSTEMI phenotypes (validated):", length(nstemi_validated), "\n\n")

reg_dir <- dirname(reg_path)

enrich_project <- function(uuid) {
  p <- file.path(reg_dir, "projects", uuid, "advocate-phenotype.yaml")
  if (!file.exists(p)) return(list(version = NA_character_, ppv = NA_real_,
                                    derived_from = NA_character_))
  s <- yaml::read_yaml(p)
  list(
    version      = s$phenotype$version    %||% NA_character_,
    ppv          = s$phenotype$ppv,
    derived_from = s$project$derived_from %||% NA_character_
  )
}

nstemi_rows <- lapply(nstemi_validated, function(p) {
  e <- enrich_project(as.character(p$id %||% ""))
  data.frame(
    phenotype_name    = p$phenotype_name    %||% NA,
    pi_name           = p$pi_name           %||% NA,
    version           = e$version           %||% NA,
    ppv               = if (!is.null(e$ppv) && !is.na(e$ppv))
                          sprintf("%.1f%%", e$ppv * 100) else "—",
    validation_status = p$validation_status %||% NA,
    derived_from      = if (!is.null(e$derived_from) && !is.na(e$derived_from))
                          paste0(substr(e$derived_from, 1, 8), "…") else "—",
    stringsAsFactors  = FALSE
  )
})

nstemi_df <- do.call(rbind, nstemi_rows)

nstemi_display <- nstemi_df[!duplicated(
  paste(nstemi_df$pi_name, nstemi_df$phenotype_name)
), ]
rownames(nstemi_display) <- NULL
names(nstemi_display)    <- c("Phenotype", "PI", "Version", "PPV",
                               "Validation Status", "Derived From")

cat("── Registry: validated NSTEMI definitions ──────────────────\n")
print(nstemi_display)

# ── Section 3: Reviewing the upstream definition ───────────────────────────────
# Amara reads the full RADAR phenotype schema: what she inherits and what
# she must change (365-day washout → removed; troponin 24h → 72h;
# TIMI ≥ 4 and hsCRP > 2 mg/L added per CARDINAL-2 protocol).

RADAR_UUID <- "f3d7c2a1-5b8e-4f9d-a2c1-7e6f0b4d3a8c"
radar_path <- file.path(reg_dir, "projects", RADAR_UUID, "advocate-phenotype.yaml")
radar      <- yaml::read_yaml(radar_path)

cat("\n── Phenotype ──────────────────────────────────────────────\n")
cat("Name:              ", radar$phenotype$name, "\n")
cat("Version:           ", radar$phenotype$version, "\n")
cat("Domain:            ", radar$phenotype$domain, "\n")
cat("Validation status: ", radar$phenotype$validation_status, "\n")
cat("Validation method: ", radar$phenotype$validation_method, "\n")
cat("PPV:               ", sprintf("%.1f%%", radar$phenotype$ppv * 100), "\n")
cat("OMOP concept(s):   ", paste(unlist(radar$phenotype$target_concept_ids),
                                  collapse = ", "), "\n")

cat("\n── Investigator ───────────────────────────────────────────\n")
cat("PI:                ", radar$investigator$pi_name, "\n")
cat("ORCID:             ", radar$investigator$pi_orcid, "\n")
cat("Department:        ", radar$institution$department, "\n")
cat("IRB:               ", radar$institution$irb_number, " (",
    radar$institution$irb_status, ")\n", sep = "")
cat("Funding:           ", radar$institution$funding_source, "\n")

cat("\n── Compute ────────────────────────────────────────────────\n")
cat("SCE tier:          ", radar$compute$sce_tier, "\n")
cat("Data tier:         ", radar$compute$data_tier, "\n")
cat("Environment:       ", radar$compute$environment, "\n")

cat("\n── Derived-from chain ─────────────────────────────────────\n")
cat("Derived from:      ",
    radar$project$derived_from %||% "(none — this is a root definition)", "\n")

cat("\n── Clinical description ───────────────────────────────────\n")
cat(radar$phenotype$description, "\n")

cat("\n── Inclusion criteria ─────────────────────────────────────\n")
cat(radar$phenotype$inclusion_criteria, "\n")

cat("\n── Exclusion criteria ─────────────────────────────────────\n")
cat(radar$phenotype$exclusion_criteria, "\n")

# ── Section 4: Initialize the derived project ─────────────────────────────────
# crio_init() supports derive_from natively. Amara passes the RADAR UUID;
# the package generates a new UUID, records the derivation chain, and computes
# inherited vs. modified status for each criteria field.

RADAR_VERSION <- radar$phenotype$version
CARDINAL_DIR  <- path.expand("~/Documents/code/crio-dev/demo/cardinal2_nstemi")

if (dir.exists(CARDINAL_DIR)) unlink(CARDINAL_DIR, recursive = TRUE)

INCLUSION <- paste(
  "1. Condition occurrence: OMOP concept 314666 (NSTEMI), ICD-10 I21.4;",
  "   includeDescendants = true, includeMapped = true.",
  "2. Encounter setting: inpatient or emergency department",
  "   (condition_type_concept_id IN 32817, 32020).",
  "3. High-sensitivity cardiac troponin I (LOINC 10839-9, OMOP concept 3016502)",
  "   > 2x ULN (ULN = 34 ng/L; threshold = 68 ng/L) within 72 HOURS of",
  "   index condition_start_date (extended from RADAR 24h for late presenters).",
  "4. TIMI Risk Score >= 4 at time of index presentation (per CARDINAL-2 protocol).",
  "5. hsCRP > 2 mg/L within 48h of index date (per CARDINAL-2 protocol).",
  "6. Index event on or after 2024-01-01.",
  sep = "\n"
)

EXCLUSION <- paste(
  "1. Concurrent STEMI (ICD-10 I21.0-I21.3, OMOP concept 312327)",
  "   within +/-1 day of index event.",
  "2. Type 2 MI (demand ischemia, ICD-10 I21.A1) by provider documentation.",
  "3. Troponin not documented within 72h of index encounter.",
  "4. TIMI Risk Score < 4 (does not meet protocol risk threshold).",
  "5. hsCRP not documented or <= 2 mg/L within 48h of index.",
  "6. Prior enrollment in another Advocate / Atrium interventional trial",
  "   within 90 days of index date.",
  sep = "\n"
)

crio_init(
  pi_name              = "Dr. Amara Nwosu",
  pi_email             = "anwosu@wakehealth.edu",
  pi_orcid             = "0000-0002-4817-3965",
  department           = "NCCT Clinical Trials Methods Center",
  phenotype_name       = "Incident NSTEMI — CARDINAL-2 Trial Eligibility",
  domain               = "condition",
  sce_tier             = 4L,
  data_tier            = "C",
  environment          = "azure_tre",
  omop_aligned         = TRUE,
  clarity_required     = FALSE,
  irb_number           = "IRB-2026-CARDINAL2",
  irb_status           = "active",
  description          = paste(
    "Trial eligibility phenotype for the CARDINAL-2 randomized controlled trial.",
    "Derived from RADAR's validated incident NSTEMI RWE phenotype (v1.2.0; PPV 91.4%).",
    "Adapts the OMOP-based NSTEMI core for point-in-time eligibility screening:",
    "extends the troponin confirmation window from 24h to 72h to capture late",
    "presenters, removes the 365-day incident washout (not applicable for",
    "point-in-time trial enrollment), and adds TIMI >= 4 and hsCRP > 2 mg/L",
    "per CARDINAL-2 protocol risk stratification criteria.",
    sep = " "
  ),
  inclusion_criteria          = INCLUSION,
  exclusion_criteria          = EXCLUSION,
  funding_source              = "NCCTS UL1TR001420 — CARDINAL-2 CT",
  output_dir                  = CARDINAL_DIR,
  derive_from                 = RADAR_UUID,
  derived_version             = RADAR_VERSION,
  derivation_rationale        = paste(
    "RADAR's validated incident NSTEMI phenotype (PPV 91.4%, peer-reviewed) provides",
    "the validated OMOP backbone (concept 314666, hs-TnI >2x ULN, encounter types).",
    "Three adaptations required for CARDINAL-2 protocol:",
    "(1) Troponin window extended 24h -> 72h for late presenters;",
    "(2) 365-day incident washout removed — trial enrolls at point-in-time presentation;",
    "(3) TIMI >= 4 and hsCRP > 2 mg/L added as protocol eligibility gates.",
    sep = " "
  ),
  upstream_inclusion_criteria = radar$phenotype$inclusion_criteria,
  upstream_exclusion_criteria = radar$phenotype$exclusion_criteria
)

cardinal_schema <- yaml::read_yaml(
  file.path(CARDINAL_DIR, "advocate-phenotype.yaml")
)
cardinal_uuid <- cardinal_schema$project$id

cat("\n── Project initialized ─────────────────────────────────────\n")
cat("UUID:              ", cardinal_uuid, "\n")
cat("Phenotype:         ", cardinal_schema$phenotype$name, "\n")
cat("Version:           ", cardinal_schema$phenotype$version, "\n")
cat("Validation status: ", cardinal_schema$phenotype$validation_status, "\n")
cat("SCE tier:          ", cardinal_schema$compute$sce_tier, "\n")
cat("IRB:               ", cardinal_schema$institution$irb_number, "\n")

cat("\n── Derived-from chain ──────────────────────────────────────\n")
cat("This project:      ", cardinal_uuid, "\n")
cat("  └─ derived from: ", cardinal_schema$project$derived_from, "\n")
cat("     (RADAR Incident NSTEMI RWE v",
    cardinal_schema$project$derived_version, ")\n", sep = "")
cat("     └─ root definition (no further upstream)\n")

cat("\n── Inherited vs. modified criteria ─────────────────────────\n")
for (crit in cardinal_schema$phenotype$inherited_criteria %||% list()) {
  status_label <- switch(crit$status %||% "unknown",
    inherited = "INHERITED (unchanged from RADAR)",
    modified  = "MODIFIED  (adapted for CARDINAL-2)",
    new       = "NEW       (added for this project)",
    crit$status
  )
  cat(sprintf("  %-25s %s\n", paste0(crit$field, ":"), status_label))
}

cat("\n── Derivation rationale ────────────────────────────────────\n")
cat(cardinal_schema$project$derivation_rationale, "\n")

# Add OMOP concept IDs before sourcing (required for OMOP-aligned schemas).
cardinal_schema$phenotype$target_concept_ids <- list(
  314666L,  # NSTEMI
  312327L,  # Acute MI (parent)
  3016502L  # hs-TnI (measurement concept)
)
cardinal_schema$phenotype$icd_codes <- list("I21.4")
yaml::write_yaml(cardinal_schema, file.path(CARDINAL_DIR, "advocate-phenotype.yaml"))

# ── Section 5: Start an authenticated session ─────────────────────────────────
# crio_source() validates the schema, writes a session lock to .advocate/, and
# sets CRIO_* environment variables consumed by downstream tooling.
# With an active Azure CLI login it resolves a real Bearer token (mode =
# "production"); otherwise it falls back to sandbox.

cat("\n── 5a. crio_source() ───────────────────────────────────────────────────\n")

tryCatch({
  session <- crio_source(CARDINAL_DIR, sandbox = FALSE)
}, error = function(e) {
  cat("  Azure session unavailable —", conditionMessage(e),
      "\n  Falling back to sandbox.\n")
  session <<- crio_source(CARDINAL_DIR, sandbox = TRUE)
})

cat("\nSession environment variables:\n")
cat("  CRIO_PROJECT_ID :", Sys.getenv("CRIO_PROJECT_ID"), "\n")
cat("  CRIO_SCE_TIER   :", Sys.getenv("CRIO_SCE_TIER"),   "\n")
cat("  CRIO_ENVIRONMENT:", Sys.getenv("CRIO_ENVIRONMENT"), "\n")
cat("  CRIO_SANDBOX    :", Sys.getenv("CRIO_SANDBOX"),     "\n")

# ── Section 6: Baseline query — inherited NSTEMI core ─────────────────────────
# (was Section 5)
# NSTEMI patients by site using RADAR's OMOP backbone (concept 314666).
# The 365-day washout is not applied here: mock data has one event per patient.
# In production, washout reduces counts ~8-12% per site.

sql_baseline <- "
SELECT
    cs.care_site_name                       AS site,
    p.care_site_id                          AS site_id,
    COUNT(DISTINCT co.person_id)            AS n_patients
FROM condition_occurrence co
JOIN person p
    ON  p.person_id = co.person_id
JOIN care_site cs
    ON  cs.care_site_id = p.care_site_id
WHERE
    co.condition_concept_id      = 314666
    AND co.condition_type_concept_id IN (32817, 32020)
GROUP BY cs.care_site_name, p.care_site_id
ORDER BY p.care_site_id
"

baseline_df <- dbGetQuery(con, sql_baseline)

cat("\n── Baseline: NSTEMI patients by site (RADAR definition) ────\n")
print(baseline_df[, c("site", "n_patients")])

print(
  ggplot(baseline_df, aes(x = reorder(site, -n_patients), y = n_patients)) +
    geom_col(fill = "#2C7BB6", alpha = 0.85) +
    geom_text(aes(label = n_patients), vjust = -0.4, size = 3.5, fontface = "bold") +
    scale_x_discrete(labels = wrap_label(18)) +
    labs(
      title    = "Baseline: NSTEMI patients by site (RADAR definition)",
      subtitle = "OMOP concept 314666 · inpatient / ED · mock_nexus.duckdb",
      x        = NULL,
      y        = "Patients (n)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey40"),
      axis.text.x   = element_text(size = 9)
    )
)

# ── Section 6: Apply trial-specific criteria ───────────────────────────────────
# Add troponin within 72h (INTERVAL 3 DAY). TIMI ≥ 4 and hsCRP > 2 mg/L are
# not yet in the OMOP layer; they will be applied at chart-review stage.
# Atrium Health Carolinas (site 4) has a simulated hsCRP completeness gap of 72%.

sql_trial <- "
SELECT
    cs.care_site_name                       AS site,
    p.care_site_id                          AS site_id,
    COUNT(DISTINCT co.person_id)            AS n_patients
FROM condition_occurrence co
JOIN person p
    ON  p.person_id = co.person_id
JOIN care_site cs
    ON  cs.care_site_id = p.care_site_id
JOIN measurement m
    ON  m.person_id              = co.person_id
    AND m.measurement_concept_id = 3016502
    AND m.value_as_number        > 2.0 * 34
    AND m.measurement_date BETWEEN co.condition_start_date
                              AND co.condition_start_date + INTERVAL 3 DAY
WHERE
    co.condition_concept_id      = 314666
    AND co.condition_type_concept_id IN (32817, 32020)
GROUP BY cs.care_site_name, p.care_site_id
ORDER BY p.care_site_id
"

trial_df <- dbGetQuery(con, sql_trial)

cat("\n── Trial screen: NSTEMI + hs-TnI >68 ng/L within 72h ──────\n")
print(trial_df[, c("site", "n_patients")])

comparison <- merge(
  baseline_df[, c("site_id", "site", "n_patients")],
  trial_df[, c("site_id", "n_patients")],
  by       = "site_id",
  suffixes = c("_baseline", "_trial")
) |>
  mutate(
    delta_n    = n_patients_trial - n_patients_baseline,
    delta_pct  = round(delta_n / n_patients_baseline * 100, 1),
    hscrp_flag = ifelse(site_id == 4L, "hsCRP completeness 72% ⚠", "")
  ) |>
  arrange(site_id)

cat("\n── Site comparison: RADAR baseline vs. CARDINAL-2 screen ───\n")
print(comparison |>
  select(
    Site         = site,
    `Baseline n` = n_patients_baseline,
    `Trial n`    = n_patients_trial,
    `Delta`      = delta_n,
    `Delta %`    = delta_pct,
    `Note`       = hscrp_flag
  )
)

plot_df <- rbind(
  data.frame(site = comparison$site, site_id = comparison$site_id,
             n = comparison$n_patients_baseline, cohort = "Baseline (RADAR)",
             stringsAsFactors = FALSE),
  data.frame(site = comparison$site, site_id = comparison$site_id,
             n = comparison$n_patients_trial,    cohort = "Trial Screen (72h TnI)",
             stringsAsFactors = FALSE)
)
plot_df$cohort <- factor(plot_df$cohort,
  levels = c("Baseline (RADAR)", "Trial Screen (72h TnI)"))

print(
  ggplot(plot_df, aes(x = reorder(site, -n), y = n, fill = cohort)) +
    geom_col(position = "dodge", alpha = 0.85) +
    geom_text(aes(label = n), position = position_dodge(width = 0.9),
              vjust = -0.4, size = 3.2, fontface = "bold") +
    scale_fill_manual(values = c("#2C7BB6", "#D7191C")) +
    scale_x_discrete(labels = wrap_label(16)) +
    labs(
      title    = "NSTEMI patients: RADAR baseline vs. CARDINAL-2 trial screen",
      subtitle = "Red = additional troponin-within-72h filter applied",
      x        = NULL,
      y        = "Patients (n)",
      fill     = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold"),
      plot.subtitle   = element_text(colour = "grey40"),
      axis.text.x     = element_text(size = 9),
      legend.position = "top"
    )
)

# ── Section 7: Enrollment projection ──────────────────────────────────────────
# Reference window: July 2023 – December 2024 (18 months).
# PPV 91.4% from RADAR chart review applied to trial-screen counts.
# Atrium Carolinas hsCRP completeness 72% reduces effective yield at that site.

sql_18mo <- "
SELECT
    cs.care_site_name                       AS site,
    p.care_site_id                          AS site_id,
    COUNT(DISTINCT co.person_id)            AS n_18mo
FROM condition_occurrence co
JOIN person p
    ON  p.person_id = co.person_id
JOIN care_site cs
    ON  cs.care_site_id = p.care_site_id
JOIN measurement m
    ON  m.person_id              = co.person_id
    AND m.measurement_concept_id = 3016502
    AND m.value_as_number        > 2.0 * 34
    AND m.measurement_date BETWEEN co.condition_start_date
                              AND co.condition_start_date + INTERVAL 3 DAY
WHERE
    co.condition_concept_id      = 314666
    AND co.condition_type_concept_id IN (32817, 32020)
    AND co.condition_start_date >= '2023-07-01'
GROUP BY cs.care_site_name, p.care_site_id
ORDER BY p.care_site_id
"

proj_raw <- dbGetQuery(con, sql_18mo)

RADAR_PPV  <- radar$phenotype$ppv
HSCRP_COMP <- c(0.91, 0.88, 0.85, 0.72, 0.89)

proj <- proj_raw |>
  mutate(
    hscrp_completeness = HSCRP_COMP[site_id],
    n_monthly_raw      = round(n_18mo / 18, 1),
    n_chart_confirmed  = round(n_18mo * RADAR_PPV),
    n_hscrp_eligible   = round(n_chart_confirmed * hscrp_completeness),
    monthly_eligible   = round(n_hscrp_eligible / 18, 1),
    hscrp_flag         = ifelse(site_id == 4L, "⚠ 72% — coordinator review needed", "")
  )

total_18mo      <- sum(proj$n_18mo)
total_confirmed <- sum(proj$n_chart_confirmed)
total_eligible  <- sum(proj$n_hscrp_eligible)
monthly_total   <- round(total_eligible / 18, 1)

cat("\n── CARDINAL-2 enrollment projection (Jul 2023 – Dec 2024) ──\n")
print(proj |>
  select(
    Site             = site,
    `18-mo screen`   = n_18mo,
    `PPV-confirmed`  = n_chart_confirmed,
    `hsCRP adj.`     = n_hscrp_eligible,
    `Est. / month`   = monthly_eligible,
    `hsCRP note`     = hscrp_flag
  )
)

cat("\n── Enrollment projection summary ───────────────────────────\n")
cat(sprintf("  18-month screen total:       %d patients\n",  total_18mo))
cat(sprintf("  PPV-adjusted (%.1f%% chart):   %d confirmed\n",
            RADAR_PPV * 100, total_confirmed))
cat(sprintf("  hsCRP-adjusted eligible:     %d patients\n",  total_eligible))
cat(sprintf("  Estimated monthly new rate:  %.1f / month\n", monthly_total))
cat(          "  Atrium Carolinas hsCRP gap:  72% completeness — coordinator\n")
cat(          "                               review required before confirmation\n")
cat("\n")
cat("  * Footnote: projections apply RADAR's PPV (91.4%) to the CARDINAL-2\n")
cat("    screen. Chart review validation of the derived definition —\n")
cat("    particularly the 72h troponin window extension — is pending.\n")

print(
  ggplot(proj, aes(x = reorder(site, -monthly_eligible), y = monthly_eligible)) +
    geom_col(fill = "#1A9641", alpha = 0.85) +
    geom_text(aes(label = sprintf("%.1f / mo", monthly_eligible)),
              vjust = -0.4, size = 3.4, fontface = "bold") +
    scale_x_discrete(labels = wrap_label(18)) +
    labs(
      title    = "Estimated monthly CARDINAL-2 eligible patients by site",
      subtitle = sprintf(
        "PPV %.1f%% (RADAR chart review) · hsCRP completeness-adjusted · Jul 2023 – Dec 2024 window",
        RADAR_PPV * 100
      ),
      x = NULL,
      y = "Estimated eligible / month"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey40", size = 9),
      axis.text.x   = element_text(size = 9)
    )
)

dbDisconnect(con, shutdown = TRUE)

# ── Closing ───────────────────────────────────────────────────────────────────
# The derived definition is ready for clinical validation. When chart review
# is complete and the definition is published, the registry will show three
# validated perspectives on the same clinical concept:
#
#   1. NSTEMI Quality Measure        — Dr. Maya Chen      — CMS quality reporting
#   2. Incident NSTEMI (RWE)         — Dr. Chandrasekaran — RWE, PPV 91.4%
#   3. Incident NSTEMI — CARDINAL-2  — Dr. Amara Nwosu    — Trial eligibility
#
# Each built on the prior. Each serving a distinct purpose. None rebuilt from scratch.
#
# Synthetic data — not real patient records.
