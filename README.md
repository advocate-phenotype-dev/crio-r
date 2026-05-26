# crio-r

R package for Advocate Health / Wake Forest University School of Medicine
research informatics infrastructure. Provides session management, schema
validation, and phenotype library contribution tooling for investigators
working in secure computing environments.

## Installation

    # install.packages("remotes")
    remotes::install_github("advocate-phenotype-dev/crio-r")

For local development:

    git clone https://github.com/advocate-phenotype-dev/crio-r
    cd crio-r
    Rscript -e "devtools::install()"

## Quick start

    library(crio)

    # Initialize a new project (interactive interview)
    project_dir <- crio_init(output_dir = "my-phenotype")

    # Derive from an existing upstream phenotype
    project_dir <- crio_init(
      output_dir  = "my-phenotype",
      derive_from = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
    )

    crio_source(project_dir, sandbox = TRUE)
    crio_validate(project_dir)
    crio_publish(project_dir, library_dir = "../phenotype-library", message = "v0.1", sandbox = TRUE)
    crio_export(project_dir, target = "ohdsi_pl")
    crio_deposit(project_dir, sandbox = TRUE)

## Browsing the library

    crio_list()
    crio_list(domain = "condition", status = "internal_validated")
    crio_list(pi = "reyes", search = "heart failure")

Interactive pager with next/prev navigation. Set `CRIO_LIBRARY_DIR` to the
path of a local clone of the phenotype library, or clone it adjacent to your
project directories and the package will find it automatically.

## Schema enforcement

- Institutional email required (wakehealth.edu, wfusm.edu, advocatehealth.org/com)
- ORCID iD format validated
- IRB number and status required for SCE tier 3+
- Semantic versioning enforced
- Deposit eligibility computed automatically from validation status and field completeness

## External registries

OMOP-aligned phenotypes export to OHDSI Phenotype Library (`target = "ohdsi_pl"`)
or PheKB (`target = "phekb"`).

## Phenotype library

Committed projects are tracked at:
https://github.com/advocate-phenotype-dev/phenotype-library

## Status

Active prototype. Azure TRE credential endpoint not yet connected.
Run with `sandbox = TRUE` for local development.

## License

MIT
