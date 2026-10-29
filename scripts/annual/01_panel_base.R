############################################################
# Global Risk Panel — Stage 01: Panel Base
#
# Script:  01_panel_base.R
# Purpose: Apply analytic-scope filters to the canonical
#          crosswalk produced by stage 00, producing the panel
#          base that all downstream feature scripts consume.
#
# Separation of concerns with stage 00:
#   Stage 00 (00_country_crosswalk.R) is the canonical
#   reference: every (iso3c, year) row that exists in the
#   GW state system from 1946 onwards, with all secondary
#   country code mappings. It is comprehensive and not
#   subject to analytic scoping decisions.
#
#   Stage 01 (this script) applies the scoping decisions for
#   THIS PROJECT'S analytic panel. The settings at the top of
#   this script control:
#     - whether to include microstates
#     - the analytic year window (which may be tighter than
#       the crosswalk's 1946-2050 range)
#     - whether to restrict to a single DCO region (useful
#       for ECA-only development testing)
#
#   Settings are documented and reversible. Flipping any of
#   them and re-running stage 01 onwards regenerates the panel
#   without disturbing stage 00's reference table.
#
# Inputs:
#   output/excel/00_country_crosswalk.RDS
#
# Outputs:
#   output/excel/01_panel_base.csv
#   output/excel/01_panel_base.RDS
#   output/diagnostics/01_panel_audit.csv
#
# Author: George Frederick Willcoxon
############################################################

rm(list = ls())

# -----------------------------
# 0) Packages
# -----------------------------

library(here)
library(dplyr)
library(readr)

# -----------------------------
# 1) Settings
# -----------------------------
#
# These four settings are the only knobs for shaping the
# analytic panel. Flip them and re-run stages 01+ to change
# the panel scope without touching stage 00.

# Exclude GW microstates (population < 250,000). Set to FALSE
# to include them — useful when the dashboard expands beyond
# ECA to the full RC system, which serves several Caribbean
# and Pacific microstates via multi-country offices.

exclude_microstates <- TRUE

# Year window for the analytic panel. The crosswalk extends
# 1946–2050; downstream feature data typically only exists
# 1990s onwards, so most users will want a tighter window.

analysis_start_year <- 1946L
analysis_end_year   <- 2025L

# Restrict the panel by DCO/RC system membership. Valid values:
#   NULL     keep all countries (RC system + non-RC)
#   "all_rc" keep all 160 RC system countries, drop non-RC
#   "AFR"    keep only Africa region
#   "APA"    keep only Asia and the Pacific region
#   "ARAB"   keep only Arab States region
#   "ECA"    keep only Europe and Central Asia region
#   "LAC"    keep only Latin America and the Caribbean region

restrict_to_region <- NULL

# -----------------------------
# 2) Paths
# -----------------------------

project_root       <- here()
output_dir         <- file.path(project_root, "output/excel/annual")
diagnostics_dir <- file.path(project_root, "output/diagnostics")

dir.create(output_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 3) Load crosswalk
# -----------------------------

crosswalk_path <- file.path(output_dir, "00_country_crosswalk.RDS")
if (!file.exists(crosswalk_path)) {
  stop("Crosswalk not found at ", crosswalk_path,
       ". Run 00_country_crosswalk.R first.")
}

crosswalk <- readRDS(crosswalk_path)

n_initial <- nrow(crosswalk)
message("Loaded crosswalk: ", n_initial, " (iso3c, year) rows, ",
        length(unique(crosswalk$iso3c)), " distinct iso3c codes.")

# -----------------------------
# 4) Apply analytic-scope filters
# -----------------------------

panel_base <- crosswalk

# Microstate filter
if (exclude_microstates) {
  if (!"microstate" %in% names(panel_base)) {
    stop("Crosswalk has no `microstate` column — check stage 00 output.")
  }
  n_before <- nrow(panel_base)
  panel_base <- panel_base %>% filter(microstate == 0L)
  message("  Microstate filter: dropped ",
          n_before - nrow(panel_base), " rows.")
}

# Year window
n_before <- nrow(panel_base)
panel_base <- panel_base %>%
  filter(year >= analysis_start_year, year <= analysis_end_year)
message("  Year window [", analysis_start_year, "-",
        analysis_end_year, "]: dropped ",
        n_before - nrow(panel_base), " rows.")

# Regional restriction (optional)
if (!is.null(restrict_to_region)) {
  valid_values <- c("all_rc", "AFR", "APA", "ARAB", "ECA", "LAC")
  if (!restrict_to_region %in% valid_values) {
    stop("restrict_to_region must be one of: NULL, ",
         paste(valid_values, collapse = ", "))
  }
  
  n_before <- nrow(panel_base)
  if (restrict_to_region == "all_rc") {
    panel_base <- panel_base %>% filter(!is.na(dco_region))
    message("  RC system filter: kept ",
            nrow(panel_base), " of ", n_before,
            " rows (any RC region).")
  } else {
    panel_base <- panel_base %>% filter(dco_region == restrict_to_region)
    message("  Region filter [", restrict_to_region, "]: kept ",
            nrow(panel_base), " of ", n_before, " rows.")
  }
}

# Partial-year exclusions
# gwcode 345, 2006: SCG dissolved mid-2006; SRB (340) and MNE (341) cover 2006 fully.
n_before   <- nrow(panel_base)
panel_base <- panel_base %>% filter(!(gwcode == 345L & year == 2006L))
message("  Partial-year exclusions: dropped ", n_before - nrow(panel_base), " row(s).")

# -----------------------------
# 5) Validation
# -----------------------------

stopifnot(
  nrow(panel_base) == nrow(distinct(panel_base, iso3c, year)),
  !any(is.na(panel_base$iso3c)),
  !any(is.na(panel_base$year)),
  !any(is.na(panel_base$gwcode))
)

# -----------------------------
# 6) Audit
# -----------------------------

audit <- panel_base %>%
  group_by(iso3c) %>%
  summarise(
    country_name = first(country_name),
    gwcode       = first(gwcode),
    ccode        = first(ccode),
    iso3n        = first(iso3n),
    dco_region   = first(dco_region),
    first_year   = min(year),
    last_year    = max(year),
    n_years      = n(),
    .groups      = "drop"
  ) %>%
  arrange(dco_region, iso3c)

# Console summary by region
region_summary <- audit %>%
  group_by(dco_region) %>%
  summarise(n_countries = n(), .groups = "drop") %>%
  arrange(desc(n_countries))

message("\nFinal panel base composition:")
message(sprintf("  %d (iso3c, year) rows", nrow(panel_base)))
message(sprintf("  %d distinct countries", length(unique(panel_base$iso3c))))
message(sprintf("  Years %d-%d", min(panel_base$year), max(panel_base$year)))
message("\nBy DCO region:")
for (i in seq_len(nrow(region_summary))) {
  region_label <- region_summary$dco_region[i]
  if (is.na(region_label)) region_label <- "(none — outside RC system)"
  message(sprintf("  %-30s %d countries",
                  region_label, region_summary$n_countries[i]))
}

# -----------------------------
# 7) Write outputs
# -----------------------------

write_csv(panel_base, file.path(output_dir, "01_panel_base.csv"))
saveRDS(panel_base,   file.path(output_dir, "01_panel_base.RDS"))
write_csv(audit,      file.path(diagnostics_dir, "01_panel_audit.csv"))

message(
  "\n01_panel_base.R complete. Outputs:\n",
  "  ", file.path(output_dir, "01_panel_base.csv"), "\n",
  "  ", file.path(output_dir, "01_panel_base.RDS"), "\n",
  "  ", file.path(diagnostics_dir, "01_panel_audit.csv")
)