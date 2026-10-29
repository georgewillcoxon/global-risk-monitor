############################################################
# Global Risk Panel — Stage 20: Master Panel Assembly
#
# Script:  20_assemble_panel.R
# Purpose: Merge all feature scripts (02–11) into a single
#          master country-year panel keyed on (iso3c, year).
#          Apply row-count assertions after every join, log
#          the cumulative column count, and write the final
#          panel as both CSV and RDS.
#
# Inputs:
#   01_panel_base.RDS              base scaffold (gwcode, ccode,
#                                  iso3c, iso3n, statename, year)
#   02_features_wdi.RDS            WDI economic, social
#   02b_features_wgi.RDS           WGI governance indicators
#   03_features_imf.RDS            IMF WEO and IFS macro/fiscal
#   04_features_vdem_polity.RDS    V-Dem and Polity democracy indices
#   05_features_population.RDS     UN WPP population and age structure
#   06_features_governance.RDS     COW NMC + Anders SDP/GDP
#   07_features_conflict.RDS       UCDP ACD, GED, contiguity spillover
#   08_features_instability.RDS    Archigos, PT coups, REIGN, ACLED
#   08b_features_reign.RDS
#   08c_features_navco.RDS
#   09_features_climate.RDS        CCKP temperature/precipitation
#   10_features_humanitarian.RDS   UNHCR refugees, OECD humanitarian aid
#   11_features_demographic.RDS    EPR ethnic exclusion, horizontal inequality
#   11b_features_gap_filling.RDS   RR crises, SIPRI milex, IMF MONA programmes,
#                                  WRP religious fractionalisation
#   11c_crisis_extension.RDS
#   11d_features_creg.RDS
#   12_features_opinion.RDS        WVS/QoG public opinion indices, GDELT sentiment
#
# Post-assembly composites (step 4b):
#   nmc_milex_gdp         NMC milex / WDI nominal GDP (intermediate)
#   milex_gdp_composite   SIPRI % GDP primary, NMC/WDI fill (1960-2016)
#   milex_gdp_source      Provenance: "sipri" | "nmc_wb"
#   unemp_composite       WDI primary, IMF WEO fill (1980-1990)
#   unemp_composite_source Provenance: "wdi" | "imf"
#
# Outputs:
#   output/excel/annual/20_panel_master.csv
#   output/excel/annual/20_panel_master.RDS
#   output/diagnostics/20_assembly_log.csv
#
# Author: George Frederick Willcoxon
############################################################

rm(list = ls())

# -----------------------------
# 0) Packages
# -----------------------------

library(here)
library(dplyr)
library(tidyr)
library(readr)

# -----------------------------
# 1) Settings
# -----------------------------

project_root       <- here()
output_dir         <- file.path(project_root, "output/excel/annual")
diagnostics_dir <- file.path(project_root, "output/diagnostics")

dir.create(output_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2) Load all feature frames
# -----------------------------

message("Loading all feature frames...")

panel_base   <- readRDS(file.path(output_dir, "01_panel_base.RDS"))

feature_files <- c(
  wdi          = "02_features_wdi.RDS",
  wgi          = "02b_features_wgi.RDS",
  imf          = "03_features_imf.RDS",
  vdem_polity  = "04_features_vdem_polity.RDS",
  population   = "05_features_population.RDS",
  governance   = "06_features_governance.RDS",
  conflict     = "07_features_conflict.RDS",
  instability  = "08_features_instability.RDS",
  reign        = "08b_features_reign.RDS",
  navco        = "08c_features_navco.RDS",
  climate      = "09_features_climate.RDS",
  humanitarian = "10_features_humanitarian.RDS",
  demographic  = "11_features_demographic.RDS",
  gap_filling  = "11b_features_gap_filling.RDS",
  crisis_ext   = "11c_crisis_extension.RDS",
  creg         = "11d_features_creg.RDS",
  opinion      = "12_features_opinion.RDS"
)

feature_frames <- lapply(seq_along(feature_files), function(i) {
  nm <- names(feature_files)[i]
  f  <- feature_files[i]
  fp <- file.path(output_dir, f)
  if (file.exists(fp)) {
    readRDS(fp)
  } else {
    if (nm == "wgi") {
      message("  WGI not found — skipping. ",
              "Run 02b_features_wgi.R after downloading the bulk file.")
    } else {
      warning("Missing feature frame: ", f)
    }
    NULL
  }
})
names(feature_frames) <- names(feature_files)

# -----------------------------
# 3) Assemble — start with panel_base, merge each feature
# -----------------------------

panel <- panel_base
log_rows <- tibble(
  stage      = "01_base",
  n_rows     = nrow(panel),
  n_cols     = ncol(panel),
  n_iso3c    = length(unique(panel$iso3c)),
  year_min   = min(panel$year),
  year_max   = max(panel$year)
)

for (nm in names(feature_frames)) {
  ff <- feature_frames[[nm]]
  if (is.null(ff) || nrow(ff) == 0L) {
    message("  Skipping empty feature frame: ", nm)
    next
  }
  
  # Pre-merge: assert join key uniqueness on the right side
  if (!all(c("iso3c", "year") %in% names(ff))) {
    warning("Feature frame ", nm,
            " is missing iso3c/year keys; skipping.")
    next
  }
  
  ff_unique <- distinct(ff, iso3c, year)
  if (nrow(ff_unique) != nrow(ff)) {
    warning("Feature frame ", nm,
            " has duplicate (iso3c, year) keys; ",
            "merge may produce duplicates. Check upstream.")
  }
  
  # Pre-merge row count
  n_before <- nrow(panel)
  
  # Identify columns to add (all except keys)
  add_cols <- setdiff(names(ff), names(panel))
  
  # Drop any of the right-side cols that already exist in panel
  # (this prevents duplicate columns from sneaking in)
  ff <- ff %>% select(iso3c, year, all_of(add_cols))
  
  # Left join
  panel <- panel %>% left_join(ff, by = c("iso3c", "year"))
  
  # Post-merge assertions
  n_after <- nrow(panel)
  if (n_after != n_before) {
    stop("Row count changed after merging ", nm,
         ": before = ", n_before, ", after = ", n_after,
         ". Check for duplicate keys in feature frame.")
  }
  if (nrow(panel) != nrow(distinct(panel, iso3c, year))) {
    stop("After merging ", nm,
         ", panel has duplicate (iso3c, year) keys.")
  }
  
  log_rows <- bind_rows(log_rows, tibble(
    stage      = nm,
    n_rows     = nrow(panel),
    n_cols     = ncol(panel),
    n_iso3c    = length(unique(panel$iso3c)),
    year_min   = min(panel$year),
    year_max   = max(panel$year)
  ))
  
  message("  Merged ", nm, ": +", length(add_cols),
          " cols (total = ", ncol(panel), ")")
}

# ============================================================
# 9) Integration note for 20_assemble_panel.R
# ============================================================
#
# To incorporate this extension into the master panel, add the
# following block to 20_assemble_panel.R (or wherever crisis
# variables are joined):
#
#   crisis_ext <- readRDS(file.path(output_dir,
#                                   "11c_crisis_extension.RDS"))
#
#   panel <- panel %>%
#     left_join(
#       crisis_ext %>%
#         select(iso3c, year,
#                banking_crisis, systemic_crisis,
#                currency_crisis, inflation_crisis,
#                any_crisis, crisis_source),
#       by = c("iso3c", "year")
#     )
#
# If you want to keep the original rr_* column names for
# backward compatibility with existing model code, rename:
#
#   panel <- panel %>%
#     rename(
#       rr_banking_crisis   = banking_crisis,
#       rr_systemic_crisis  = systemic_crisis,
#       rr_currency_crisis  = currency_crisis,
#       rr_inflation_crisis = inflation_crisis
#     )
#
# The crisis_source variable allows you to subset analyses to
# authoritative (RR/LV) coding only, or to use the full extended
# series. For model training on historical data, you may want to
# restrict to RR/LV years. For out-of-sample prediction targets
# (2017–2025), the rule-based extension is the appropriate
# operationalisation.
# ============================================================


# -----------------------------
# 4) Final validation
# -----------------------------

stopifnot(
  nrow(panel) == nrow(distinct(panel, iso3c, year)),
  all(!is.na(panel$iso3c)),
  all(!is.na(panel$year)),
  all(panel$year >= 1946L)
)

message(
  "\nFINAL PANEL:\n",
  "  Rows:       ", nrow(panel), "\n",
  "  Cols:       ", ncol(panel), "\n",
  "  ISO3 codes: ", length(unique(panel$iso3c)), "\n",
  "  Year range: ", min(panel$year), "–", max(panel$year)
)

# -----------------------------
# 4b) Composite indicators
# -----------------------------
#
# Composite 1: Military expenditure as % of GDP
# -----------------------------------------------
# Primary:   sipri_milex_gdp  (SIPRI, % GDP, 1949-2025)
# Secondary: nmc_milex / gdp_current_wb
#              nmc_milex    = current USD thousands (NMC, 1946-2016)
#              gdp_current_wb = current USD millions (WDI NY.GDP.MKTP.CD,
#                               1960-2024)
#            Conversion: (nmc_milex * 1000) / gdp_current_wb
#            because nmc_milex is current USD thousands and
#            gdp_current_wb is current USD. Output is a GDP share,
#            matching the scale of sipri_milex_gdp in this panel.
#
# Coverage:  SIPRI % GDP alone: ~73.4%; composite: ~93.5%.
#            1946-1959 NMC gap: nmc_milex exists but gdp_current_wb
#            starts at 1960, so composite is NA for those years.
#            nmc_milex is retained as a raw feature for the model.
#
# milex_gdp_source flag:
#   "sipri"  — SIPRI sipri_milex_gdp (primary)
#   "nmc_wb" — NMC milex / WDI nominal GDP (gap-fill, 1960-2016)
#   NA       — no data available

panel <- panel %>%
  mutate(
    nmc_milex_gdp = if_else(
      !is.na(nmc_milex) & !is.na(gdp_current_wb) & gdp_current_wb > 0,
      (nmc_milex * 1000) / gdp_current_wb,
      NA_real_
    ),
    milex_gdp_composite = coalesce(sipri_milex_gdp, nmc_milex_gdp),
    milex_gdp_source = case_when(
      !is.na(sipri_milex_gdp) ~ "sipri",
      !is.na(nmc_milex_gdp)   ~ "nmc_wb",
      TRUE                    ~ NA_character_
    )
  )

n_milex_composite <- sum(!is.na(panel$milex_gdp_composite))
message(sprintf(
  "milex_gdp_composite: %d non-NA rows (%.1f%%) | sources: %s",
  n_milex_composite,
  100 * n_milex_composite / nrow(panel),
  paste(
    names(table(panel$milex_gdp_source)),
    table(panel$milex_gdp_source),
    sep = "=", collapse = ", "
  )
))

# Composite 2: Unemployment rate
# --------------------------------
# Primary:   unemp_wb   (WDI/ILO modelled, 1991-2025, 172 countries)
# Secondary: unemp_imf  (IMF WEO LUR, 1980-2025, 107 countries)
#
# The two series derive from the same ILO source and are closely
# aligned where both exist (Spearman r = 0.88, median diff = 0.26 pp).
# WDI is preferred as primary: broader country coverage and more
# stable across revisions.
#
# Composite gain: 638 additional rows (+5.5 pp), primarily from
# 1980-1990 IMF observations for 69 established economies that
# WDI does not cover pre-1991.
#
# unemp_composite_source flag:
#   "wdi"  — WDI primary series (unemp_wb)
#   "imf"  — IMF WEO fill (unemp_imf)
#   NA     — no data available

panel <- panel %>%
  mutate(
    unemp_composite = coalesce(unemp_wb, unemp_imf),
    unemp_composite_source = case_when(
      !is.na(unemp_wb)  ~ "wdi",
      !is.na(unemp_imf) ~ "imf",
      TRUE              ~ NA_character_
    )
  )

n_unemp_composite <- sum(!is.na(panel$unemp_composite))
message(sprintf(
  "unemp_composite: %d non-NA rows (%.1f%%) | sources: %s",
  n_unemp_composite,
  100 * n_unemp_composite / nrow(panel),
  paste(
    names(table(panel$unemp_composite_source)),
    table(panel$unemp_composite_source),
    sep = "=", collapse = ", "
  )
))

# -----------------------------
# 5) Write outputs
# -----------------------------

write_csv(panel, file.path(output_dir, "20_panel_master.csv"))
saveRDS(panel,   file.path(output_dir, "20_panel_master.RDS"))
write_csv(log_rows, file.path(diagnostics_dir, "20_assembly_log.csv"))

message(
  "\n20_assemble_panel.R complete. Outputs:\n",
  "  ", file.path(output_dir, "20_panel_master.csv"), "\n",
  "  ", file.path(output_dir, "20_panel_master.RDS"), "\n",
  "  ", file.path(diagnostics_dir, "20_assembly_log.csv")
)