############################################################
# Global Risk Panel — Stage 22: Quality Control & Spot-Check
#
# Script:  22_qc_spot_check.R
# Purpose: Two-part quality control of the master country-year
#          panel (20_panel_master.RDS):
#
#   PART A — Structural validation (automated)
#     Checks panel skeleton integrity: row count, key
#     uniqueness, expected countries and year range, no
#     orphan ISO codes, dco_region coverage, and column-
#     level coverage rates by source group. Hard stops on
#     critical failures; soft warnings on coverage anomalies.
#     Writes a machine-readable diagnostics CSV.
#
#   PART B — Stratified random spot-check workbook
#     Draws a stratified random sample of (iso3c, year) pairs
#     — 8 per RC region + 4 from the non-RC group — and
#     extracts a curated set of ~20 key variables across all
#     major source domains (economic, demographic, conflict,
#     instability, governance, climate, humanitarian).
#     Exports an Excel workbook with two sheets:
#       "spot_check"  — sampled cells with a blank
#                       "manual_verified_value" column for
#                       the analyst to fill in against the
#                       original source
#       "key_lookup"  — quick-reference table mapping each
#                       spot-check variable to its original
#                       source and an online lookup URL
#     The filled workbook is read back in by 22b_qc_compare.R.
#
# Inputs:
#   output/excel/annual/20_panel_master.RDS
#   output/excel/annual/01_panel_base.RDS    (expected skeleton)
#
# Outputs:
#   output/diagnostics/22_structural_validation.csv
#   output/diagnostics/22_coverage_by_source.csv
#   output/diagnostics/22_spot_check_workbook.xlsx
#
# Run order: after 20_assemble_panel.R
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
library(stringr)
library(openxlsx)

# -----------------------------
# 1) Settings
# -----------------------------

project_root    <- here()
output_dir      <- file.path(project_root, "output/excel/annual")
diagnostics_dir <- file.path(project_root, "output/diagnostics")

dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

# Random seed for reproducible sample — change to re-draw
set.seed(2025L)

# Number of (iso3c, year) pairs to sample per RC region
n_per_rc_region  <- 8L
# ... and for the non-RC group (NA dco_region)
n_non_rc_sample  <- 4L

# Coverage warning threshold: flag any column with < this
# fraction non-NA among country-years where data is expected
coverage_warn_threshold <- 0.05   # 5 %

# Year ranges where each source domain is expected to have data.
# Used to scope coverage checks to the relevant window only;
# avoids penalising a source for years it was never meant to cover.
source_coverage_windows <- list(
  wdi        = c(1960L, 2024L),
  imf        = c(1980L, 2024L),
  wgi        = c(1996L, 2024L),
  vdem       = c(1946L, 2024L),
  polity     = c(1946L, 2018L),
  population = c(1950L, 2024L),
  conflict   = c(1946L, 2024L),
  instab     = c(1950L, 2024L),
  climate    = c(1950L, 2024L),
  humanitarian = c(1975L, 2024L),
  demographic  = c(1946L, 2024L)
)

# Curated spot-check variables: one representative per source domain.
# Format: list of named vectors — name = display label, value = column name.
# These are chosen to be independently verifiable against online portals.

spot_check_vars <- c(
  # Economic — WDI
  gdppc_wb       = "gdppc_wb",
  xgdp_wb        = "xgdp_wb",
  inflation_wb   = "inflation_wb",
  unemp_wb       = "unemp_wb",
  # Economic — IMF
  unemp_imf      = "unemp_imf",
  # Governance — WGI (bare two-letter column names: cc, ge, ps, rq, rl, va)
  wgi_rule_of_law    = "rl",
  wgi_control_corr   = "cc",
  # Democracy — V-Dem / Polity
  vdem_polyarchy = "v2x_polyarchy",
  polity_score   = "polity2",
  # Population — UN WPP
  pop_total      = "pop_total",
  # Conflict — UCDP
  ucdp_ongoing    = "ucdp_ongoing",
  ucdp_ged_brd_best = "ucdp_ged_brd_best",
  # Instability — Powell-Thyne / Archigos
  pt_coup              = "pt_coup",
  archigos_irregular   = "archigos_irregular",
  # Climate — CCKP
  tmp_mean        = "tmp_mean",
  pre_total       = "pre_total",
  # Humanitarian — UNHCR
  refugees_origin = "refugees_origin",
  # Demographic — EPR
  epr_n_groups    = "epr_n_groups"
)

# Source lookup table for the key sheet of the workbook
source_lookup <- tribble(
  ~variable,               ~source_label,         ~lookup_url,
  "gdppc_wb",        "World Bank WDI",        "https://data.worldbank.org/indicator/NY.GDP.PCAP.KD",
  "xgdp_wb",         "World Bank WDI",        "https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG",
  "inflation_wb",    "World Bank WDI",        "https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG",
  "unemp_wb",        "World Bank WDI",        "https://data.worldbank.org/indicator/SL.UEM.TOTL.ZS",
  "unemp_imf",       "IMF WEO",               "https://www.imf.org/en/Publications/WEO/weo-database/2024/April",
  "rl",              "World Bank WGI",        "https://info.worldbank.org/governance/wgi/",
  "cc",              "World Bank WGI",        "https://info.worldbank.org/governance/wgi/",
  "v2x_polyarchy",   "V-Dem",                 "https://v-dem.net/data/the-v-dem-dataset/",
  "polity2",         "Polity5",               "https://www.systemicpeace.org/polityproject.html",
  "pop_total",       "UN WPP",                "https://population.un.org/wpp/",
  "ucdp_ongoing",    "UCDP ACD",              "https://ucdp.uu.se/",
  "ucdp_ged_brd_best", "UCDP GED",              "https://ucdp.uu.se/",
  "pt_coup",         "Powell-Thyne Coups",    "http://www.jonathanmpowell.com/coup-detat-dataset.html",
  "archigos_irregular", "Archigos",           "https://www.rochester.edu/college/faculty/hgoemans/data.htm",
  "tmp_mean",        "World Bank CCKP",       "https://climateknowledgeportal.worldbank.org/",
  "pre_total",       "World Bank CCKP",       "https://climateknowledgeportal.worldbank.org/",
  "refugees_origin", "UNHCR",                 "https://data.unhcr.org/",
  "epr_n_groups",    "EPR Dataset",           "https://icr.ethz.ch/data/epr/"
)

# ============================================================
# PART A — STRUCTURAL VALIDATION
# ============================================================

message("\n", strrep("=", 60))
message("PART A: STRUCTURAL VALIDATION")
message(strrep("=", 60))

# -----------------------------
# A1) Load panel and base scaffold
# -----------------------------

panel_path <- file.path(output_dir, "20_panel_master.RDS")
base_path  <- file.path(output_dir, "01_panel_base.RDS")

if (!file.exists(panel_path)) {
  stop("Master panel not found at: ", panel_path,
       "\nRun 20_assemble_panel.R first.")
}
if (!file.exists(base_path)) {
  stop("Panel base not found at: ", base_path,
       "\nRun 01_panel_base.R first.")
}

panel      <- readRDS(panel_path)
panel_base <- readRDS(base_path)

message("\nPanel loaded: ", nrow(panel), " rows x ", ncol(panel), " cols.")
message("Panel base:   ", nrow(panel_base), " rows x ", ncol(panel_base), " cols.")

# Collect all validation results in a tidy log
val_log <- tibble(
  check    = character(),
  status   = character(),    # "PASS", "WARN", "FAIL"
  detail   = character()
)

log_result <- function(check, status, detail = "") {
  symbol <- switch(status, PASS = "OK", WARN = "!!", FAIL = "XX")
  message(sprintf("  [%s] %s%s", symbol, check,
                  if (nchar(detail) > 0) paste0(" — ", detail) else ""))
  val_log <<- bind_rows(val_log, tibble(check = check,
                                        status = status,
                                        detail = detail))
}

# -----------------------------
# A2) Key column presence
# -----------------------------

message("\n--- A2: Required columns ---")

required_cols <- c("iso3c", "year", "gwcode", "ccode",
                   "country_name", "dco_region")

for (col in required_cols) {
  if (col %in% names(panel)) {
    log_result(paste0("Column present: ", col), "PASS")
  } else {
    log_result(paste0("Column present: ", col), "FAIL",
               "Column missing from panel — check 20_assemble_panel.R")
  }
}

# -----------------------------
# A3) Duplicate key check
# -----------------------------

message("\n--- A3: Duplicate (iso3c, year) keys ---")

n_rows    <- nrow(panel)
n_distinct_keys <- nrow(distinct(panel, iso3c, year))
n_dupes   <- n_rows - n_distinct_keys

if (n_dupes == 0L) {
  log_result("No duplicate (iso3c, year) keys", "PASS",
             sprintf("%d unique keys", n_distinct_keys))
} else {
  dup_detail <- panel %>%
    group_by(iso3c, year) %>%
    filter(n() > 1L) %>%
    ungroup() %>%
    distinct(iso3c, year) %>%
    arrange(iso3c, year) %>%
    slice_head(n = 10L) %>%
    mutate(label = paste0(iso3c, "/", year)) %>%
    pull(label) %>%
    paste(collapse = ", ")
  log_result("No duplicate (iso3c, year) keys", "FAIL",
             sprintf("%d duplicate rows. First offenders: %s",
                     n_dupes, dup_detail))
  message("  DUPLICATE DETAIL:")
  panel %>%
    group_by(iso3c, year) %>%
    filter(n() > 1L) %>%
    arrange(iso3c, year) %>%
    print(n = 30L)
}

# -----------------------------
# A4) Panel row count vs base scaffold
# -----------------------------

message("\n--- A4: Row count vs base scaffold ---")

if (nrow(panel) == nrow(panel_base)) {
  log_result("Row count matches base scaffold", "PASS",
             sprintf("%d rows", nrow(panel)))
} else {
  log_result("Row count matches base scaffold", "FAIL",
             sprintf("Panel = %d, Base = %d, Diff = %+d",
                     nrow(panel), nrow(panel_base),
                     nrow(panel) - nrow(panel_base)))
}

# -----------------------------
# A5) No NA keys
# -----------------------------

message("\n--- A5: No NA key values ---")

na_iso3c  <- sum(is.na(panel$iso3c))
na_year   <- sum(is.na(panel$year))
na_gwcode <- sum(is.na(panel$gwcode))

if (na_iso3c == 0L) {
  log_result("No NA iso3c",  "PASS")
} else {
  log_result("No NA iso3c",  "FAIL", sprintf("%d NA rows", na_iso3c))
}

if (na_year == 0L) {
  log_result("No NA year",   "PASS")
} else {
  log_result("No NA year",   "FAIL", sprintf("%d NA rows", na_year))
}

if (na_gwcode == 0L) {
  log_result("No NA gwcode", "PASS")
} else {
  # gwcode is legitimately NA for a small number of non-GW entities
  # (e.g. Kosovo in some earlier years); treat as a warning, not a hard fail
  log_result("No NA gwcode", "WARN",
             sprintf("%d NA rows — may be non-GW entities; inspect manually",
                     na_gwcode))
}

# -----------------------------
# A6) Year range
# -----------------------------

message("\n--- A6: Year range ---")

year_min <- min(panel$year, na.rm = TRUE)
year_max <- max(panel$year, na.rm = TRUE)

if (year_min == 1946L) {
  log_result("Year range: min = 1946", "PASS")
} else {
  log_result("Year range: min = 1946", "WARN",
             sprintf("Actual min = %d", year_min))
}

if (year_max >= 2024L) {
  log_result(sprintf("Year range: max >= 2024 (actual %d)", year_max), "PASS")
} else {
  log_result("Year range: max >= 2024", "WARN",
             sprintf("Actual max = %d — panel may not be current", year_max))
}

# -----------------------------
# A7) Country count and DCO region coverage
# -----------------------------

message("\n--- A7: Country and region coverage ---")

n_countries <- n_distinct(panel$iso3c)
log_result(sprintf("Country count: %d distinct iso3c codes", n_countries), "PASS")

region_counts <- panel %>%
  distinct(iso3c, dco_region) %>%
  mutate(region_label = if_else(is.na(dco_region), "(non-RC)", dco_region)) %>%
  count(region_label, name = "n_countries") %>%
  arrange(desc(n_countries))

message("\n  DCO region composition (distinct countries):")
for (i in seq_len(nrow(region_counts))) {
  message(sprintf("    %-20s %d countries",
                  region_counts$region_label[i],
                  region_counts$n_countries[i]))
}

# Expected minimum country counts per region (conservative floors)
expected_floors <- c(AFR = 40L, APA = 20L, ARAB = 10L, ECA = 15L, LAC = 20L)
for (rgn in names(expected_floors)) {
  actual <- region_counts %>%
    filter(region_label == rgn) %>%
    pull(n_countries)
  actual <- if (length(actual) == 0L) 0L else actual
  if (actual >= expected_floors[rgn]) {
    log_result(sprintf("Region %s country floor (%d)", rgn, expected_floors[rgn]), "PASS",
               sprintf("actual = %d", actual))
  } else {
    log_result(sprintf("Region %s country floor (%d)", rgn, expected_floors[rgn]), "WARN",
               sprintf("actual = %d — fewer countries than expected", actual))
  }
}

# -----------------------------
# A8) Orphan ISO codes
# -----------------------------

message("\n--- A8: Orphan ISO codes (panel vs base) ---")

panel_iso  <- sort(unique(panel$iso3c))
base_iso   <- sort(unique(panel_base$iso3c))

orphan_in_panel <- setdiff(panel_iso, base_iso)
missing_from_panel <- setdiff(base_iso, panel_iso)

if (length(orphan_in_panel) == 0L) {
  log_result("No iso3c codes in panel absent from base", "PASS")
} else {
  log_result("No iso3c codes in panel absent from base", "WARN",
             paste(orphan_in_panel, collapse = ", "))
}

if (length(missing_from_panel) == 0L) {
  log_result("No iso3c codes in base absent from panel", "PASS")
} else {
  log_result("No iso3c codes in base absent from panel", "WARN",
             paste(missing_from_panel, collapse = ", "))
}

# -----------------------------
# A9) Column-level coverage by source group
# -----------------------------

message("\n--- A9: Coverage by source group ---")

# Map column prefixes / names to source groups
col_groups <- list(
  wdi        = grep("_wb$",       names(panel), value = TRUE),
  imf        = grep("_imf$",      names(panel), value = TRUE),
  wgi        = intersect(c("cc", "ge", "ps", "rq", "rl", "va"), names(panel)),
  vdem       = grep("^v2x_|^v2",  names(panel), value = TRUE),
  polity     = grep("polity|xconst|xrreg|xropen|parreg|parcomp",
                    names(panel), value = TRUE),
  population = grep("^pop_",      names(panel), value = TRUE),
  conflict   = grep("^ucdp_",     names(panel), value = TRUE),
  instab     = grep("^(archigos|pt_|acled_|reign_)",
                    names(panel), value = TRUE),
  climate    = intersect(c("tmp_mean", "tmp_anom_6190", "pre_total", "pre_anom_6190",
                           "spei12_mean", "spei12_min", "spei24_mean", "spei24_min"),
                         names(panel)),
  humanitarian = grep("^(refugees|idp|oda_)", names(panel), value = TRUE),
  demographic  = grep("^epr_",    names(panel), value = TRUE)
)

coverage_rows <- tibble(
  source_group  = character(),
  n_columns     = integer(),
  year_min      = integer(),
  year_max      = integer(),
  n_rows_window = integer(),
  pct_nonNA_first_col = numeric(),
  first_col     = character(),
  status        = character()
)

for (grp in names(col_groups)) {
  cols <- col_groups[[grp]]
  if (length(cols) == 0L) {
    message(sprintf("  %-18s — NO columns found in panel", grp))
    log_result(sprintf("Source group present: %s", grp), "WARN",
               "No matching columns found")
    next
  }
  
  win     <- source_coverage_windows[[grp]]
  subpanel <- panel %>% filter(year >= win[1], year <= win[2])
  n_window <- nrow(subpanel)
  
  first_col  <- cols[1]
  pct_nonNA  <- if (n_window > 0)
    round(100 * mean(!is.na(subpanel[[first_col]])), 1) else NA_real_
  
  status <- if (is.na(pct_nonNA)) "WARN" else
    if (pct_nonNA / 100 >= coverage_warn_threshold) "PASS" else "WARN"
  
  log_result(
    sprintf("Coverage: %-18s (%d cols, %d–%d window)", grp, length(cols), win[1], win[2]),
    status,
    sprintf("first col '%s': %.1f%% non-NA", first_col, pct_nonNA)
  )
  
  coverage_rows <- bind_rows(coverage_rows, tibble(
    source_group        = grp,
    n_columns           = length(cols),
    year_min            = win[1],
    year_max            = win[2],
    n_rows_window       = n_window,
    pct_nonNA_first_col = pct_nonNA,
    first_col           = first_col,
    status              = status
  ))
}

# -----------------------------
# A10) Composite variable sanity
# -----------------------------

message("\n--- A10: Composite variable sanity ---")

# milex_gdp_composite: should be in [0, 100] where non-NA
if ("milex_gdp_composite" %in% names(panel)) {
  vals <- panel$milex_gdp_composite[!is.na(panel$milex_gdp_composite)]
  out_of_range <- sum(vals < 0 | vals > 100)
  if (out_of_range == 0L) {
    log_result("milex_gdp_composite range [0,100]", "PASS",
               sprintf("%d non-NA values", length(vals)))
  } else {
    log_result("milex_gdp_composite range [0,100]", "WARN",
               sprintf("%d values outside [0,100]", out_of_range))
  }
} else {
  log_result("milex_gdp_composite present", "WARN", "Column not found")
}

# unemp_composite: should be in [0, 100]
if ("unemp_composite" %in% names(panel)) {
  vals <- panel$unemp_composite[!is.na(panel$unemp_composite)]
  out_of_range <- sum(vals < 0 | vals > 100)
  if (out_of_range == 0L) {
    log_result("unemp_composite range [0,100]", "PASS",
               sprintf("%d non-NA values", length(vals)))
  } else {
    log_result("unemp_composite range [0,100]", "WARN",
               sprintf("%d values outside [0,100]", out_of_range))
  }
} else {
  log_result("unemp_composite present", "WARN", "Column not found")
}

# polity2: should be in [-10, 10]
if ("polity2" %in% names(panel)) {
  vals <- panel$polity2[!is.na(panel$polity2) & panel$polity2 > -88]
  out_of_range <- sum(vals < -10 | vals > 10)
  if (out_of_range == 0L) {
    log_result("polity2 range [-10,10]", "PASS",
               sprintf("%d non-NA non-special values", length(vals)))
  } else {
    log_result("polity2 range [-10,10]", "WARN",
               sprintf("%d values outside [-10,10]", out_of_range))
  }
} else {
  log_result("polity2 present", "WARN", "Column not found")
}

# v2x_polyarchy: should be in [0, 1]
if ("v2x_polyarchy" %in% names(panel)) {
  vals <- panel$v2x_polyarchy[!is.na(panel$v2x_polyarchy)]
  out_of_range <- sum(vals < 0 | vals > 1)
  if (out_of_range == 0L) {
    log_result("v2x_polyarchy range [0,1]", "PASS",
               sprintf("%d non-NA values", length(vals)))
  } else {
    log_result("v2x_polyarchy range [0,1]", "WARN",
               sprintf("%d values outside [0,1]", out_of_range))
  }
} else {
  log_result("v2x_polyarchy present", "WARN", "Column not found")
}

# -----------------------------
# A11) Summary
# -----------------------------

message("\n", strrep("-", 60))
n_pass <- sum(val_log$status == "PASS")
n_warn <- sum(val_log$status == "WARN")
n_fail <- sum(val_log$status == "FAIL")
message(sprintf("Structural validation complete: %d PASS | %d WARN | %d FAIL",
                n_pass, n_warn, n_fail))

if (n_fail > 0L) {
  fails <- val_log %>% filter(status == "FAIL") %>%
    mutate(msg = paste0(check, ": ", detail)) %>%
    pull(msg)
  message("\nFAILURES:")
  for (f in fails) message("  - ", f)
}

# Write structural validation log
write_csv(val_log,
          file.path(diagnostics_dir, "22_structural_validation.csv"))
write_csv(coverage_rows,
          file.path(diagnostics_dir, "22_coverage_by_source.csv"))

message("\nDiagnostics written to:")
message("  ", file.path(diagnostics_dir, "22_structural_validation.csv"))
message("  ", file.path(diagnostics_dir, "22_coverage_by_source.csv"))

# ============================================================
# PART B — STRATIFIED RANDOM SPOT-CHECK WORKBOOK
# ============================================================

message("\n", strrep("=", 60))
message("PART B: STRATIFIED RANDOM SPOT-CHECK WORKBOOK")
message(strrep("=", 60))

# -----------------------------
# B1) Draw stratified sample
# -----------------------------

message("\nDrawing stratified sample...")

# Helper: sample n rows from a group, falling back to all rows if group is small
sample_group <- function(df, n) {
  if (nrow(df) <= n) df else slice_sample(df, n = n)
}

# Restrict the sampling frame to years where at least some data exists
# (post-1960, where most sources have meaningful coverage)
# and to country-years with a valid iso3c
sample_frame <- panel %>%
  filter(!is.na(iso3c), year >= 1960L) %>%
  distinct(iso3c, year, dco_region, country_name)

# Sample from each RC region
rc_regions <- c("AFR", "APA", "ARAB", "ECA", "LAC")

rc_samples <- lapply(rc_regions, function(rgn) {
  sample_frame %>%
    filter(dco_region == rgn) %>%
    sample_group(n_per_rc_region)
})
rc_sample <- bind_rows(rc_samples)

# Sample from non-RC group (NA dco_region)
non_rc_sample <- sample_frame %>%
  filter(is.na(dco_region)) %>%
  sample_group(n_non_rc_sample)

sample_keys <- bind_rows(rc_sample, non_rc_sample) %>%
  arrange(dco_region, iso3c, year) %>%
  mutate(sample_id = row_number())

message(sprintf("  Sampled %d (iso3c, year) pairs:", nrow(sample_keys)))
sample_keys %>%
  mutate(region_label = if_else(is.na(dco_region), "(non-RC)", dco_region)) %>%
  count(region_label) %>%
  arrange(region_label) %>%
  { for (i in seq_len(nrow(.)))
    message(sprintf("    %-20s %d pairs", .[[1]][i], .[[2]][i])) }

# -----------------------------
# B2) Extract spot-check cells
# -----------------------------

message("\nExtracting spot-check variables from panel...")

# Only keep spot-check vars that actually exist in the panel
present_spot_vars <- spot_check_vars[spot_check_vars %in% names(panel)]
absent_spot_vars  <- spot_check_vars[!spot_check_vars %in% names(panel)]

if (length(absent_spot_vars) > 0L) {
  message("  NOTE: the following spot-check variables were not found in the panel:")
  message("  ", paste(absent_spot_vars, collapse = ", "))
}

# Join sampled keys back to the panel
spot_check_df <- sample_keys %>%
  left_join(
    panel %>% select(iso3c, year, all_of(unname(present_spot_vars))),
    by = c("iso3c", "year")
  ) %>%
  # Add an empty column for the analyst to fill
  mutate(
    manual_verified_value = NA_character_,
    verification_source   = NA_character_,
    discrepancy_flag      = NA_character_,
    notes                 = NA_character_
  ) %>%
  # Reorder: identifiers first, then spot-check vars, then manual columns
  select(
    sample_id, country_name, iso3c, year, dco_region,
    all_of(unname(present_spot_vars)),
    manual_verified_value, verification_source, discrepancy_flag, notes
  )

message(sprintf("  Spot-check workbook: %d rows x %d cols",
                nrow(spot_check_df), ncol(spot_check_df)))

# -----------------------------
# B3) Build source lookup sheet
# -----------------------------

# Restrict lookup table to variables present in the panel
key_lookup <- source_lookup %>%
  filter(variable %in% unname(present_spot_vars)) %>%
  left_join(
    tibble(
      variable     = unname(present_spot_vars),
      column_label = names(present_spot_vars)
    ),
    by = "variable"
  ) %>%
  select(column_label, variable, source_label, lookup_url) %>%
  arrange(column_label)

# -----------------------------
# B4) Write workbook
# -----------------------------

message("\nWriting spot-check workbook...")

wb_path <- file.path(diagnostics_dir, "22_spot_check_workbook.xlsx")

wb <- createWorkbook()

# --- Sheet 1: spot_check ---
addWorksheet(wb, "spot_check")
writeData(wb, "spot_check", spot_check_df)

# Header style: bold, light blue fill
header_style <- createStyle(
  fontColour  = "#FFFFFF",
  fgFill      = "#2E75B6",
  halign      = "left",
  textDecoration = "bold"
)
addStyle(wb, "spot_check", header_style,
         rows = 1, cols = seq_len(ncol(spot_check_df)), gridExpand = TRUE)

# Alternating row shading for readability
alt_style <- createStyle(fgFill = "#EBF3FB")
for (i in seq(2, nrow(spot_check_df) + 1, by = 2)) {
  addStyle(wb, "spot_check", alt_style,
           rows = i, cols = seq_len(ncol(spot_check_df)), gridExpand = TRUE)
}

# Highlight the four manual-entry columns in yellow
manual_cols <- which(names(spot_check_df) %in%
                       c("manual_verified_value", "verification_source",
                         "discrepancy_flag", "notes"))
manual_style <- createStyle(
  fgFill      = "#FFF2CC",
  fontColour  = "#7F6000",
  textDecoration = "bold"
)
addStyle(wb, "spot_check", manual_style,
         rows = 1, cols = manual_cols, gridExpand = TRUE)

# Auto column widths
setColWidths(wb, "spot_check", cols = seq_len(ncol(spot_check_df)),
             widths = "auto")

# Freeze top row and first two cols
freezePane(wb, "spot_check", firstRow = TRUE, firstCol = FALSE)

# --- Sheet 2: key_lookup ---
addWorksheet(wb, "key_lookup")
writeData(wb, "key_lookup", key_lookup)

key_header_style <- createStyle(
  fontColour     = "#FFFFFF",
  fgFill         = "#375623",
  halign         = "left",
  textDecoration = "bold"
)
addStyle(wb, "key_lookup", key_header_style,
         rows = 1, cols = seq_len(ncol(key_lookup)), gridExpand = TRUE)
setColWidths(wb, "key_lookup", cols = seq_len(ncol(key_lookup)),
             widths = "auto")

# --- Sheet 3: instructions ---
addWorksheet(wb, "instructions")
instructions <- tibble(
  instructions = c(
    "QUALITY CONTROL SPOT-CHECK — HOW TO USE THIS WORKBOOK",
    "",
    "Purpose:",
    "  This workbook contains a stratified random sample of country-year observations",
    "  drawn from the master panel (20_panel_master.RDS). Each row represents one",
    "  (country, year) pair. Your task is to verify a selection of cell values against",
    "  the original data sources listed in the 'key_lookup' sheet.",
    "",
    "Instructions:",
    "  1. For each row in the 'spot_check' sheet, pick ONE variable to verify.",
    "     Prioritise variables where you already have domain knowledge (e.g. you",
    "     know roughly what GDP per capita should be for that country and year).",
    "  2. Look up the true value in the original source using the URL in 'key_lookup'.",
    "  3. Enter the verified value in the 'manual_verified_value' column.",
    "  4. Note the source you consulted in 'verification_source'.",
    "  5. If the panel value differs materially from the source value, enter 'YES'",
    "     in 'discrepancy_flag' and describe the discrepancy in 'notes'.",
    "  6. Save the workbook and pass to 22b_qc_compare.R to generate the discrepancy report.",
    "",
    "Strata:",
    "  The sample is drawn 8 per RC region (AFR, APA, ARAB, ECA, LAC) plus 4 from",
    "  non-RC countries, for a total of 44 spot-check rows.",
    "",
    "What counts as a material discrepancy?",
    "  - GDP per capita: > 5% difference",
    "  - Population: > 3% difference",
    "  - Binary indicators (coup, conflict): any mismatch",
    "  - Index scores (polity2, v2x_polyarchy): > 0.5 units or > 5% of range",
    "  - Rates (unemployment, inflation): > 1 percentage point",
    "",
    "Generated by: 22_qc_spot_check.R",
    paste0("Generated on: ", Sys.time())
  )
)
writeData(wb, "instructions", instructions, colNames = FALSE)
setColWidths(wb, "instructions", cols = 1, widths = 90)

# Title row
title_style <- createStyle(
  fontSize       = 13,
  textDecoration = "bold",
  fontColour     = "#2E75B6"
)
addStyle(wb, "instructions", title_style, rows = 1, cols = 1)

saveWorkbook(wb, wb_path, overwrite = TRUE)

# -----------------------------
# B5) Console summary
# -----------------------------

message(strrep("=", 60))
message("22_qc_spot_check.R complete.")
message(strrep("-", 60))
message("PART A outputs:")
message("  ", file.path(diagnostics_dir, "22_structural_validation.csv"))
message("  ", file.path(diagnostics_dir, "22_coverage_by_source.csv"))
message("\nPART B output:")
message("  ", wb_path)
message(strrep("-", 60))
message(sprintf("Structural validation: %d PASS | %d WARN | %d FAIL",
                n_pass, n_warn, n_fail))
message(sprintf("Spot-check workbook:   %d rows sampled across %d regions",
                nrow(spot_check_df),
                length(rc_regions) + 1L))
message("\nNext step: fill in 'manual_verified_value' and run 22b_qc_compare.R")