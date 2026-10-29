############################################################
# Global Risk Panel — Stage 07: Conflict Features
#
# Script:  07_features_conflict.R
# Purpose: Pull armed conflict features from UCDP datasets:
#            - UCDP/PRIO Armed Conflict Dataset (ACD), via
#              peacesciencer::add_ucdp_acd() — country-year
#              conflict status, onset, intensity
#            - UCDP Georeferenced Event Dataset (GED), via
#              direct API — incident counts and battle deaths
#              aggregated to country-year
#          Plus conflict-history features:
#            - Years since last conflict
#            - Lagged conflict indicator
#            - Conflict neighborhood spillover (share of
#              contiguous neighbors currently in conflict)
#
# Sources:
#   peacesciencer::add_ucdp_acd()      Internal armed conflict
#                                       (gov + territory)
#   peacesciencer::add_ucdp_onsets()   UCDP onset variables
#   peacesciencer::add_contiguity()    COW direct contiguity
#   UCDP/GED API at https://ucdpapi.pcr.uu.se/
#
# Indicators produced:
#   ucdp_ongoing       1 if ongoing internal conflict in year, 0 else
#   ucdp_onset         1 if conflict onset in year (after >=2y peace)
#   ucdp_maxintensity  1 = minor; 2 = war (>=1000 BRD)
#   ucdp_n_conflicts   Number of distinct conflict episodes that year
#   peace_years        Years since last conflict
#   ucdp_ged_incidents Count of GED incidents in country-year
#   ucdp_ged_brd_best  Sum of best estimate battle-related deaths
#   ucdp_ged_brd_low   Sum of low estimate
#   ucdp_ged_brd_high  Sum of high estimate
#   neigh_n_inconflict Number of contiguous land neighbors in conflict
#   neigh_share_inconf Share of contiguous land neighbors in conflict
#
# Note on GED:
#   GED has near-global coverage from 1989, but Syria 2011+
#   and a few other cases have data quality caveats. We do
#   NOT zero-fill missing GED — leave as NA so downstream
#   analysis can decide how to handle.
#
# Outputs:
#   output/excel/07_features_conflict.csv
#   output/excel/07_features_conflict.RDS
#   output/diagnostics/07_conflict_coverage.csv
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
library(httr)
library(jsonlite)
library(purrr)
library(peacesciencer)
library(countrycode)

# -----------------------------
# 1) Settings
# -----------------------------

start_year <- 1946L

project_root       <- here()
output_dir         <- file.path(project_root, "output/excel/annual")
diagnostics_dir    <- file.path(project_root, "output/diagnostics")

dir.create(output_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2) Load panel base
# -----------------------------

panel_base <- readRDS(file.path(output_dir, "01_panel_base.RDS"))

# -----------------------------
# 3) UCDP ACD via peacesciencer
# -----------------------------

message("Building UCDP ACD features via peacesciencer::add_ucdp_acd()...")

# add_ucdp_acd() with type = "intrastate" and "internationalized interstate" returns:
#   ucdpongoing, ucdponset, maxintensity, conflict_ids

acd <- create_stateyears(system = "gw") %>%
  filter(year >= start_year) %>%
  add_ucdp_acd(type = c("intrastate", "II"), only_wars = FALSE)

# Count number of distinct conflict IDs in conflict_ids string
# (this is a comma-separated list when there are multiple)
acd <- acd %>%
  mutate(
    ucdp_n_conflicts = if_else(
      is.na(conflict_ids) | conflict_ids == "",
      0L,
      lengths(strsplit(conflict_ids, ";"))   # peacesciencer separator is ;
    )
  ) %>%
  rename(
    ucdp_ongoing      = ucdpongoing,
    ucdp_onset        = ucdponset,
    ucdp_maxintensity = maxintensity
  ) %>%
  select(gwcode, year, ucdp_ongoing, ucdp_onset,
         ucdp_maxintensity, ucdp_n_conflicts)

message("ACD: ", nrow(acd), " rows.")

# -----------------------------
# 4) Compute peace years (years since last conflict)
# -----------------------------

acd <- acd %>%
  arrange(gwcode, year) %>%
  group_by(gwcode) %>%
  mutate(
    # Reset counter at every conflict year; otherwise increment
    peace_years = {
      pyrs <- integer(length(year))
      counter <- 0L
      for (i in seq_along(year)) {
        if (!is.na(ucdp_ongoing[i]) && ucdp_ongoing[i] == 1L) {
          counter <- 0L
        } else {
          counter <- counter + 1L
        }
        pyrs[i] <- counter
      }
      pyrs
    }
  ) %>%
  ungroup()

# -----------------------------
# 5) UCDP/GED via direct API
# -----------------------------
#
# UCDP/GED API base URL: https://ucdpapi.pcr.uu.se/api/gedevents/
# For most recent version, see https://ucdp.uu.se/downloads/
#
# Pagination: 1000 events per page, follow NextPageUrl.
# Returns: a JSON object with $Result containing event records.
# Key fields: country_id (GW code), year, best, low, high, type_of_violence
#
# Pulling all events globally from 1989 is large (~250k events,
# ~50 MB). We loop with pagination and aggregate to country-year.

ged_version <- "25.1"  # update as new UCDP versions are released
ged_base    <- paste0("https://ucdpapi.pcr.uu.se/api/gedevents/",
                      ged_version)

# UCDP API token — store in your .Renviron file as:
#   UCDP_TOKEN=your-token-here
# Never hard-code the token in the script.
ucdp_token <- Sys.getenv("UCDP_TOKEN")
if (nchar(ucdp_token) == 0L) {
  warning(
    "UCDP_TOKEN environment variable is not set.\n",
    "Add UCDP_TOKEN=<your-token> to ~/.Renviron and restart R.\n",
    "The API will return 401 Unauthorized without a valid token."
  )
}

# Columns needed from GED. The full dataset has ~30 columns;
# selecting upfront reduces memory and parse time significantly.
# Large text fields (where_description, source_article, etc.)
# are excluded — they are not needed for country-year aggregation.
#
#   id               Unique event ID (for deduplication)
#   country_id       GW state code (panel join key)
#   year             Calendar year
#   type_of_violence 1=state-based, 2=non-state, 3=one-sided
#   best             Best estimate battle-related deaths
#   low              Low estimate
#   high             High estimate
ged_keep_cols <- c("id", "country_id", "year",
                   "type_of_violence", "best", "low", "high")

fetch_ged_events <- function() {
  url    <- paste0(ged_base, "?pagesize=1000")
  events <- list()
  page   <- 1L
  
  while (!is.null(url) && nchar(url) > 0L) {
    if (page %% 10L == 0L) message("  GED page ", page, "...")
    
    res <- tryCatch(
      GET(
        url,
        add_headers("x-ucdp-access-token" = ucdp_token),
        timeout(180)
      ),
      error = function(e) NULL
    )
    
    if (is.null(res)) {
      warning("GED page ", page, ": request returned NULL.")
      break
    }
    if (status_code(res) == 401L) {
      stop(
        "UCDP API returned 401 Unauthorized.\n",
        "Check that UCDP_TOKEN is set in .Renviron and restart R."
      )
    }
    if (status_code(res) != 200L) {
      warning("GED page ", page, ": HTTP ", status_code(res), ".")
      break
    }
    
    parsed <- content(res, as = "parsed", simplifyVector = TRUE)
    
    if (!is.null(parsed$Result) && length(parsed$Result) > 0L) {
      # Pre-select needed columns before storing to avoid
      # accumulating large text fields across 250+ pages.
      page_df   <- as_tibble(parsed$Result)
      available <- intersect(ged_keep_cols, names(page_df))
      events[[page]] <- page_df %>% select(all_of(available))
    }
    
    url  <- parsed$NextPageUrl %||% ""
    page <- page + 1L
  }
  
  bind_rows(events)
}

message("Pulling UCDP/GED v", ged_version,
        " events — this can take 5–15 minutes...")

# Manual fallback path. The GED CSV can be downloaded from:
#   https://ucdp.uu.se/downloads/
#   → UCDP Georeferenced Event Dataset (GED) Global → CSV
# Save to data/manual/GEDEvent_v<version>.csv
# e.g. data/manual/GEDEvent_v25_1.csv
# Manual CSV filename uses underscores: e.g. GEDEvent_v25_1.csv
ged_version_file <- gsub("\\.", "_", ged_version)
ged_manual_path <- here("data", "manual",
                        paste0("GEDEvent_v", ged_version_file, ".csv"))

ged_raw <- NULL

# Try API first
message("  Trying UCDP GED API...")
ged_raw <- tryCatch(fetch_ged_events(), error = function(e) {
  message("  API error: ", conditionMessage(e))
  NULL
})

# If API returned nothing, fall back to manual CSV
if (is.null(ged_raw) || nrow(ged_raw) == 0L) {
  if (file.exists(ged_manual_path)) {
    message("  API unavailable — loading from manual file: ", ged_manual_path)
    ged_raw <- tryCatch(
      read_csv(ged_manual_path, show_col_types = FALSE),
      error = function(e) {
        warning("Failed to read manual GED file: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(ged_raw)) {
      message("  Manual GED file loaded: ", nrow(ged_raw), " events.")
    }
  } else {
    warning(
      "UCDP GED API unavailable and no manual file found at:\n  ",
      ged_manual_path, "\n",
      "Download GED CSV from https://ucdp.uu.se/downloads/ and save to that path.\n",
      "GED indicators (incidents, battle deaths) will be NA for this run."
    )
  }
} else {
  message("GED API returned ", nrow(ged_raw), " events.")
}

# Aggregate to country-year. country_id in UCDP/GED is the
# GW state code. Column names are identical between the API
# response and the CSV download.

if (!is.null(ged_raw) && nrow(ged_raw) > 0L) {
  ged_agg <- ged_raw %>%
    filter(year >= start_year) %>%
    mutate(
      best = suppressWarnings(as.numeric(best)),
      low  = suppressWarnings(as.numeric(low)),
      high = suppressWarnings(as.numeric(high))
    ) %>%
    group_by(country_id, year) %>%
    summarise(
      ucdp_ged_incidents = n(),
      ucdp_ged_brd_best  = sum(best, na.rm = TRUE),
      ucdp_ged_brd_low   = sum(low,  na.rm = TRUE),
      ucdp_ged_brd_high  = sum(high, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(gwcode = country_id) %>%
    mutate(gwcode = as.integer(gwcode),
           year   = as.integer(year))
  message("GED aggregated: ", nrow(ged_agg), " country-year rows.")
} else {
  ged_agg <- tibble(
    gwcode             = integer(0),
    year               = integer(0),
    ucdp_ged_incidents = integer(0),
    ucdp_ged_brd_best  = numeric(0),
    ucdp_ged_brd_low   = numeric(0),
    ucdp_ged_brd_high  = numeric(0)
  )
  message("GED unavailable — GED columns will be NA in output.")
}

# -----------------------------
# 6) Build neighborhood spillover via add_contiguity
# -----------------------------
#
# add_contiguity() is COW-keyed — it merges on ccode1/ccode2.
# We use system = "cow" for the dyadic pull, then convert
# ccode back to gwcode via the crosswalk so the spillover
# frame joins onto the GW-keyed acd frame.

message("Building neighborhood spillover...")

dyads <- create_dyadyears(system = "cow", directed = TRUE) %>%
  filter(year >= start_year) %>%
  add_contiguity()

# conttype = 1 means direct land contiguity
dyads_land <- dyads %>%
  filter(!is.na(conttype) & conttype == 1L) %>%
  select(ccode1, ccode2, year) %>%
  # Convert ccode1/ccode2 to gwcode by joining panel_base on
  # (ccode, year). Joining on year as well as ccode eliminates
  # the many-to-many: each (ccode, year) pair maps to exactly
  # one gwcode in panel_base (e.g. ccode 345 = gwcode 345 in
  # 1992 but gwcode 340 in 2010).
  left_join(
    panel_base %>%
      filter(!is.na(ccode)) %>%
      distinct(ccode, year, gwcode) %>%
      group_by(ccode, year) %>%
      slice_max(gwcode, n = 1L, with_ties = FALSE) %>%
      ungroup() %>%
      rename(ccode1 = ccode, gwcode1 = gwcode),
    by = c("ccode1", "year")
  ) %>%
  left_join(
    panel_base %>%
      filter(!is.na(ccode)) %>%
      distinct(ccode, year, gwcode) %>%
      group_by(ccode, year) %>%
      slice_max(gwcode, n = 1L, with_ties = FALSE) %>%
      ungroup() %>%
      rename(ccode2 = ccode, gwcode2 = gwcode),
    by = c("ccode2", "year")
  ) %>%
  filter(!is.na(gwcode1), !is.na(gwcode2)) %>%
  select(gwcode1, gwcode2, year)

# Merge in side2's conflict status
dyads_land <- dyads_land %>%
  left_join(
    acd %>% select(gwcode, year, ucdp_ongoing) %>%
      rename(gwcode2 = gwcode, neighbor_inconf = ucdp_ongoing),
    by = c("gwcode2", "year")
  )

# Aggregate to country-year
spillover <- dyads_land %>%
  group_by(gwcode1, year) %>%
  summarise(
    neigh_n_total       = n(),
    neigh_n_inconflict  = sum(neighbor_inconf == 1L, na.rm = TRUE),
    neigh_share_inconf  = if_else(
      neigh_n_total > 0L,
      neigh_n_inconflict / neigh_n_total,
      NA_real_
    ),
    .groups             = "drop"
  ) %>%
  rename(gwcode = gwcode1)

# -----------------------------
# 7) Merge everything into panel
# -----------------------------

conflict_features <- panel_base %>%
  select(gwcode, iso3c, year) %>%
  left_join(acd,        by = c("gwcode", "year")) %>%
  left_join(ged_agg,    by = c("gwcode", "year")) %>%
  left_join(spillover,  by = c("gwcode", "year")) %>%
  select(-gwcode) %>%
  arrange(iso3c, year)

# ---------------------------------------------------------------
# Post-merge imputations
# ---------------------------------------------------------------
#
# 1. GED zero-fill for peaceful country-years within GED coverage.
#    GED exists from 1989. For years >= 1989, a country-year that
#    ACD codes as having no ongoing conflict (ucdp_ongoing == 0)
#    and where GED returned no events (NA after left_join) can
#    legitimately be coded zero — no events means zero incidents
#    and zero battle deaths.
#    For years < 1989 the NA must remain: GED simply does not
#    cover that period and zero would be false precision.
#
# 2. ucdp_maxintensity zero-fill for non-conflict years.
#    ACD populates maxintensity only for years with ongoing conflict
#    (1 = minor, 2 = war). For country-years where ucdp_ongoing == 0
#    the correct value is 0 (no intensity), not NA. We apply this
#    wherever ACD has coverage (ucdp_ongoing is non-NA), across the
#    full ACD time series (1946+).

ged_start_year <- 1989L

conflict_features <- conflict_features %>%
  mutate(
    # GED incidents: zero only within GED coverage window and
    # only for confirmed non-conflict country-years.
    ucdp_ged_incidents = if_else(
      year >= ged_start_year &
        !is.na(ucdp_ongoing) & ucdp_ongoing == 0L &
        is.na(ucdp_ged_incidents),
      0L,
      ucdp_ged_incidents
    ),
    ucdp_ged_brd_best = if_else(
      year >= ged_start_year &
        !is.na(ucdp_ongoing) & ucdp_ongoing == 0L &
        is.na(ucdp_ged_brd_best),
      0,
      ucdp_ged_brd_best
    ),
    ucdp_ged_brd_low = if_else(
      year >= ged_start_year &
        !is.na(ucdp_ongoing) & ucdp_ongoing == 0L &
        is.na(ucdp_ged_brd_low),
      0,
      ucdp_ged_brd_low
    ),
    ucdp_ged_brd_high = if_else(
      year >= ged_start_year &
        !is.na(ucdp_ongoing) & ucdp_ongoing == 0L &
        is.na(ucdp_ged_brd_high),
      0,
      ucdp_ged_brd_high
    ),
    # maxintensity: 0 wherever ACD has coverage and conflict is absent.
    ucdp_maxintensity = if_else(
      !is.na(ucdp_ongoing) & ucdp_ongoing == 0L &
        is.na(ucdp_maxintensity),
      0L,
      ucdp_maxintensity
    )
  )

# Validation
stopifnot(
  nrow(conflict_features) == nrow(distinct(conflict_features, iso3c, year))
)

message(
  "Conflict features: ", nrow(conflict_features), " rows, ",
  length(unique(conflict_features$iso3c)), " ISO3 codes."
)

# -----------------------------
# Final source-boundary cleanup for UCDP ACD-derived variables
# -----------------------------
#
# If UCDP ACD has no coverage for a panel year, do not treat
# missing conflict status as peace. Keep all ACD-derived fields
# missing for those years.

acd_coverage_years <- acd %>%
  group_by(year) %>%
  summarise(
    acd_has_any_data = any(!is.na(ucdp_ongoing)),
    .groups = "drop"
  )

conflict_features <- conflict_features %>%
  left_join(acd_coverage_years, by = "year") %>%
  mutate(
    ucdp_ongoing      = if_else(isFALSE(acd_has_any_data) | is.na(acd_has_any_data), NA_integer_, as.integer(ucdp_ongoing)),
    ucdp_onset        = if_else(isFALSE(acd_has_any_data) | is.na(acd_has_any_data), NA_integer_, as.integer(ucdp_onset)),
    ucdp_maxintensity = if_else(isFALSE(acd_has_any_data) | is.na(acd_has_any_data), NA_integer_, as.integer(ucdp_maxintensity)),
    ucdp_n_conflicts  = if_else(isFALSE(acd_has_any_data) | is.na(acd_has_any_data), NA_integer_, as.integer(ucdp_n_conflicts)),
    peace_years       = if_else(isFALSE(acd_has_any_data) | is.na(acd_has_any_data), NA_integer_, as.integer(peace_years))
  ) %>%
  select(-acd_has_any_data)


# -----------------------------
# 8) Coverage diagnostic
# -----------------------------

coverage <- conflict_features %>%
  pivot_longer(
    cols      = -c(iso3c, year),
    names_to  = "indicator",
    values_to = "value"
  ) %>%
  group_by(indicator) %>%
  summarise(
    n_obs       = sum(!is.na(value)),
    n_total     = n(),
    coverage    = round(100 * n_obs / n_total, 1L),
    .groups     = "drop"
  ) %>%
  arrange(desc(coverage))

write_csv(coverage, file.path(diagnostics_dir, "07_conflict_coverage.csv"))

# -----------------------------
# 9) Write outputs
# -----------------------------

write_csv(conflict_features, file.path(output_dir, "07_features_conflict.csv"))
saveRDS(conflict_features,   file.path(output_dir, "07_features_conflict.RDS"))

message(
  "07_features_conflict.R complete. Outputs:\n",
  "  ", file.path(output_dir, "07_features_conflict.csv"), "\n",
  "  ", file.path(output_dir, "07_features_conflict.RDS"), "\n",
  "  ", file.path(diagnostics_dir, "07_conflict_coverage.csv")
)