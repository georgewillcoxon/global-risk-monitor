############################################################
# Global Risk Panel — Annual Pipeline Orchestrator
#
# Script:  annual_pipeline.R
# Purpose: Run the full data pipeline from scaffold construction
#          through master panel assembly and missingness
#          diagnostics. Pipeline.R
#          architecture: stage flags, dependency map, isolated
#          execution environments via local({source(...)}), and
#          a run log written to output/reports/.
#
# Usage:
#   1. From an interactive R session, set the working directory
#      to ~/global_risk (or rely on {here} resolving from any
#      file inside the project).
#   2. Edit the `run` flags below to control which stages execute.
#      For a full clean run, leave all TRUE. For incremental
#      updates, set heavy stages (e.g. GED, ACLED, CCKP, UNHCR)
#      to FALSE if their cached outputs are still current.
#   3. source("scripts/pipeline.R")
#
# Dependency map (downstream stages assume upstream outputs exist):
#
#   01_panel_base ──► (everything downstream needs this)
#                ├──► 02_features_wdi
#                ├──► 02b_features_wgi   (QoG bulk download — manual trigger)
#                ├──► 03_features_imf
#                ├──► 04_features_vdem_polity   (also needs gwcode→ccode)
#                ├──► 05_features_population
#                ├──► 06_features_governance
#                ├──► 07_features_conflict
#                ├──► 08_features_instability
#                ├──► 08b_features_reign
#                ├──► 08c_features_navco
#                ├──► 09_features_climate
#                ├──► 10_features_humanitarian
#                ├──► 11_features_demographic
#                ├──► 11b_gap_filling    (RR crises, SIPRI, MONA, WRP — manual files)
#                ├──► 11c_crisis_extension
#                ├──► 11d_features_creg
#                ├──► 12_features_opinion (WVS/QoG opinion, GDELT — needs quarterly pipeline)
#                │
#                └──► 20_assemble_panel  (needs 02–12)
#                          │
#                          └──► 21_missingness_report
#
# Heavy / network-dependent stages (slowest):
#   - 03_features_imf    (IMF SDMX, sequential calls per series)
#   - 07_features_conflict (UCDP/GED full pull with pagination)
#   - 08_features_instability (ACLED full pull)
#   - 09_features_climate (CCKP per-country loop)
#
# Author: George Frederick Willcoxon
############################################################

rm(list = ls())

# -----------------------------
# 0) Run flags
# -----------------------------
#
# Set to FALSE to skip a stage. Skipped stages still need their
# upstream outputs to exist on disk for downstream stages to
# work — pipeline.R does not validate that.

run <- list(
  stage_01_panel_base          = TRUE,
  stage_02_features_wdi        = TRUE,
  stage_02b_features_wgi       = TRUE,
  stage_03_features_imf        = TRUE,
  stage_04_features_vdem       = TRUE,
  stage_05_features_pop        = TRUE,
  stage_06_features_gov        = TRUE,
  stage_07_features_conflict   = TRUE,
  stage_08_features_instab     = TRUE,
  stage_08b_features_reign     = TRUE,
  stage_08c_features_navco     = TRUE,
  stage_09_features_climate    = TRUE,
  stage_10_features_humanitar  = TRUE,
  stage_11_features_demog      = TRUE,
  stage_11b_gap_filling        = TRUE,
  stage_11c_crisis_extension   = TRUE,
  stage_11d_features_creg      = TRUE,
  stage_12_features_opinion    = TRUE,
  stage_20_assemble            = TRUE,
  stage_21_missingness         = TRUE
)

# -----------------------------
# 1) Setup
# -----------------------------

library(here)

scripts_dir <- here("scripts", "annual")
reports_dir <- here("output", "reports")
dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)

# Run log
log_file <- file.path(
  reports_dir,
  paste0("pipeline_log_", format(Sys.Date(), "%Y_%m_%d"), ".txt")
)
log_con <- file(log_file, open = "wt")
log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%H:%M:%S"), "  ",
                paste0(..., collapse = ""))
  message(msg)
  writeLines(msg, log_con)
  flush(log_con)
}

log_msg("=== Global Risk Panel annual pipeline run started ===")
log_msg("R version:        ", R.version.string)
log_msg("Working directory: ", getwd())
log_msg("here() root:      ", here())

# -----------------------------
# 2) Run a stage in an isolated environment
# -----------------------------

run_stage <- function(flag_name, script_basename) {
  if (!isTRUE(run[[flag_name]])) {
    log_msg("SKIP  ", script_basename)
    return(invisible(NULL))
  }
  script_path <- file.path(scripts_dir, script_basename)
  if (!file.exists(script_path)) {
    log_msg("MISS  ", script_basename, " — file not found, skipping")
    return(invisible(NULL))
  }
  log_msg("START ", script_basename)
  t0 <- Sys.time()
  res <- tryCatch({
    local({
      source(script_path, local = TRUE, echo = FALSE)
    })
    "OK"
  },
  error = function(e) {
    paste0("ERROR: ", conditionMessage(e))
  })
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  if (res == "OK") {
    log_msg("DONE  ", script_basename, "  (", dt, "s)")
  } else {
    log_msg("FAIL  ", script_basename, "  (", dt, "s)  ", res)
  }
  invisible(NULL)
}

# -----------------------------
# 3) Execute pipeline in dependency order
# -----------------------------

# Stage 01 — base scaffold
run_stage("stage_01_panel_base",         "01_panel_base.R")

# Stages 02–12 — feature scripts (independent of each other,
# except script 10 which reads 02's output for derived ratios)
run_stage("stage_02_features_wdi",       "02_features_wdi.R")
run_stage("stage_02b_features_wgi",      "02b_features_wgi.R")
run_stage("stage_03_features_imf",       "03_features_imf.R")
run_stage("stage_04_features_vdem",      "04_features_vdem_polity.R")
run_stage("stage_05_features_pop",       "05_features_population.R")
run_stage("stage_06_features_gov",       "06_features_governance.R")
run_stage("stage_07_features_conflict",  "07_features_conflict.R")
run_stage("stage_08_features_instab",    "08_features_instability.R")
run_stage("stage_08b_features_reign",    "08b_features_reign.R")
run_stage("stage_08c_features_reign",    "08c_features_navco.R")
run_stage("stage_09_features_climate",   "09_features_climate.R")
run_stage("stage_10_features_humanitar", "10_features_humanitarian.R")
run_stage("stage_11_features_demog",     "11_features_demographic.R")
run_stage("stage_11b_gap_filling",       "11b_gap_filling.R")
run_stage("stage_11c_crisis_extension",  "11c_crisis_extension.R")
run_stage("stage_11d_features_creg",     "11d_features_creg.R")
run_stage("stage_12_features_opinion",   "12_features_opinion.R")

# Stage 20 — master panel assembly (depends on 02–12)
run_stage("stage_20_assemble",           "20_assemble_panel.R")

# Stage 21 — missingness diagnostic (depends on 20)
run_stage("stage_21_missingness",        "21_missingness_report.R")

# -----------------------------
# 4) Wrap up
# -----------------------------

log_msg("=== Annual pipeline run complete ===")
close(log_con)

message("\nLog written to: ", log_file)