# Global Risk Monitor

An open-source early-warning system for country-level risk of armed conflict, economic crisis, political instability, and humanitarian crisis. The forecasting stage is in development and is benchmarked against [VIEWS](https://viewsforecasting.org/), the Violence & Impacts Early-Warning System. The project extends a random-forest model for the Arab region that I co-authored with Håvard Mokleiv Nygård at UN ESCWA in 2019 to a global country-year panel reaching back to 1946, with a quarterly panel alongside it.

**Author:** George Frederick Willcoxon, Ph.D. | [georgewillcoxon.com](https://georgewillcoxon.com)

## What this repository is

This is a **method sampler**, not the full system. It publishes the parts of the data pipeline that show how the panel is built and verified, so that the engineering can be inspected without redistributing the assembled panel, the full feature set, or the forecasting layer. Six scripts are included; the complete pipeline has about thirty.

| File | Stage | What it shows |
|---|---|---|
| `scripts/annual/annual_pipeline.R` | Orchestrator | Stage flags, the dependency map, isolated execution of each stage in its own environment, and a run log. The map lists every stage in the full pipeline, including those not published here. |
| `scripts/annual/00_country_crosswalk.R` | 00 | The canonical country-year reference from 1946: Gleditsch–Ward, COW, ISO, and UN codes reconciled into one crosswalk that is comprehensive and not subject to analytic scoping. |
| `scripts/annual/01_panel_base.R` | 01 | The analytic scaffold: scoping decisions (microstates, year window, regional restriction) applied to the crosswalk, kept deliberately separate from the reference it draws on. |
| `scripts/annual/07_features_conflict.R` | 07 | One representative feature script of nineteen: UCDP armed-conflict and georeferenced-event data via API, conflict-history variables, and a contiguity-based spillover measure. Credentials are read from the environment, never from the script. |
| `scripts/annual/20_assemble_panel.R` | 20 | Assembly of the master country-year panel with a row-count assertion after every join and a running column count, so a silent many-to-many merge cannot pass. |
| `scripts/annual/22_qc_spot_check.R` | 22 | Quality control in two parts: automated structural validation with hard stops on critical failures and soft warnings on coverage anomalies, and a stratified random spot-check workbook for human review. |

Not included: the raw data, the assembled panels, the other feature scripts (World Bank, IMF, V-Dem and Polity, UN population, Correlates of War, Archigos, REIGN, NAVCO, ACLED, climate, UNHCR, OECD, and others), the missingness report and codebook generators, the quarterly pipeline, and the forecasting stage.

## Design

The pipeline is a sequence of numbered, self-contained R scripts. Each stage reads the outputs of the stages before it, writes its own output and a coverage diagnostic, and is run in isolation by the orchestrator. The scaffold is built once from a canonical crosswalk; feature scripts add columns to it, never rows; assembly asserts that the row count is unchanged after every join; quality control then tests the structure, samples the content for human review, and documents every variable. Verification is designed into the pipeline rather than added afterwards: hard stops where an error would invalidate everything downstream, warnings where it would not, and diagnostics written to disk at every stage so that a number in the final panel can be traced to the script and the source that produced it.

## Data and licences

No data are redistributed. The scripts fetch from each source under the reader's own credentials and terms. UCDP requires an API token (`UCDP_TOKEN` in `~/.Renviron`); ACLED, used in stages not published here, requires registration and restricts redistribution of its data. World Bank, IMF, V-Dem, Polity, UN population, and Correlates of War data are fetched or loaded under their respective licences. The code in this repository is released under the MIT License.

## Status

Data pipeline: annual panel assembled and quality-checked (1946 to the present, roughly 230 variables); quarterly panel assembled. Forecasting stage: in development, with the VIEWS benchmark as the evaluation standard. Working method: the code is written with a frontier AI model as the coding assistant, under the division of labor described in [Vibecoding an Oil-Price Shock Analysis](https://www.linkedin.com/pulse/vibecoding-oil-price-shock-analysis-george-willcoxon-ph-d--u9u9c/): the model writes the code, the author supplies the research design, the data judgment, and the verification.
