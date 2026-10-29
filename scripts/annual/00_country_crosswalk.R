############################################################
# Global Risk Panel — Stage 00: Canonical Country Crosswalk
#
# Script:  00_country_crosswalk.R
# Purpose: Build the canonical country-year scaffold and the
#          single source of truth for all country-code
#          reconciliation. Every downstream script left-joins
#          its features to this scaffold via (iso3c, year). No
#          downstream script may call countrycode() or do its
#          own gwcode/ccode/iso3n reconciliation. If a script
#          needs another code system (gwcode, ccode, iso3n,
#          imf, wb, wb_api3c, vdem, wvs), it pulls that column
#          from this crosswalk.
#
# Master state system:
#   The Gleditsch-Ward (GW) state system is the master. A
#   country-year exists in the panel if and only if GW
#   recognizes it as an independent state in that year.
#   Microstates (GW microstate flag, population < 250,000)
#   are included for now, and can be removed in the next script.
#
# Temporal coverage: 1946 onwards.
#
# State-succession philosophy:
#
#   The fundamental tension in country-coded risk panels is
#   that GW assigns one continuous gwcode to entities that
#   went through state-level political transitions (e.g. USSR
#   gwcode 365 carries through to modern Russia), while data
#   sources like WDI, IMF, and V-Dem treat the predecessor
#   and successor as different entities. Forcing a single
#   modern iso3c onto the entire gwcode lifespan would either
#   produce decades of NA (if WDI's modern series only starts
#   at the transition year) or produce false equivalence (if
#   WDI backcasts modern data into the predecessor period).
#
#   The 2024–2026 IR data literature has converged on using
#   ISO 3166-3 codes for predecessor entities. ISO 3166-3 is
#   the registry of codes for countries that no longer exist
#   as such; it preserves the entity for historical reference.
#   This crosswalk uses these codes for the seven predecessor
#   entities relevant to the 1946+ window:
#
#     SUN  Soviet Union              (gwcode 365, 1946–1991)
#     YUG  Yugoslavia (SFRY)         (gwcode 345, 1946–1991)
#     SCG  Serbia & Montenegro / FRY (gwcode 345, 1992–2005)
#     CSK  Czechoslovakia            (gwcode 315, 1946–1992)
#     DDR  East Germany              (gwcode 265, 1949–1990)
#     YDR  South Yemen (PDR Yemen)   (gwcode 680, 1967–1990)
#     VDR  South Vietnam             (gwcode 817, 1955–1975)
#
#   For each of these, the gwcode→iso3c mapping is
#   year-conditional: the same gwcode may produce different
#   iso3c values for different year ranges. The COW ccode
#   stays continuous (per COW's own conventions), so
#   peacesciencer-keyed sources (NMC, V-Dem via gwcode) flow
#   correctly while iso3c-keyed sources (WDI, IMF) see
#   distinct entities.
#
#   For the modern successors:
#
#     RUS  Russia        (gwcode 365, 1992+)
#     SRB  Serbia        (gwcode 340, 2006+; gwcode 345 → SCG before 2006)
#     MNE  Montenegro    (gwcode 341, 2006+)
#     XKX  Kosovo        (gwcode 347, 2008+)
#     CZE  Czechia       (gwcode 316, 1993+)
#     SVK  Slovakia      (gwcode 317, 1993+)
#     YEM  unified Yemen (gwcode 678 throughout — North Yemen carried forward)
#     VNM  unified Vietnam (gwcode 816 throughout — North Vietnam carried forward)
#     DEU  unified Germany (gwcode 260 throughout — FRG carried forward)
#
#   This means the panel will have rows like:
#     gwcode  iso3c  year
#     365     SUN    1946  (USSR)
#     365     SUN    1991  (USSR last year)
#     365     RUS    1992  (Russia)
#     345     YUG    1991  (SFRY last year)
#     345     SCG    1992  (FRY)
#     345     SCG    2005  (Serbia & Montenegro last year)
#     340     SRB    2006  (modern Serbia, distinct gwcode)
#     341     MNE    2006  (Montenegro, distinct gwcode)
#     347     XKX    2008  (Kosovo, distinct gwcode)
#
# Implementation:
#   1. Build GW state-year scaffold from peacesciencer.
#   2. Attach codelist_panel codes (iso3c, ccode, iso3n, imf,
#      wb, wb_api3c, vdem, wvs, fips) via gwcode lookup.
#   3. Apply year-conditional overrides via inline tribble for
#      the seven predecessor entities (replaces iso3c, iso3n,
#      and fips).
#   4. Apply manual patches for codelist_panel bugs (PRK row
#      showing "731", Kosovo missing iso3c, etc.).
#   5. Validate uniqueness on (iso3c, year).
#
# Outputs:
#   output/excel/00_country_crosswalk.csv
#   output/excel/00_country_crosswalk.RDS
#   output/diagnostics/00_crosswalk_audit.csv
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
library(tibble)
library(peacesciencer)
library(countrycode)

# Force UTF-8 locale for countrycode's internal grepl() calls.
# countrycode::codelist_panel contains Latin-1 encoded strings in
# some country name fields; without this, grepl() emits invalid
# UTF-8 warnings on strings 5581-5585 of the internal dictionary.
invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))

# -----------------------------
# 1) Settings
# -----------------------------

start_year <- 1946L
end_year   <- 2025L

project_root       <- here()
output_dir         <- file.path(project_root, "output/excel/annual")
diagnostics_dir    <- file.path(project_root, "output/diagnostics")

dir.create(output_dir,      recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2) Build GW state-year scaffold
# -----------------------------

gw_sy <- create_stateyears(system = "gw") %>%
  filter(year >= start_year, year <= end_year) %>%
  filter(gwcode != 711L, gwcode != 396L, gwcode != 396L, 
         gwcode != 397L, gwcode != 511L) %>%   # exclude Tibet, Abkhazia, South Ossetia, Zanzibar (no covariate data anyway)
  filter(!(gwcode == 345L & year == 2006)) %>% #Delete duplicate observation of Yug & Serbia/Montenegro in 2006
  select(gwcode, year, gw_name, microstate)

message(
  "GW scaffold: ", nrow(gw_sy), " state-year rows, ",
  length(unique(gw_sy$gwcode)), " GW codes, ",
  "years ", min(gw_sy$year), "–", max(gw_sy$year), "."
)

# -----------------------------
# 3) Attach codelist_panel codes
# -----------------------------
#
# countrycode::codelist_panel is the panel version of
# countrycode's master codelist. We collapse it to one row
# per gwn (rather than joining on year) for two reasons:
#
#   (a) codelist_panel has a right boundary (currently ~2020).
#       Joining on (gwn, year) produces NA on all code columns
#       for any panel year past that boundary.
#   (b) For all but a handful of state-transition cases, the
#       codes don't actually vary year-to-year. The cases that
#       DO vary (predecessor entities like USSR, Yugoslavia,
#       Czechoslovakia) are handled explicitly via the
#       year-conditional override tribble in step 4.
#
# So: take one representative row per gwn from codelist_panel
# (preferring the most recent non-NA values), then merge to
# the GW scaffold by gwcode alone.

panel_codes <- codelist_panel %>%
  select(gwn, year,
         iso3c, iso3n, cown,
         imf, wb, wb_api3c, vdem, wvs, fips) %>%
  filter(!is.na(gwn)) %>%
  filter(year >= 1946L) %>%
  arrange(gwn, desc(year)) %>%
  group_by(gwn) %>%
  summarise(
    iso3c = {
      x <- iso3c[!is.na(iso3c)]
      if (length(x) == 0) NA_character_ else x[1]
    },
    iso3n = {
      x <- iso3n[!is.na(iso3n)]
      if (length(x) == 0) NA_real_ else x[1]
    },
    cown = {
      x <- cown[!is.na(cown)]
      if (length(x) == 0) NA_real_ else x[1]
    },
    imf = {
      x <- imf[!is.na(imf)]
      if (length(x) == 0) NA_real_ else x[1]
    },
    wb = {
      x <- wb[!is.na(wb)]
      if (length(x) == 0) NA_character_ else x[1]
    },
    wb_api3c = {
      x <- wb_api3c[!is.na(wb_api3c)]
      if (length(x) == 0) NA_character_ else x[1]
    },
    vdem = {
      x <- vdem[!is.na(vdem)]
      if (length(x) == 0) NA_real_ else x[1]
    },
    wvs = {
      x <- wvs[!is.na(wvs)]
      if (length(x) == 0) NA_real_ else x[1]
    },
    fips = {
      x <- fips[!is.na(fips)]
      if (length(x) == 0) NA_character_ else x[1]
    },
    .groups = "drop"
  ) %>%
  rename(gwcode = gwn, ccode = cown)

risk_panel <- gw_sy %>%
  left_join(panel_codes, by = "gwcode")

# -----------------------------
# 3b) Fallback iso3c via direct countrycode() lookup
# -----------------------------
#
# codelist_panel doesn't carry every gwcode in the GW state
# system — notably, several European and Pacific microstates
# have no entry (Andorra 232, Liechtenstein 223, Monaco 221,
# San Marino 331, Marshall Islands 983, etc.). countrycode()
# can resolve many of these directly via origin="gwn" because
# its underlying dictionary is broader than what gets surfaced
# in codelist_panel.
#
# We fill iso3c only — the other code columns (imf, wb,
# wb_api3c, vdem, wvs) will remain NA for these microstates,
# which is correct: most international data sources don't
# carry them anyway.

unresolved_gwcodes <- risk_panel %>%
  filter(is.na(iso3c)) %>%
  distinct(gwcode) %>%
  pull(gwcode)

if (length(unresolved_gwcodes) > 0L) {
  iso3c_fallback <- tibble(
    gwcode = unresolved_gwcodes,
    iso3c_cc = countrycode(
      unresolved_gwcodes,
      origin       = "gwn",
      destination  = "iso3c",
      warn         = FALSE
    ),
    iso3n_cc = countrycode(
      unresolved_gwcodes,
      origin       = "gwn",
      destination  = "iso3n",
      warn         = FALSE
    )
  )
  
  risk_panel <- risk_panel %>%
    left_join(iso3c_fallback, by = "gwcode") %>%
    mutate(
      iso3c = coalesce(iso3c, iso3c_cc),
      iso3n = coalesce(iso3n, iso3n_cc)
    ) %>%
    select(-iso3c_cc, -iso3n_cc)
}

# -----------------------------
# 4) Year-conditional iso3c overrides for predecessor entities
# -----------------------------
#
# Each row of this tribble specifies a (gwcode, year_min,
# year_max) range and the predecessor-entity iso3c / iso3n to
# apply. ISO 3166-3 numeric codes use the assignments from
# the published ISO standard.

predecessor_overrides <- tribble(
  ~gwcode, ~year_min, ~year_max, ~iso3c_override, ~iso3n_override, ~fips_override, ~entity_name,
  365L,    1946L,     1991L,     "SUN",            810L,            "SX",           "Soviet Union",
  345L,    1946L,     1991L,     "YUG",            890L,            "YI",           "Yugoslavia (SFRY)",
  345L,    1992L,     2005L,     "SCG",            891L,            "YI",           "Serbia & Montenegro / FRY",
  315L,    1946L,     1992L,     "CSK",            200L,            "CZ",           "Czechoslovakia",
  265L,    1949L,     1990L,     "DDR",            278L,            "GE",           "East Germany",
  680L,    1967L,     1990L,     "YDR",            720L,            "YS",           "South Yemen (PDR Yemen)",
  817L,    1954L,     1975L,     "VDR",            704L,            "VS",           "South Vietnam"
)

# Apply overrides: any (gwcode, year) matching a tribble row
# gets its iso3c, iso3n, and fips replaced. For continuation
# gwcodes (365, 345, 315), rows OUTSIDE the year range keep
# their codelist_panel-derived values (RUS, SRB, CZE).
#
# Note on FIPS GE collision: DDR uses FIPS "GE" through 1990.
# Georgia (gwcode 372) also resolves to FIPS "GE" via
# countrycode — but Georgia only enters the GW system in 1992,
# two years after DDR ceases to exist, so there is no overlap.
# Georgia is patched to "GG" in the manual patches below.

apply_overrides <- function(panel, overrides) {
  panel <- panel %>% mutate(.row_id = row_number())
  
  for (i in seq_len(nrow(overrides))) {
    o <- overrides[i, ]
    panel <- panel %>%
      mutate(
        iso3c = if_else(
          gwcode == o$gwcode & year >= o$year_min & year <= o$year_max,
          o$iso3c_override,
          iso3c
        ),
        iso3n = if_else(
          gwcode == o$gwcode & year >= o$year_min & year <= o$year_max,
          o$iso3n_override,
          iso3n
        ),
        fips  = if_else(
          gwcode == o$gwcode & year >= o$year_min & year <= o$year_max,
          o$fips_override,
          fips
        )
      )
  }
  
  panel %>% select(-.row_id)
}

risk_panel <- apply_overrides(risk_panel, predecessor_overrides)

# Predecessor entity rows have no modern source IDs (WDI,
# IMF, etc. don't carry the defunct entities under these
# iso3c codes). Wipe those columns for predecessor rows so
# downstream merges don't try to match RUS data into SUN
# rows. The ccode is RETAINED because COW preserves it
# across the political continuation.

predecessor_iso3c <- predecessor_overrides$iso3c_override

risk_panel <- risk_panel %>%
  mutate(
    imf      = if_else(iso3c %in% predecessor_iso3c, NA_real_,     imf),
    wb       = if_else(iso3c %in% predecessor_iso3c, NA_character_, wb),
    wb_api3c = if_else(iso3c %in% predecessor_iso3c, NA_character_, wb_api3c),
    wvs      = if_else(iso3c %in% predecessor_iso3c, NA_real_,     wvs)
    # fips is NOT wiped: predecessor entities have their own FIPS codes
    # (SX, YI, CZ, GE, YS, VS) set by the overrides above. GDELT uses
    # these codes for events during the predecessor period, so retaining
    # them in the crosswalk is correct and necessary.
  )

# Note: vdem is NOT wiped here. V-Dem actually does carry
# predecessor entities (Yugoslavia, Soviet Union, East
# Germany, etc.) as their own country IDs. We'll attach
# V-Dem-specific predecessor IDs in step 5 below.

# -----------------------------
# 5) Manual patches to codelist_panel
# -----------------------------
#
# codelist_panel has known gaps and quirks. This section
# patches the cases identified in 00_missing_key_codes.csv
# diagnostic.

manual_patches <- tribble(
  ~gwcode, ~iso3c, ~iso3n, ~imf,  ~wb,   ~wb_api3c, ~vdem, ~wvs,  ~fips,  ~note,
  # ----- Bug fixes: countrycode returned the gwcode itself -----
  731L,    "PRK",  408L,   NA,    "PRK", "PRK",     41L,   NA,    "KN",   "North Korea — patched from gwcode-as-string bug",
  816L,    "VNM",  704L,   582L,  "VNM", "VNM",     34L,   704L,  "VM",   "Vietnam (DRV / unified) — patched",
  
  # ----- codelist_panel coverage gaps -----
  347L,    "XKX",  NA,     967L,  "KSV", "XKX",     43L,   915L,  "KV",   "Kosovo — manual entry",
  678L,    "YEM",  887L,   474L,  "YEM", "YEM",     14L,   887L,  "YM",   "Yemen (continuous in GW) — manual entry",
  713L,    "TWN",  158L,   528L,  "TWN", "TWN",     NA,    NA,    "TW",   "Taiwan — WB API code TWN, no IMF membership",

  # ----- Eastern Caribbean microstates -----
  54L,     "DMA",  212L,   321L,  "DMA", "DMA",     NA,    NA,    "DO",   "Dominica",
  55L,     "GRD",  308L,   328L,  "GRD", "GRD",     NA,    NA,    "GJ",   "Grenada",
  56L,     "LCA",  662L,   362L,  "LCA", "LCA",     NA,    NA,    "ST",   "Saint Lucia",
  57L,     "VCT",  670L,   364L,  "VCT", "VCT",     NA,    NA,    "VC",   "Saint Vincent and the Grenadines",
  58L,     "ATG",  28L,    311L,  "ATG", "ATG",     NA,    NA,    "AC",   "Antigua and Barbuda",
  60L,     "KNA",  659L,   361L,  "KNA", "KNA",     NA,    NA,    "SC",   "Saint Kitts and Nevis",
  
  # ----- European microstates -----
  221L,    "MCO",  492L,   NA,    "MCO", "MCO",     NA,    NA,    "MN",   "Monaco — no IMF Article IV",
  223L,    "LIE",  438L,   NA,    "LIE", "LIE",     NA,    NA,    "LS",   "Liechtenstein — not IMF member",
  232L,    "AND",  20L,    NA,    "AND", "AND",     NA,    NA,    "AN",   "Andorra",
  331L,    "SMR",  674L,   311L,  "SMR", "SMR",     NA,    NA,    "SM",   "San Marino",
  
  # ----- Other microstates with codelist_panel gaps -----
  403L,    "STP",  678L,   716L,  "STP", "STP",     NA,    NA,    "TP",   "São Tomé and Príncipe",
  591L,    "SYC",  690L,   718L,  "SYC", "SYC",     NA,    NA,    "SE",   "Seychelles",
  
  # ----- Pacific microstates -----
  935L,    "VUT",  548L,   846L,  "VUT", "VUT",     NA,    NA,    "NH",   "Vanuatu",
  970L,    "KIR",  296L,   826L,  "KIR", "KIR",     NA,    NA,    "KR",   "Kiribati",
  971L,    "NRU",  520L,   NA,    "NRU", "NRU",     NA,    NA,    "NR",   "Nauru",
  972L,    "TON",  776L,   866L,  "TON", "TON",     NA,    NA,    "TN",   "Tonga",
  973L,    "TUV",  798L,   NA,    "TUV", "TUV",     NA,    NA,    "TV",   "Tuvalu",
  983L,    "MHL",  584L,   867L,  "MHL", "MHL",     NA,    NA,    "RM",   "Marshall Islands",
  986L,    "PLW",  585L,   565L,  "PLW", "PLW",     NA,    NA,    "PS",   "Palau",
  987L,    "FSM",  583L,   868L,  "FSM", "FSM",     NA,    NA,    "FM",   "Federated States of Micronesia",
  990L,    "WSM",  882L,   862L,  "WSM", "WSM",     NA,    NA,    "WS",   "Samoa",
  
  # ----- FIPS-specific corrections -----
  # Georgia: countrycode resolves GEO to FIPS "GE", which collides with
  # East Germany (DDR, gwcode 265, 1949-1990). DDR is patched to "GE" via
  # predecessor_overrides above. Georgia enters the GW system in 1992,
  # after DDR ceases to exist, so there is no year-overlap — but we patch
  # Georgia to "GG" here to ensure the fips_to_iso3 lookup built from
  # the crosswalk is unambiguous across all years.
  372L,    "GEO",  268L,   915L,    "GEO", "GEO",     21L,   268L,  "GG",   "Georgia — FIPS GG (not GE; GE reserved for DDR pre-1990)",
  
  # North Macedonia: countrycode resolves to FIPS "MK", which is correct
  # per CIA FIPS 10-4. Madagascar's FIPS is "MA" (not "MK").

  # Madagascar: FIPS is "MA" per CIA FIPS 10-4
  548L,    "MDG",  450L,   674L,    "MDG", "MDG",     NA,    450L,  "MA",   "Madagascar — FIPS MA (not MK)"
)

# Predecessor V-Dem IDs (V-Dem has these as proper country IDs)
predecessor_vdem <- tribble(
  ~iso3c,  ~vdem_pred,
  "SUN",   17L,    # USSR
  "YUG",   198L,   # Yugoslavia (SFRY)
  "SCG",   198L,   # FRY / Serbia & Montenegro continues 198 in V-Dem
  "CSK",   157L,   # Czechoslovakia
  "DDR",   137L,   # German Democratic Republic
  "YDR",   23L,    # South Yemen (PDR Yemen)
  "VDR",   35L     # South Vietnam (Republic of Vietnam)
)

# Apply manual gwcode-level patches: overwrite the entire
# row of secondary codes for the listed gwcodes (regardless
# of year, since these are bug fixes for entities whose
# codes are wrong/missing throughout their lifespan).

for (i in seq_len(nrow(manual_patches))) {
  p <- manual_patches[i, ]
  risk_panel <- risk_panel %>%
    mutate(
      iso3c    = if_else(gwcode == p$gwcode, p$iso3c,    iso3c),
      iso3n    = if_else(gwcode == p$gwcode, p$iso3n,    iso3n),
      imf      = if_else(gwcode == p$gwcode, p$imf,      imf),
      wb       = if_else(gwcode == p$gwcode, p$wb,       wb),
      wb_api3c = if_else(gwcode == p$gwcode, p$wb_api3c, wb_api3c),
      vdem     = if_else(gwcode == p$gwcode, p$vdem,     vdem),
      wvs      = if_else(gwcode == p$gwcode, p$wvs,      wvs),
      fips     = if_else(gwcode == p$gwcode, p$fips,     fips)
    )
}

# Apply predecessor V-Dem IDs (overrides any V-Dem ID that
# came in via codelist_panel for the modern successor)

risk_panel <- risk_panel %>%
  left_join(predecessor_vdem, by = "iso3c") %>%
  mutate(vdem = coalesce(vdem_pred, vdem)) %>%
  select(-vdem_pred)

# -----------------------------
# 5b) Manual COW ccode patches
# -----------------------------
#
# Only patch cases where the entity is genuinely represented
# in COW-linked datasets but codelist_panel failed to provide
# the cown value.
#
# Do NOT fill ccode = gwcode globally. Many missing ccode rows
# are microstates or de facto states that should not receive
# synthetic COW codes.

ccode_patches <- tribble(
  ~iso3c, ~gwcode, ~year_min, ~year_max, ~ccode_patch, ~note,
  "VNM",  816L,    1954L,     2025L,     816L,         "Vietnam / DRV / unified Vietnam",
  "SRB",  340L,    2005L,     2025L,     345L,         "Serbia: GW 340 but COW continuation code 345"
)

risk_panel <- risk_panel %>%
  left_join(
    ccode_patches %>%
      select(iso3c, gwcode, year_min, year_max, ccode_patch),
    by = c("iso3c", "gwcode")
  ) %>%
  mutate(
    ccode = if_else(
      is.na(ccode) &
        !is.na(ccode_patch) &
        year >= year_min &
        year <= year_max,
      ccode_patch,
      ccode
    )
  ) %>%
  select(-year_min, -year_max, -ccode_patch)

# -----------------------------
# 6) UN DCO/RC regional encoding
# -----------------------------
#
# Each row of this tribble assigns a country to one of the
# five UN DCO/RC regions, based on the official UN DCO Office
# Directory (130 primary RC offices + 30 countries served by
# 8 multi-country offices = 160 countries). Countries NOT in
# the RC system (donor countries, OECD members, the rest of
# Western/Northern Europe and Asia-Pacific developed states)
# get region = NA.
#
# The five regions:
#   AFR   Africa
#   APA   Asia and the Pacific
#   ARAB  Arab States
#   ECA   Europe and Central Asia
#   LAC   Latin America and the Caribbean
#
# Predecessor entities follow the rule: assign region only if
# the modern successor is itself in the RC system.
#   YUG → ECA  (successor states SRB, MNE, BIH, MKD in ECA)
#   SCG → ECA  (successors SRB, MNE in ECA)
#   YDR → ARAB (successor YEM in ARAB)
#   VDR → APA  (successor VNM in APA)
#   ZAN → AFR  (successor TZA in AFR)
# Predecessors with non-RC modern successors get NA:
#   SUN, DDR, CSK
#
# Note on microstate exclusion: the GW microstate filter at
# step 2 drops many small Caribbean and Pacific states that
# the RC system covers via MCOs. Their iso3c→region rows are
# kept in this tribble for completeness (so the table is
# directly reusable in projects that don't apply the GW
# microstate filter), even though they will not match any
# (iso3c, year) row in the current crosswalk.

dco_regions <- tribble(
  ~iso3c, ~dco_region,
  
  # ECA — Europe and Central Asia
  "ALB", "ECA",   "ARM", "ECA",   "AZE", "ECA",   "BIH", "ECA",
  "BLR", "ECA",   "GEO", "ECA",   "KAZ", "ECA",   "KGZ", "ECA",
  "MDA", "ECA",   "MKD", "ECA",   "MNE", "ECA",   "SRB", "ECA",
  "TJK", "ECA",   "TKM", "ECA",   "TUR", "ECA",   "UKR", "ECA",
  "UZB", "ECA",   "XKX", "ECA",
  # Predecessors → ECA
  "YUG", "ECA",   "SCG", "ECA",
  
  # ARAB — Arab States
  "BHR", "ARAB",  "IRQ", "ARAB",  "JOR", "ARAB",  "KWT", "ARAB",
  "LBN", "ARAB",  "PSE", "ARAB",  "SAU", "ARAB",  "SYR", "ARAB",
  "ARE", "ARAB",  "YEM", "ARAB",
  # Predecessor → ARAB
  "YDR", "ARAB",
  
  # AFR — Africa
  "DZA", "AFR",   "AGO", "AFR",   "BEN", "AFR",   "BWA", "AFR",
  "BFA", "AFR",   "BDI", "AFR",   "CPV", "AFR",   "CMR", "AFR",
  "CAF", "AFR",   "TCD", "AFR",   "COM", "AFR",   "COG", "AFR",
  "CIV", "AFR",   "COD", "AFR",   "DJI", "AFR",   "EGY", "AFR",
  "GNQ", "AFR",   "ERI", "AFR",   "SWZ", "AFR",   "ETH", "AFR",
  "GAB", "AFR",   "GMB", "AFR",   "GHA", "AFR",   "GIN", "AFR",
  "GNB", "AFR",   "KEN", "AFR",   "LSO", "AFR",   "LBR", "AFR",
  "LBY", "AFR",   "MDG", "AFR",   "MWI", "AFR",   "MLI", "AFR",
  "MRT", "AFR",   "MAR", "AFR",   "MOZ", "AFR",   "NAM", "AFR",
  "NER", "AFR",   "NGA", "AFR",   "RWA", "AFR",   "STP", "AFR",
  "SEN", "AFR",   "SLE", "AFR",   "SOM", "AFR",   "ZAF", "AFR",
  "SSD", "AFR",   "SDN", "AFR",   "TGO", "AFR",   "TUN", "AFR",
  "UGA", "AFR",   "TZA", "AFR",   "ZMB", "AFR",   "ZWE", "AFR",
  # AFR MCO host + covered
  "MUS", "AFR",   "SYC", "AFR",
  
  # APA — Asia and the Pacific
  "AFG", "APA",   "BGD", "APA",   "BTN", "APA",   "KHM", "APA",
  "CHN", "APA",   "PRK", "APA",   "IND", "APA",   "IDN", "APA",
  "IRN", "APA",   "LAO", "APA",   "MDV", "APA",   "MNG", "APA",
  "MMR", "APA",   "NPL", "APA",   "PAK", "APA",   "PNG", "APA",
  "PHL", "APA",   "LKA", "APA",   "THA", "APA",   "TLS", "APA",
  "VNM", "APA",
  # APA MCO hosts and covered
  "FJI", "APA",   "MYS", "APA",   "FSM", "APA",   "WSM", "APA",
  "SGP", "APA",   "BRN", "APA",   "COK", "APA",   "NIU", "APA",
  "TKL", "APA",   "KIR", "APA",   "MHL", "APA",   "NRU", "APA",
  "PLW", "APA",   "SLB", "APA",   "TON", "APA",   "TUV", "APA",
  "VUT", "APA",
  # Predecessor → APA
  "VDR", "APA",
  
  # LAC — Latin America and the Caribbean
  "ARG", "LAC",   "BOL", "LAC",   "BRA", "LAC",   "CHL", "LAC",
  "COL", "LAC",   "CRI", "LAC",   "CUB", "LAC",   "DOM", "LAC",
  "ECU", "LAC",   "GTM", "LAC",   "GUY", "LAC",   "HTI", "LAC",
  "HND", "LAC",   "MEX", "LAC",   "NIC", "LAC",   "PAN", "LAC",
  "PRY", "LAC",   "PER", "LAC",   "URY", "LAC",   "VEN", "LAC",
  # LAC MCO hosts
  "BRB", "LAC",   "SLV", "LAC",   "JAM", "LAC",   "TTO", "LAC",
  # LAC MCO-covered
  "AIA", "LAC",   "ATG", "LAC",   "VGB", "LAC",   "DMA", "LAC",
  "GRD", "LAC",   "MSR", "LAC",   "LCA", "LAC",   "VCT", "LAC",
  "KNA", "LAC",   "ABW", "LAC",   "CUW", "LAC",   "SXM", "LAC",
  "BMU", "LAC",   "BHS", "LAC",   "CYM", "LAC",   "TCA", "LAC"
)

# Sanity check on the regional table
stopifnot(
  nrow(dco_regions) == nrow(distinct(dco_regions, iso3c)),
  all(dco_regions$dco_region %in% c("AFR", "APA", "ARAB", "ECA", "LAC"))
)

# Apply to risk_panel
risk_panel <- risk_panel %>%
  left_join(dco_regions, by = "iso3c")

# -----------------------------
# 7) Clean country names
# -----------------------------
#
# peacesciencer's gw_name field uses ASCII-only country names
# (CRAN's non-ASCII restriction forced this) and in some cases
# leaks the underlying hex escapes for accented characters
# ("S\xe3o Tom\xe9 and Principe"). We derive a clean UTF-8
# country_name column from countrycode, with a manual override
# table for entities countrycode doesn't have a name for
# (predecessor entities, de facto states, Kosovo, Zanzibar).
#
# The original gw_name column is retained for reference.

country_name_overrides <- tribble(
  ~iso3c,  ~country_name,
  # Predecessor entities (not in countrycode's iso3c dictionary)
  "SUN",   "Soviet Union",
  "YUG",   "Yugoslavia",
  "SCG",   "Serbia and Montenegro",
  "CSK",   "Czechoslovakia",
  "DDR",   "East Germany",
  "YDR",   "South Yemen",
  "VDR",   "South Vietnam",
  # iso3c codes countrycode may not name conventionally
  "XKX",   "Kosovo"
)

risk_panel <- risk_panel %>%
  mutate(
    country_name_cc = countrycode(
      iso3c,
      origin       = "iso3c",
      destination  = "country.name.en",
      warn         = FALSE
    )
  ) %>%
  left_join(country_name_overrides, by = "iso3c") %>%
  rename(country_name_override = country_name) %>%
  mutate(country_name = coalesce(country_name_override, country_name_cc)) %>%
  select(-country_name_cc, -country_name_override)

# Sanity: every row should have a country_name. If any are NA,
# they need to be added to country_name_overrides above.
n_na_name <- sum(is.na(risk_panel$country_name))
if (n_na_name > 0L) {
  na_name_rows <- risk_panel %>%
    filter(is.na(country_name)) %>%
    distinct(iso3c, gwcode, gw_name)
  message("\nNA country_name diagnostic:")
  print(na_name_rows, n = Inf)
  warning("Some rows have NA country_name. Add to country_name_overrides above.")
}

# -----------------------------
# 8) Final ordering and validation
# -----------------------------

risk_panel <- risk_panel %>%
  select(country_name, year, microstate, gwcode, ccode, iso3n, iso3c,
         imf, wb, wb_api3c, vdem, wvs, fips, dco_region) %>%
  arrange(country_name, year)

# Validation
n_na_iso3c <- sum(is.na(risk_panel$iso3c))
if (n_na_iso3c > 0L) {
  na_rows <- risk_panel %>% filter(is.na(iso3c)) %>%
    distinct(gwcode, gw_name) %>%
    arrange(gwcode)
  message("\nNA iso3c diagnostic — ", nrow(na_rows),
          " gwcodes failed all lookup steps:")
  print(na_rows, n = Inf)
  stop("Some rows have NA iso3c. ",
       "Add manual patches in the manual_patches tribble for the gwcodes ",
       "shown above.")
}

# Duplicate check (runs after NA check so the diagnostic isn't
# polluted by NA-NA "duplicates")
n_total    <- nrow(risk_panel)
n_distinct <- nrow(distinct(risk_panel, iso3c, year))

if (n_total != n_distinct) {
  dups <- risk_panel %>%
    group_by(iso3c, year) %>%
    filter(n() > 1L) %>%
    arrange(iso3c, year, gwcode)
  message("\nDUPLICATE (iso3c, year) DIAGNOSTIC:")
  print(head(dups, 30L))
  stop("Duplicate (iso3c, year) rows in crosswalk. ",
       "Check predecessor_overrides for year-range collisions.")
}

stopifnot(
  !any(is.na(risk_panel$year)),
  !any(is.na(risk_panel$gwcode)),
  all(risk_panel$year >= start_year),
  all(risk_panel$year <= end_year)
)

# -----------------------------
# 9) Audit summary
# -----------------------------

audit <- risk_panel %>%
  group_by(iso3c) %>%
  summarise(
    gwcode      = paste(sort(unique(gwcode)), collapse = "/"),
    ccode        = first(ccode),
    iso3n        = first(iso3n),
    dco_region   = first(dco_region),
    first_year   = min(year),
    last_year    = max(year),
    n_years      = n(),
    .groups      = "drop"
  ) %>%
  arrange(dco_region, iso3c)

# Regional coverage summary
region_summary <- audit %>%
  mutate(in_rc = !is.na(dco_region)) %>%
  group_by(dco_region) %>%
  summarise(
    n_countries = n(),
    .groups     = "drop"
  ) %>%
  arrange(desc(n_countries))

message("\nDCO/RC regional coverage in crosswalk:")
for (i in seq_len(nrow(region_summary))) {
  region_label <- region_summary$dco_region[i]
  if (is.na(region_label)) region_label <- "(none — outside RC system)"
  message(sprintf("  %-30s %d countries",
                  region_label,
                  region_summary$n_countries[i]))
}

# ECA-18 detail (sanity check)
eca_check <- audit %>% filter(dco_region == "ECA")
message("\nECA detail:")
for (i in seq_len(nrow(eca_check))) {
  message(sprintf("  %s: %d-%d  [%d years]",
                  eca_check$iso3c[i],
                  eca_check$first_year[i],
                  eca_check$last_year[i],
                  eca_check$n_years[i]))
}

# Predecessor entity check
pred_check <- audit %>% filter(iso3c %in% predecessor_iso3c)
message("\nPredecessor entity coverage:")
for (i in seq_len(nrow(pred_check))) {
  region_label <- pred_check$dco_region[i]
  if (is.na(region_label)) region_label <- "—"
  message(sprintf("  %s [%s]: gwcode(s) %s, %d-%d  [%d years]",
                  pred_check$iso3c[i],
                  region_label,
                  pred_check$gwcode[i],
                  pred_check$first_year[i],
                  pred_check$last_year[i],
                  pred_check$n_years[i]))
}

# Sanity: countries listed in dco_regions that didn't merge
# to any panel row (because they're filtered out as
# microstates, or because they're not in the GW state list).
unmatched_dco <- setdiff(dco_regions$iso3c, audit$iso3c)
if (length(unmatched_dco) > 0L) {
  message(sprintf(
    "\nNote: %d DCO/RC iso3c codes in the regional table are not present in the panel\n  (microstates filtered or absent from GW state list): %s",
    length(unmatched_dco),
    paste(sort(unmatched_dco), collapse = ", ")
  ))
}

expected_missing_ccode <- c(
  # microstates not expected in most COW-linked datasets
  "AND", "LIE", "MCO", "SMR",
  "WSM", "NRU", "TON", "GRD", "STP", "SYC",
  "DMA", "TUV", "KIR", "LCA", "VCT", "VUT",
  "ATG", "KNA", "FSM", "MHL", "PLW"
)

unexpected_missing_ccode <- risk_panel %>%
  filter(is.na(ccode)) %>%
  distinct(iso3c, country_name, gwcode, microstate) %>%
  filter(!iso3c %in% expected_missing_ccode)

if (nrow(unexpected_missing_ccode) > 0L) {
  print(unexpected_missing_ccode, n = Inf)
  stop("Unexpected missing ccode values. Add explicit ccode patches or mark as expected.")
}

# -----------------------------
# 10) Write outputs
# -----------------------------

write_csv(risk_panel, file.path(output_dir, "00_country_crosswalk.csv"))
saveRDS(risk_panel,   file.path(output_dir, "00_country_crosswalk.RDS"))
write_csv(audit,      file.path(diagnostics_dir, "00_crosswalk_audit.csv"))

message(
  "\n00_country_crosswalk.R complete. ",
  nrow(risk_panel), " (iso3c, year) rows, ",
  length(unique(risk_panel$iso3c)), " distinct iso3c codes, ",
  "years ", min(risk_panel$year), "–", max(risk_panel$year), ".\n",
  "Outputs:\n",
  "  ", file.path(output_dir, "00_country_crosswalk.csv"), "\n",
  "  ", file.path(output_dir, "00_country_crosswalk.RDS"), "\n",
  "  ", file.path(diagnostics_dir, "00_crosswalk_audit.csv")
)