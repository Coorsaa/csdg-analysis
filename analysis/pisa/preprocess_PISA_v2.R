# ============================================================
# PISA 2018 — Compact analysis dataset (reduced variables, selected countries)
#
# Extraction script of the coauthors who prepared the PISA data. Its output is
# the 32-country analytic extract read by the PISA analyses in analysis/pisa/.
#
# Input: the official OECD PISA 2018 database, SPSS format, available at
#   https://www.oecd.org/en/data/datasets/pisa-2018-database.html
# Two files are needed (download the student and school questionnaire data
# files and unzip them):
#   - student questionnaire data file, CY07_MSU_STU_QQQ.sav
#   - school questionnaire data file,  CY07_MSU_SCH_QQQ.sav
# Place them in data/PISA/raw/ (default) or set PISA2018_RAW_DIR to their
# directory. The files are matched by these name prefixes.
#
# Output (written to data/PISA/ unless PISA2018_OUTPUT_DIR is set; data/ is
# excluded from version control and must not be committed):
#   - pisa2018_reading_compactv2.csv       analytic extract (260,641 students)
#   - pisa2018_reading_compactv2.rds       the same extract as an R object
#   - pisa2018_reading_compactv2_manifest.csv  variable table (role, level,
#     source file, label, missing rate); a copy of the manifest used in the
#     article is analysis/pisa/pisa2018_reading_compactv2_manifest.csv
#
# Use in the analyses: analysis/pisa/analysis.R, analysis/pisa/ses_reanalysis.R,
# and analysis/pisa/tuning_check.R read the extract and the manifest from
# data/PISA/ and draw the equal-country sample (1,500 students per country)
# themselves. analysis/pisa/analysis.R also expects a copy of this script at
# data/PISA/preprocess_PISA_v2.R and records the checksums of all its inputs.
#
# SHA-256 of the files used for the article (recorded by the primary PISA run):
#   pisa2018_reading_compactv2.csv           01ac4fe51d3c80ae7c8f86ecb537d2c6e4ddcbe5dab9874627a36cf440e12867
#   pisa2018_reading_compactv2_manifest.csv  60b4b0bc0bf0ca67b4b456d8d959c9c97e89548402c18f478139ad2d6098fa48
#   preprocess_PISA_v2.R (as run)            7714ee0560ade5e8e62af26410e612c98e72065987ad4d71d1d67b9d4f5f2c54
# This copy differs from the script as run only in this header and in the
# default input and output directories, which now resolve to data/PISA/ from
# the repository root; the extraction logic is unchanged.
#
# Usage (from the repository root; requires haven, dplyr, stringr, readr,
# tibble, and vctrs):
#   Rscript analysis/pisa/preprocess_PISA_v2.R
#
# Construction rules:
#   1. Country filter: 32 prespecified country codes shared by the frozen analysis
#   2. Remove entire variable clusters with structural missingness:
#      - All teacher-level variables (TCH_*): 75–100% missing
#      - All parent questionnaire variables: ~85% missing
#      - ICT cluster (rotated module): ~45–51% missing
#      - Global competence module: ~27–43% missing
#      - Career information module: ~63% missing
#   3. Threshold-based auto-drop: any remaining var with > MAX_MISS_RATE
#      missing (computed on the selected countries) is logged and dropped
#   4. Manifest updated accordingly
# ============================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
  library(readr)
  library(tibble)
})

# -----------------------------
# Config
# -----------------------------
arguments = commandArgs(trailingOnly = FALSE)
file_argument = grep("^--file=", arguments, value = TRUE)
script_path = if (length(file_argument)) sub("^--file=", "", file_argument[[1L]]) else "analysis/pisa/preprocess_PISA_v2.R"
project_root = dirname(dirname(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE))))
pisa_dir = file.path(project_root, "data", "PISA")
base_dir = Sys.getenv("PISA2018_RAW_DIR", unset = file.path(pisa_dir, "raw"))
output_dir = Sys.getenv("PISA2018_OUTPUT_DIR", unset = pisa_dir)

# Maximum tolerated missing rate (applied AFTER country filtering)
MAX_MISS_RATE <- 0.15

# Countries to keep: the 32-country frame frozen for the compact analysis.
# The frame was chosen for shared availability of the retained variables and
# manageable equal-country computation. It is an explicit analytic-frame
# definition, not a representative sample of OECD members or PISA countries.
COUNTRIES_KEEP <- c(
  "AUS", "AUT", "BEL", "CAN", "CZE",
  "DNK", "EST", "FIN", "FRA", "DEU",
  "GRC", "HUN", "IRL", "ISL", "ITA",
  "JPN", "KOR", "LVA", "LTU", "MEX",
  "NLD", "NOR", "POL", "PRT", "SVK",
  "SVN", "ESP", "SWE", "CHE", "GBR",
  "USA", "NZL"
)

# Output names
out_rds      <- file.path(output_dir, "pisa2018_reading_compactv2.rds")
out_csv      <- file.path(output_dir, "pisa2018_reading_compactv2.csv")
out_manifest <- file.path(output_dir, "pisa2018_reading_compactv2_manifest.csv")

# -----------------------------
# Helpers
# -----------------------------
find_sav <- function(dir, pattern_regex) {
  hits <- list.files(dir, pattern = pattern_regex, full.names = TRUE, ignore.case = TRUE)
  if (length(hits) == 0) {
    stop(sprintf("No SAV file found for pattern: %s\n\nAvailable .sav files:\n%s",
                 pattern_regex,
                 paste(list.files(dir, pattern = "\\.sav$", full.names = FALSE, ignore.case = TRUE),
                       collapse = "\n")))
  }
  if (length(hits) > 1) message("Multiple matches found; using first:\n", paste(hits, collapse = "\n"))
  hits[[1]]
}

strip_haven_labelled <- function(df) {
  df %>% mutate(across(where(haven::is.labelled), ~vctrs::vec_data(.x)))
}

extract_var_labels <- function(df) {
  tibble(
    variable_source = names(df),
    label = vapply(df, function(x) {
      lab <- attr(x, "label")
      if (is.null(lab)) NA_character_ else as.character(lab)
    }, character(1))
  )
}

coerce_keys <- function(df, keys) {
  keys_present <- intersect(keys, names(df))
  df %>% mutate(across(all_of(keys_present), ~as.character(.x)))
}

mean_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

detect_pv_read <- function(nms) {
  pv <- grep("^PV\\d+READ$", nms, value = TRUE)
  pv <- pv[order(as.integer(str_match(pv, "^PV(\\d+)READ$")[,2]))]
  pv
}

missing_rate <- function(x) mean(is.na(x))

# -----------------------------
# Locate files
# -----------------------------
stu_file <- find_sav(base_dir, "^CY07_MSU_STU_QQQ.*\\.sav$")
sch_file <- find_sav(base_dir, "^CY07_MSU_SCH_QQQ.*\\.sav$")
# Teacher file no longer needed — all TCH_ variables are dropped (75-100% missing).

message("Student file: ", stu_file)
message("School  file: ", sch_file)

# -----------------------------
# Read raw
# -----------------------------
stu_raw <- read_sav(stu_file) %>% haven::zap_missing()
sch_raw <- read_sav(sch_file) %>% haven::zap_missing()

names(stu_raw) <- toupper(names(stu_raw))
names(sch_raw) <- toupper(names(sch_raw))

# IDs
id_stu <- c("CNT", "CNTSCHID", "CNTSTUID")
id_sch <- c("CNT", "CNTSCHID")

# PVs
pv_read <- detect_pv_read(names(stu_raw))
if (length(pv_read) < 10) stop("Could not detect PV1READ..PV10READ in student file.")
pv_read <- pv_read[1:10]

# -----------------------------
# Variable selection
# (Only variables with low structural missingness in OECD core countries)
# -----------------------------

# — Student background —
stu_bg <- c(
  "ST004D01T",                          # gender
  "AGE",
  "ST001D01T",                          # grade
  "IMMIG",                              # immigration status
  "LANGN",                              # language at home
  "ISCEDD", "ISCEDL", "ISCEDO",         # ISCED codes
  "ESCS", "HISEI", "PARED",             # SES core
  "HOMEPOS", "WEALTH", "HEDRES",        # home resources
  "CULTPOSS", "ICTRES"                  # possessions
  # DROPPED: DURECEC (26% missing)
)

# — Reading engagement & metacognition —
stu_reading <- c(
  "JOYREAD",                            # joy of reading          (~6% missing)
  "SCREADCOMP",                         # reading self-concept: competence (~12%)
  "SCREADDIFF",                         # reading self-concept: difficulty (~12%)
  "STIMREAD",                           # teacher stimulation of reading  (~7%)
  "METASUM",                            # metacognition: summarising      (~13%)
  "METASPAM",                           # metacognition: assess credibility (~14%)
  "UNDREM"                              # metacognition: understanding    (~13%)
)

# — Classroom instruction & climate —
stu_instruction <- c(
  "DISCLIMA",                           # disciplinary climate     (~5%)
  "TEACHSUP",                           # teacher support          (~10%)
  "TEACHINT",                           # teacher interest         (~10%)
  "DIRINS",                             # direct instruction       (~9%)
  "ADAPTIVITY",                         # adaptive instruction     (~7%)
  "PERFEED"                             # perceived feedback       (~11%)
)

# — Motivation & self-regulation —
stu_motivation <- c(
  "WORKMAST",                           # work mastery             (~11%)
  "MASTGOAL",                           # mastery goal orientation (~12%)
  "GFOFAIL",                            # fear of failure          (~11%)
  "EFFORT1",                            # test effort (actual)     (~17%)
  "EFFORT2"                             # test effort (hypothetical)(~18%)
  # DROPPED: EUDMO (17%), COMPETE (9% but less theoretically central)
)

# — Well-being & school environment —
stu_wellbeing <- c(
  "BELONG",                             # sense of belonging       (~13%)
  "RESILIENCE",                         # resilience               (~12%)
  "ATTLNACT"                            # attitude: learning activities (~8%)
  # DROPPED: BEINGBULLIED (24%), SWBP (20%)
)

# — EXPLICITLY DROPPED CLUSTERS (kept here for documentation) —
# ICT cluster (rotated; 44-51% missing):
#   AUTICT, COMPICT, ENTUSE, HOMESCH, ICTCLASS, ICTHOME, ICTOUTSIDE,
#   INTICT, SOIAICT, USESCH, ICTHOME
# Global competence module (27-43% missing):
#   GCAWARE, GCSELFEFF, GLOBMIND, PERSPECT, RESPECT, COGFLEX,
#   DISCRIM, ATTIMM
# Parent questionnaire (~85% missing):
#   ATTIMMP, CURSUPP, EMOSUPP, GCAWAREP, INTCULTP, JOYREADP,
#   PASCHPOL, PRESUPP, PQSCHOOL, SOCONPA
# Career information module (~63% missing):
#   INFOCAR, INFOJOB1, INFOJOB2
# Learning time (36% missing): TMINS
# Perception of competition/cooperation (22-24%): PERCOMP, PERCOOP

stu_covars <- unique(c(stu_bg, stu_reading, stu_instruction, stu_motivation, stu_wellbeing))

# — School-level —
# Candidate variables had comparatively low missingness in the source review.
# No missing-at-random assumption is made; missingness is recomputed after the
# country filter and audited in the downstream analysis.
sch_covars <- c(
  "EDUSHORT",                           # shortage: educational material (~5%)
  "STAFFSHORT",                         # shortage: staff                (~5%)
  "STUBEHA",                            # student behaviour hindrance    (~4%)
  "TEACHBEHA",                          # teacher behaviour hindrance    (~4%)
  "CLSIZE",                             # class size                     (~11%)
  "SCHSIZE",                            # school size                    (~15%)
  "STRATIO"                             # student-teacher ratio          (~17%)
  # DROPPED: SCH_PROATCE (15%), SCH_RATCMP1/2 (~11-12%), SCH_SCMCEG (22%)
)

# Teacher level: entirely dropped — best available variables are 75% missing.

# -----------------------------
# Build keep-lists
# -----------------------------
stu_keep <- unique(c(id_stu, pv_read, stu_covars))
sch_keep <- unique(c(id_sch, sch_covars))

# -----------------------------
# Student: select + labels + strip haven + coerce keys
# -----------------------------
stu_labels <- extract_var_labels(stu_raw %>% select(any_of(stu_keep)))

stu <- stu_raw %>%
  select(any_of(stu_keep)) %>%
  strip_haven_labelled() %>%
  coerce_keys(id_stu)

missing_stu <- setdiff(stu_keep, names(stu))
if (length(missing_stu) > 0)
  warning("Missing in student file (dropped):\n", paste(missing_stu, collapse = ", "))

# --- Country filter (applied early so missingness stats are representative) ---
n_before <- nrow(stu)
stu <- stu %>% filter(CNT %in% COUNTRIES_KEEP)
n_after  <- nrow(stu)
message(sprintf("Country filter: kept %d / %d students (%d countries)",
                n_after, n_before, length(unique(stu$CNT))))

# -----------------------------
# School: select + labels + strip haven + coerce keys + prefix SCH_
# -----------------------------
sch_labels <- extract_var_labels(sch_raw %>% select(any_of(sch_keep)))

sch <- sch_raw %>%
  select(any_of(sch_keep)) %>%
  strip_haven_labelled() %>%
  coerce_keys(id_sch) %>%
  filter(CNT %in% COUNTRIES_KEEP)       # align school data to selected countries

missing_sch <- setdiff(sch_keep, names(sch))
if (length(missing_sch) > 0)
  warning("Missing in school file (dropped):\n", paste(missing_sch, collapse = ", "))

sch_vars_present <- setdiff(names(sch), id_sch)
sch <- sch %>%
  rename_with(~paste0("SCH_", .x), all_of(sch_vars_present))

# -----------------------------
# Merge
# -----------------------------
dat <- stu %>%
  left_join(sch, by = c("CNT", "CNTSCHID"))

# -----------------------------
# Post-merge missingness filter
# Drop any covariate still exceeding MAX_MISS_RATE (computed on selected countries)
# -----------------------------
covar_cols <- setdiff(names(dat), c(id_stu, pv_read))
miss_rates  <- sapply(dat[covar_cols], missing_rate)
drop_cols   <- names(miss_rates[miss_rates > MAX_MISS_RATE])

if (length(drop_cols) > 0) {
  message(sprintf(
    "\nAuto-dropped %d variable(s) exceeding %.0f%% missingness threshold:\n%s",
    length(drop_cols), MAX_MISS_RATE * 100,
    paste(sprintf("  %-20s  %.1f%%", drop_cols, miss_rates[drop_cols] * 100),
          collapse = "\n")
  ))
  dat <- dat %>% select(-all_of(drop_cols))
} else {
  message("All remaining variables are within the missingness threshold.")
}

# -----------------------------
# Manifest (variable table)
# -----------------------------
manifest_ids <- tibble(
  variable_out    = id_stu,
  role            = "id",
  level           = "ID",
  source_file     = "STU_QQQ",
  variable_source = id_stu,
  label           = id_stu,
  fixed_or_auto   = "fixed"
)

manifest_pv <- tibble(
  variable_out    = pv_read,
  role            = "outcome",
  level           = "Student",
  source_file     = "STU_QQQ",
  variable_source = pv_read,
  label           = pv_read,
  fixed_or_auto   = "fixed"
)

stu_present_covars <- intersect(setdiff(names(stu), c(id_stu, pv_read)), names(dat))
manifest_stu <- tibble(
  variable_out    = stu_present_covars,
  role            = "covariate",
  level           = "Student",
  source_file     = "STU_QQQ",
  variable_source = stu_present_covars,
  fixed_or_auto   = "fixed"
) %>%
  left_join(stu_labels, by = "variable_source")

sch_present <- intersect(
  setdiff(names(sch), id_sch),
  names(dat)
)
manifest_sch <- tibble(
  variable_out    = sch_present,
  role            = "covariate",
  level           = "School",
  source_file     = "SCH_QQQ",
  variable_source = str_remove(sch_present, "^SCH_"),
  fixed_or_auto   = "fixed"
) %>%
  left_join(sch_labels, by = "variable_source") %>%
  mutate(variable_out = paste0("SCH_", variable_source))

manifest <- bind_rows(manifest_ids, manifest_pv, manifest_stu, manifest_sch) %>%
  mutate(
    missing_rate = vapply(variable_out, function(v) {
      if (!v %in% names(dat)) return(NA_real_)
      missing_rate(dat[[v]])
    }, numeric(1))
  ) %>%
  arrange(role, level, variable_out)

# -----------------------------
# Save
# -----------------------------
saveRDS(dat, out_rds)
write_csv(dat, out_csv, na = "")
write_csv(manifest, out_manifest, na = "")

message("\n=== Done ===")
message("Countries included:  ", paste(sort(unique(dat$CNT)), collapse = ", "))
message("Saved dataset:  ", out_rds)
message("Saved CSV:      ", out_csv)
message("Saved manifest: ", out_manifest)
message(sprintf("Rows: %d  |  Cols: %d", nrow(dat), ncol(dat)))
message("Student covars: ", length(stu_present_covars))
message("School  covars: ", length(sch_present))
message("Teacher covars: 0 (all dropped — 75-100%% missing in original data)")
