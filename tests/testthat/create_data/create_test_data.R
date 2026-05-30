library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(xportr)

set.seed(20260501)

# Codelist / VLM control for AVALCA1N
avalca1n_lookup <- list(
   ADURD  = tibble(AVALCA1N = 1:5,
                   AVALCA1C = c("1-12 weeks", "13-26 weeks", "27-40 weeks", "41-52 weeks", "> 52 weeks")),
   NUMINJ = tibble(AVALCA1N = 1:5,
                   AVALCA1C = c("1-12 doses", "13-26 doses", "27-40 doses", "41-52 doses", ">52 doses")),
   NUMINJM = tibble(AVALCA1N = 1:5,
                    AVALCA1C = c("0 missed doses", "1 missed dose", "2 missed doses", "3 missed doses", ">3 missed doses")),
   PERCOMP = tibble(AVALCA1N = 1:2,
                    AVALCA1C = c("< 80%", ">= 80%"))
)

# A small synthetic subject-level shell
n_subj <- 40
subjects <- tibble(
   STUDYID = "ADEX01",
   USUBJID  = sprintf("ADEX01-%03d", 1:n_subj),
   SUBJID   = sprintf("%03d", 1:n_subj),
   SITEID   = sample(sprintf("%03d", 1:8), n_subj, replace = TRUE),
   AAGE     = sample(18:75, n_subj, replace = TRUE),
   AAGEU    = "YEARS",
   SEX      = sample(c("M", "F"), n_subj, replace = TRUE),
   RACE     = sample(c("WHITE", "BLACK OR AFRICAN AMERICAN"), n_subj, replace = TRUE),
   RACEN    = match(RACE, c("WHITE", "BLACK OR AFRICAN AMERICAN")),
   ARACE    = RACE,
   ARACEN   = RACEN,
   ETHNIC   = sample(c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"), n_subj, replace = TRUE),
   ENRLFL   = "Y",
   SCRNFL   = "Y",
   FASFL    = "Y",
   SAFFL    = "Y",
   RANDFL   = "Y",
   TRT01P   = sample(c("Placebo", "Active Treatment"), n_subj, replace = TRUE),
   TRT01PN  = if_else(TRT01P == "Placebo", 1L, 2L),
   TRT01A   = TRT01P,
   TRT01AN  = TRT01PN,
   TRTSDT   = as.Date("2025-01-01") + sample(0:60, n_subj, replace = TRUE),
   TRTSTM   = sample(0:(23*3600), n_subj, replace = TRUE),
   TRTSDTM  = as.POSIXct(TRTSDT) + TRTSTM,
   TRTEDT   = TRTSDT + sample(30:180, n_subj, replace = TRUE),
   TRTEDTM  = as.POSIXct(TRTEDT) + sample(0:(23*3600), n_subj, replace = TRUE),
   OLEFL    = sample(c("Y", "N"), n_subj, replace = TRUE, prob = c(0.2, 0.8)),
   ILDDURCT = sample(c("<1 year", "1-2 years", ">2 years"), n_subj, replace = TRUE),
   CTDPRIMY = sample(c("Atopic dermatitis", "Psoriasis", "Other"), n_subj, replace = TRUE),
   STRATAR  = sample(c("Age", "Baseline severity", "Region"), n_subj, replace = TRUE)
)

# Rows for the four PARAMCD values that control AVALCA1N
param_rows <- tibble(PARAMCD = c("ADURD", "NUMINJ", "NUMINJM", "PERCOMP")) |>
   mutate(
      PARAM = recode(PARAMCD,
                     ADURD = "Duration of Exposure (days)",
                     NUMINJ = "Number of injections",
                     NUMINJM = "Number of missed injections",
                     PERCOMP = "Intervention Compliance (%)"),
      PARAMN = row_number()
   )

# Build ADEX
adex <- subjects |>
   crossing(param_rows) |>
   group_by(USUBJID) |>
   mutate(
      ASEQ  = row_number(),
      AVISITN = 10,
      AVISIT  = "Baseline",
      EXTRT   = TRT01A,
      EXDOSE  = if_else(TRT01A == "PLACEBO", 0L, sample(c(50L, 100L, 150L), n(), replace = TRUE)),
      EXDOSU  = "mg",
      EXDOSFRM = "TABLET",
      EXDOSFRQ = "QW",
      EXROUTE  = "SUBCUTANEOUS",
      ASTDT    = TRTSDT,
      ASTTM    = as.POSIXct(TRTSDTM),
      ASTDTM   = TRTSDTM,
      AENDT    = TRTEDT,
      AENTM    = as.POSIXct(TRTEDTM),
      AENDTM   = TRTEDTM,
      ASTDY    = 1L,
      AENDY    = as.integer(TRTEDT - TRTSDT) + 1L
   ) |>
   ungroup() |>
   left_join(
      bind_rows(lapply(names(avalca1n_lookup), function(p) {
         avalca1n_lookup[[p]] |>
            mutate(PARAMCD = p)
      })),
      by = "PARAMCD"
   ) |>
   mutate(
      # strict VLM-controlled values
      AVALCA1N = as.integer(AVALCA1N),
      AVALCA1N = if_else(PARAMCD %in% names(avalca1n_lookup), AVALCA1N, NA_integer_),
      AVALCAT1 = AVALCA1C,
      # keep AVAL numeric and reasonable
      AVAL = case_when(
         PARAMCD == "ADURD"   ~ as.numeric(sample(1:60, n(), replace = TRUE)),
         PARAMCD == "NUMINJ"  ~ as.numeric(sample(1:60, n(), replace = TRUE)),
         PARAMCD == "NUMINJM" ~ as.numeric(sample(0:10, n(), replace = TRUE)),
         PARAMCD == "PERCOMP" ~ as.numeric(sample(c(70, 75, 80, 85, 90, 95, 100), n(), replace = TRUE)),
         TRUE ~ NA_real_
      )
   ) |>
   select(
      STUDYID, USUBJID, SUBJID, SITEID, AAGE, AAGEU, SEX, RACE, RACEN, ARACE, ARACEN,
      ETHNIC, ENRLFL, SCRNFL, FASFL, SAFFL, RANDFL,
      TRT01P, TRT01PN, TRT01A, TRT01AN, TRTSDT, TRTSTM, TRTSDTM, TRTEDT, TRTEDTM,
      OLEFL, ILDDURCT, CTDPRIMY, STRATAR,
      AVISITN, AVISIT, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXDOSFRQ, EXROUTE,
      ASTDT, ASTTM, ASTDTM, AENDT, AENTM, AENDTM, ASTDY, AENDY,
      PARAM, PARAMCD, PARAMN, AVAL, AVALCAT1, AVALCA1N, ASEQ
   )

# Validation: all AVALCA1N values are acceptable for each PARAMCD
valid_values <- list(
   ADURD   = 1:5,
   NUMINJ  = 1:5,
   NUMINJM = 1:5,
   PERCOMP = 1:2
)

check <- adex |>
   mutate(is_valid = map2_lgl(PARAMCD, AVALCA1N, ~ is.na(.y) || .y %in% valid_values[[.x]]))

stopifnot(all(check$is_valid))

adex


# Make bad data
row1 <- adex |> mutate(row = if_else(PARAMCD == "NUMINJM", row_number(), NA)) |> filter(!is.na(row)) |> pull(row)
row2 <- adex |> mutate(row = if_else(PARAMCD == "PERCOMP", row_number(), NA)) |> filter(!is.na(row)) |> pull(row)

adex2 <- adex |> mutate(AVALCAT1 = case_when(row_number() %in% c(11, 12) ~ "Invalid 1", row_number() == 13 ~ "Invalid 2", TRUE ~ AVALCAT1))
adex2 <- adex2 |> mutate(AVALCAT1 = case_when(row_number() %in% c(16) ~ "Invalid 3", TRUE ~ AVALCAT1))

xportr::xportr_write(adex2, "~/metatools/inst/extdata/adex.xpt")

