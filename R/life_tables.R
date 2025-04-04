# life_tables.R
# Functions for calculating and formatting life tables
# demography_helpers.R
# Helper functions for life table metrics

#' Build a full demographic life table from cohort data
#'
#' @param df A cohort-formatted tibble with Births, Nx, Age, Cohort
#' @return A life table with survivorship, mortality, and reproductive values
#' @export
#'
#' @importFrom dplyr arrange group_by join_by if_else lead mutate relocate select ungroup
#' @importFrom tidyr fill replace_na
lifeTab <- function(df) {
  df %>%
    arrange(Cohort, Age) %>%
    mutate(across(c(Births, Nx), ~ replace_na(., 0)),
           Qx_risk = Nx,
           Mx_risk = Nx) %>%
    group_by(Cohort) %>%
    mutate(
      Deaths = if_else(Age == max(Age), Nx, Nx - lead(Nx)),
      N0     = if_else(Age == 0, Nx, NA),
      N1     = if_else(Age == 1, Nx, NA),
      Px     = if_else(Nx == 0 | Age == max(Age), 0, lead(Nx)/Nx)
    ) %>%
    fill(N0, N1, .direction = "downup") %>%
    ungroup() %>%
    mutate(
      Lx1 = if_else(N1 == 0, 0, Nx / N1),
      Lx  = if_else(N0 == 0, 0, Nx / N0),
      Qx  = if_else(Qx_risk == 0, 0, Deaths / Qx_risk),
      Mx  = if_else(Mx_risk == 0, 0, (Births / Mx_risk) / 2),
      Qx_1 = if_else(Age == 0, Qx, NA),
      Fx  = Mx * Lx,
      numT = Age * Fx
    ) %>%
    group_by(Cohort) %>%
    mutate(
      MLE        = if_else(max(N1, na.rm = TRUE) < 1, 0, mle(Lx1, Age)),
      repro_first = min(Age[Mx > 0]),
      repro_last  = max(Age[Mx > 0]),
      age_max  = max(Age[Deaths > 0]),
      R0   = sum(Fx),
      Tnum = sum(numT)
    ) %>%
    ungroup() %>%
    mutate(
      T = if_else(R0 > 0, Tnum / R0, 0),
      lambda = if_else(R0 > 0 & T > 0, R0^(1/T), 0)
    ) %>%
    select(-Tnum) %>%
    relocate(Qx_risk,
             Mx_risk,
             N0,
             Nx,
             Px,
             Lx,
             Lx1,
             Qx,
             Qx_1,
             Mx,
             R0,
             T,
             MLE,
             lambda,
             repro_first,
             repro_last,
             age_max,
             .after = Age)
}

#' Generate life table from studbook over cohorts
#'
#' @param studbook Studbook tibble
#' @param Year_min First cohort year (optional)
#' @param Year_max Last cohort year (optional)
#' @param span Years per cohort (optional)
#' @param age_max Maximum age (optional)
#' @return A tibble with cohort life table summary
#' @export
#'
#' @importFrom dplyr arrange bind_rows count group_by n summarise
cohort_lifeTab <- function(studbook, Year_min = NULL, Year_max = NULL, span = 10, age_max = NULL) {
  if (is.null(age_max)) { age_max <- max(studbook$age_last) } else { age_max <- age_max }
  if (is.null(Year_min)) { Year_min <- min(studbook$Year_birth) } else { Year_min <- Year_min }
  if (is.null(Year_max)) { Year_max <- year(today()) } else { Year_max <- Year_max }
  life.table.sexes <- count_births(studbook) %>%
    summarize(Births = sum(Births),
              Nx     = n(),
              .by    = c(Year_birth, Sex, Age)) %>%
    make_cohorts(Year_min    = Year_min,
                 Year_max    = Year_max,
                 span        = span,
                 age_max     = age_max,
                 include_sex = TRUE) %>%
    summarize(Births = sum(Births),
              Nx      = sum(Nx),
              .by     = c(Cohort_label, Cohort_birth, Sex, Cohort, Age)) %>%
    arrange(Cohort_birth, Sex, Age) %>%
    lifeTab()

  life.table.totals <- count_births(studbook) %>%
    summarize(Births = sum(Births),
              Nx     = n(),
              .by    = c(Year_birth, Age)) %>%
    make_cohorts(Year_min    = Year_min,
                 Year_max    = Year_max,
                 span        = span,
                 age_max     = age_max,
                 include_sex = FALSE) %>%
    summarize(Births = sum(Births),
              Nx      = sum(Nx),
              .by     = c(Cohort_label, Cohort_birth, Cohort, Sex, Age)) %>%
    arrange(Cohort_birth, Age) %>%
    lifeTab()

  bind_rows(life.table.totals, life.table.sexes) %>%
    arrange(Cohort_birth, Age, Sex)
}

#' Reduce full life table to summary stats per cohort
#'
#' @param df Life table tibble
#' @return Condensed summary table with lambda and vital rates
#' @export
#'
#' @importFrom dplyr arrange distinct filter mutate select
lifeTab_static <- function(df) {
  df %>%
    mutate(across(c(Px:lambda), ~ round(., 3))) %>%
    select(Cohort,
           Cohort_label,
           Sex,
           N0,
           Qx_1,
           R0,
           T,
           MLE,
           lambda,
           repro_first,
           repro_last,
           age_max) %>%
    filter(N0 > 0) %>%
    distinct() %>%
    arrange(Cohort, Sex)
}

