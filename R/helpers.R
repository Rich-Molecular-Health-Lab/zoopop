
#' Gather IDs of all living individuals
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param include A string specifying whether to return `"all"` living individuals, only `"females"`, or only `"males"`.
#' @return A vector containing a subset of individual IDs
#' @export
#'
#' @importFrom dplyr filter pull
#' @importFrom magrittr %>%
living <- function(studbook, include = "all") {
  if (include == "all") {
    filter(studbook, Status %in% c("Alive", "A")) %>% pull(ID) %>% unique()
  } else if (include == "males") {
    filter(studbook, Status %in% c("Alive", "A") & Sex %in% c("Male", "M")) %>% pull(ID) %>% unique()
  } else if (include == "females") {
    filter(studbook, Status %in% c("Alive", "A") & Sex %in% c("Female", "F")) %>% pull(ID) %>% unique()
  } else if (include == "undetermined") {
    filter(studbook, Status %in% c("Alive", "A") & Sex %in% c("Undetermined", "U")) %>% pull(ID) %>% unique()
  }

}

#' Gather IDs of all deceased individuals
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param include A string specifying whether to return `"all"` deceased individuals, only `"females"`, or only `"males"`.
#' @return A vector containing a subset of individual IDs
#' @export
#'
#' @importFrom dplyr filter pull
#' @importFrom magrittr %>%
deceased <- function(studbook, include = "all") {
  if (include == "all") {
    filter(studbook, Status %in% c("Deceased", "D", "H", "Hypothetical")) %>% pull(ID) %>% unique()
  } else if (include == "males") {
    filter(studbook, Status %in% c("Deceased", "D", "H", "Hypothetical") & Sex %in% c("Male", "M")) %>% pull(ID) %>% unique()
  } else if (include == "females") {
    filter(studbook, Status %in% c("Deceased", "D", "H", "Hypothetical") & Sex %in% c("Female", "F")) %>% pull(ID) %>% unique()
  } else if (include == "undetermined") {
    filter(studbook, Status %in% c("Deceased", "D", "H", "Hypothetical") & Sex %in% c("Undetermined", "U")) %>% pull(ID) %>% unique()
  }
}

annotate_cohorts <- function(studbook, Year_min = NULL, Year_max = NULL, span = 10) {
  if (is.null(Year_min)) { Year_min <- min(studbook$Year_birth) } else { Year_min <- Year_min }
  if (is.null(Year_max)) { Year_max <- year(today()) } else { Year_max <- Year_max }
  cohort_map <- function(x) {
    list(start = x, end = x + (span - 1))
  }
  cohorts  <- map(as.list(seq(from = Year_min, to = (Year_max - span + 1), by = span)), \(x) cohort_map(x))
  specials <- filter(studbook, !is.na(name_spec)) %>% distinct(ID, Year_birth)




}

#' Generate cohort-structured data
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param Year_min Start year of cohorts (optional)
#' @param Year_max End year (optional)
#' @param span Years per cohort (optional)
#' @param age_max Max age to include (optional)
#' @param include_sex Whether to include sex as a grouping var (optional)
#' @return A joined and restructured tibble
#' @export
#'
#' @importFrom dplyr across arrange case_when filter full_join join_by mutate relocate right_join select
#' @importFrom lubridate year today
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid replace_na
#' @importFrom tidyselect where
#' @importFrom magrittr %>%
make_cohorts <- function(studbook, Year_min = NULL, Year_max = NULL, span = 10, age_max = NULL, include_sex = TRUE) {
  if (is.null(age_max)) { age_max <- max(studbook$age_last) } else { age_max <- age_max }
  if (is.null(Year_min)) { Year_min <- min(studbook$Year_birth) } else { Year_min <- Year_min }
  if (is.null(Year_max)) { Year_max <- year(today()) } else { Year_max <- Year_max }

  N_letters <- (Year_max - Year_min + 1)/span
  cohorts   <- expand_grid(
    Age        = 0:age_max,
    Sex        = c("M", "F"),
    Year_birth = Year_min:Year_max
  ) %>%
    full_join(tibble(Year_birth   = Year_min:Year_max,
                     Cohort_birth = rep(LETTERS[1:N_letters], each = span)),
              by = join_by(Year_birth)) %>%
    mutate(Cohort_min   = min(Year_birth),
           Cohort_max   = max(Year_birth),
           .by = Cohort_birth) %>%
    select(Cohort_min, Cohort_max, Cohort_birth, Year_birth, Sex, Age) %>%
    arrange(Cohort_birth, Year_birth, Sex, Age) %>%
    filter(Year_birth + Age <= year(today()))

  if (include_sex) {
    studbook %>%
      right_join(cohorts, by = join_by(Year_birth, Sex, Age)) %>%
      mutate(Cohort = paste0(Sex, Cohort_birth),
             across(where(is.numeric), ~ replace_na(., 0))) %>%
    mutate(Cohort_label = if_else(
      str_detect(name_spec, "\\w+"),
      str_glue("{Cohort_min}", "-", "{Cohort_max}", " \\(", "{name_spec}", "\\)"),
      str_glue("{Cohort_min}", "-", "{Cohort_max}")), .keep = "unused") %>%
      relocate(Cohort_label, Cohort_birth, Sex, Age)
  } else {
    studbook %>%
      right_join(distinct(cohorts, Age, Year_birth, Cohort_birth, Cohort_min, Cohort_max),
                 by = join_by(Year_birth, Age)) %>%
      mutate(Cohort = Cohort_birth,
             across(where(is.numeric), ~ replace_na(., 0)),
             Sex = "Total") %>%
      mutate(Cohort_label = if_else(
        str_detect(name_spec, "\\w+"),
        str_glue("{Cohort_min}", "-", "{Cohort_max}", " \\(", "{name_spec}", "\\)"),
        str_glue("{Cohort_min}", "-", "{Cohort_max}")), .keep = "unused") %>%
      relocate(Cohort_label, Cohort_birth, Sex, Age)
  }
}

#' Count births and track individual presence over time
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @return A tibble of births per ID by year
#' @export
#'
#' @importFrom dplyr count distinct filter if_else mutate select
#' @importFrom lubridate floor_date today year
#' @importFrom purrr map2
#' @importFrom tidyr replace_na unnest
count_births <- function(studbook) {
  births <- studbook %>%
    filter(Event == "breed") %>%
    select(ID, Date) %>%
    distinct() %>%
    mutate(Year = year(Date)) %>%
    summarize(Births = n(), .by = c(ID, Year))

  counts <- studbook %>%
    select(
      ID,
      Sex,
      Year_birth,
      Start     = Date_birth,
      End       = Date_last
    ) %>%
    filter(Sex != "U") %>%
    mutate(Start = floor_date(Start, "years"),
           End   = floor_date(End, "years")
    ) %>%
    mutate(Years = pmap(list(Start, End), \(x, y) seq(x, y, by = "years"))) %>%
    unnest(Years) %>%
    mutate(Year = year(Years),
           Age  = calculate_age(Start, Years)) %>%
    select(ID,
           Sex,
           Year_birth,
           Age,
           Year) %>%
    left_join(births, by = join_by(ID, Year)) %>%
    mutate(Births = replace_na(Births, 0)) %>%
    distinct() %>%
    select(ID,
           Year_birth,
           Sex,
           Age,
           Births)
}

#' Generate census summary by sex over time
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param period "years" or "months"
#' @return A wide-format census summary with counts per sex and date
#' @export
#'
#' @importFrom dplyr across arrange count distinct filter left_join join_by mutate n rename row_number select
#' @importFrom lubridate ceiling_date floor_date
#' @importFrom purrr map2 pmap
#' @importFrom tidyr pivot_wider unnest
#' @importFrom tidyselect where
census <- function(studbook, period) {
  studbook <- studbook_short(studbook)
  counts   <- studbook %>%
    mutate(Birth = floor_date(Date_birth, period),
           End   = ceiling_date(Date_last, period)) %>%
    mutate(Dates = pmap(list(Birth, End), \(x, y) seq(x, y, by = period))) %>%
    unnest(Dates) %>%
    select(ID, Date = Dates, Birth, Sex) %>%
    mutate(Age = calculate_age(Birth, Date)) %>%
    select(-Birth) %>%
    summarize(N = n(), .by = c(Sex, Date)) %>%
    distinct() %>%
    arrange(Date) %>%
    pivot_wider(id_cols     = "Date",
                names_from  = "Sex",
                values_from = "N") %>%
    rename(Females      = F,
           Males        = M,
           Unidentified = U) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
    filter(Date <= today())
  return(counts)
}

#' Estimate age at 50% survivorship (MLE age 1+)
#'
#' This function returns the age at which the first-life cohort (lx1)
#' reaches 0.5 using linear interpolation between the nearest age classes.
#'
#' @param lx A vector of Lx values
#' @param age A vector of corresponding ages
#'
#' @export
#'
#' @return A single numeric value representing the interpolated MLE age at lx1 = 0.5
#' @keywords internal
#' @importFrom dplyr filter mutate slice_min slice_max
#' @importFrom tibble tibble
mle <- function(lx, age) {
  df <- tibble(lx = lx, age = age) %>%
    filter(age >= 1, !is.na(lx), lx > 0)

  if (nrow(df) == 0) return(0)

  lx1_start <- df$lx[1]
  df <- df %>% mutate(lx1 = lx / lx1_start)

  below <- df %>% filter(lx1 <= 0.5) %>% slice_min(order_by = age, n = 1, with_ties = FALSE)
  above <- df %>% filter(lx1 >  0.5) %>% slice_max(order_by = age, n = 1, with_ties = FALSE)

  if (nrow(below) == 0 || nrow(above) == 0) return(0)

  age_low  <- above$age[1]
  age_high <- below$age[1]
  lx_low   <- above$lx1[1]
  lx_high  <- below$lx1[1]

  mle <- age_low + ((0.5 - lx_low) / (lx_high - lx_low)) * (age_high - age_low)
  return(mle)
}

