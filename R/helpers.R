
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
  }
}

#' Generate cohort-structured data
#'
#' @param df A tibble of birth records
#' @param Year_min Start year of cohorts
#' @param Year_max End year
#' @param span Years per cohort
#' @param age_max Max age to include
#' @param include_sex Whether to include sex as a grouping var
#' @return A joined and restructured tibble
#' @export
#'
#' @importFrom dplyr across arrange case_when filter full_join join_by mutate relocate right_join select
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid replace_na
#' @importFrom tidyselect where
#' @importFrom magrittr %>%
make_cohorts <- function(df, Year_min, Year_max, span, age_max, include_sex = TRUE) {
  N_letters <- (Year_max - Year_min + 1)/span
  cohorts <- expand_grid(
    Age        = 0:age_max,
    Sex        = c("M", "F"),
    Year_birth = Year_min:Year_max
  ) %>%
    full_join(tibble(Year_birth   = Year_min:Year_max,
                     Cohort_birth = rep(LETTERS[1:N_letters], each = span)),
              by = join_by(Year_birth)) %>%
    mutate(Cohort_min   = min(Year_birth), Cohort_max = max(Year_birth), .by = Cohort_birth) %>%
    mutate(Cohort_label = if_else(
      str_detect(name_spec, "\\w+"),
      str_glue("{Cohort_min}", "-", "{Cohort_max}", " \\(", "{name_spec}", "\\)"),
      str_glue("{Cohort_min}", "-", "{Cohort_max}"))) %>%
    select(Cohort_label, Cohort_birth, Year_birth, Sex, Age) %>%
    arrange(Cohort_birth, Year_birth, Sex, Age) %>%
    filter(Year_birth + Age <= 2025)

  if (include_sex) {
    df %>%
      right_join(cohorts, by = join_by(Year_birth, Sex, Age)) %>%
      mutate(Cohort = paste0(Sex, Cohort_birth),
             across(where(is.numeric), ~ replace_na(., 0)))
  } else {
    df %>%
      right_join(distinct(cohorts, Age, Year_birth, Cohort_birth, Cohort_label),
                 by = join_by(Year_birth, Age)) %>%
      mutate(Cohort = Cohort_birth,
             across(where(is.numeric), ~ replace_na(., 0)),
             Sex = "Total") %>%
      relocate(Cohort_label, Cohort_birth, Sex, Age)
  }
}
