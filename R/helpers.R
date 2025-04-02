
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
#' @param minYear Start year of cohorts
#' @param maxYear End year
#' @param span Years per cohort
#' @param maxAge Max age to include
#' @param include_sex Whether to include sex as a grouping var
#' @return A joined and restructured tibble
#' @export
#'
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr join_by
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr right_join
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid
#' @importFrom tidyr replace_na
#' @importFrom tidyselect where
#' @importFrom magrittr %>%
make_cohorts <- function(df, minYear, maxYear, span, maxAge, include_sex = TRUE) {
  N_letters <- (maxYear - minYear + 1)/span
  cohorts <- expand_grid(
    Age = 0:maxAge,
    Sex = c("M", "F"),
    BirthYear = minYear:maxYear
  ) %>%
    full_join(tibble(BirthYear = minYear:maxYear,
                     BirthCohort = rep(LETTERS[1:N_letters], each = span)),
              by = join_by(BirthYear)) %>%
    mutate(CohortMin = min(BirthYear), CohortMax = max(BirthYear), .by = BirthCohort) %>%
    mutate(CohortLabel = case_when(
      CohortMin <= 2013 & CohortMax >= 2013 ~ paste0(CohortMin, "-", CohortMax, "\n(Culi)"),
      CohortMin <= 2017 & CohortMax >= 2013 ~ paste0(CohortMin, "-", CohortMax, "\n(Warble)"),
      TRUE                                  ~ paste0(CohortMin, "-", CohortMax)
    )) %>%
    select(CohortLabel, BirthCohort, BirthYear, Sex, Age) %>%
    arrange(BirthCohort, BirthYear, Sex, Age) %>%
    filter(BirthYear + Age <= 2025)

  if (include_sex) {
    df %>%
      right_join(cohorts, by = join_by(BirthYear, Sex, Age)) %>%
      mutate(Cohort = paste0(Sex, BirthCohort),
             across(where(is.numeric), ~ replace_na(., 0)))
  } else {
    df %>%
      right_join(distinct(cohorts, Age, BirthYear, BirthCohort, CohortLabel),
                 by = join_by(BirthYear, Age)) %>%
      mutate(Cohort = BirthCohort,
             across(where(is.numeric), ~ replace_na(., 0)),
             Sex = "Total") %>%
      relocate(CohortLabel, BirthCohort, Sex, Age)
  }
}
