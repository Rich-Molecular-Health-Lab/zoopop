# functions.R
# Core demographic and pedigree calculation functions
#
#' Import studbook demo data
#'
#' @return Studbook tibble for use in tutorial script
#' @export
#'
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
load_studbook <- function() {
  read_tsv(system.file("extdata", "studbook.tsv", package = "zoolabs")) %>%
    mutate(across(c(DateBirth, DateDeath),  ~ymd(.)))
}

#' Import timeline demo data
#'
#' @return Timeline tibble for use in tutorial script
#' @export
#'
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd
#' @importFrom magrittr %>%
#' @importFrom readr read_tsv
load_timeline <- function() {
  read_tsv(system.file("extdata", "timeline.tsv", package = "zoolabs")) %>%
    mutate(across(c(StartLoc, Date, EndLoc), ~ ymd(.)))
}
#' Calculate age in years
#'
#' @param birth Date of birth
#' @param date Reference date
#' @return Integer age in years
#' @export
#'
#' @importFrom lubridate as.period
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate interval
calculate_age <- function(birth, date) {
  floor(as.numeric(as.period(interval(floor_date(birth, "month"), ceiling_date(date, "month")), unit = "years"), "years"))
}

#' Return vector of living or deceased IDs
#' @param studbook Studbook data
#' @rdname studbook_ids
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom magrittr %>%
living <- function(studbook) {
  filter(studbook, Status %in% c("Alive", "A")) %>% pull(ID) %>% unique()
}

#' @rdname studbook_ids
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom magrittr %>%
deceased <- function(studbook) {
  filter(studbook, Status %in% c("Deceased", "D", "H", "Hypothetical")) %>% pull(ID) %>% unique()
}

#' @rdname studbook_ids
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom magrittr %>%
living.males <- function(studbook) {
  filter(studbook, Status == "Alive", Sex %in% c("M", "Male")) %>% pull(ID) %>% unique()
}

#' @rdname studbook_ids
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom magrittr %>%
living.females <- function(studbook) {
  filter(studbook, Status == "Alive", Sex %in% c("F", "Female")) %>% pull(ID) %>% unique()
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

#' Nest timeline data by group
#'
#' @param timeline Timeline tibble
#' @param groupBy Optional grouping variable (e.g., "Location", "Age", "Sex")
#' @return Nested list by Date
#' @export
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom purrr map
nest_timeline <- function(timeline, groupBy = NULL) {
  switch(groupBy,
         Location = timeline %>%
           group_by(Date, Location) %>%
           summarise(Individuals = list(tibble(ID, Sex, BirthYear, Age)), .groups = "drop") %>%
           group_by(Date) %>%
           summarise(Locations = split(Individuals, Location)) %>%
           split(.$Date) %>%
           map(~ .x$Locations),
         Age = timeline %>%
           group_by(Date, Age) %>%
           summarise(Individuals = list(tibble(ID, Sex, BirthYear, Location))) %>%
           group_by(Date) %>%
           summarise(Ages = split(Individuals, Age)) %>%
           split(.$Date) %>%
           map(~ .x$Ages),
         Sex = timeline %>%
           group_by(Date, Sex) %>%
           summarise(Individuals = list(tibble(ID, Age, BirthYear, Location))) %>%
           group_by(Date) %>%
           summarise(Sexes = split(Individuals, Sex)) %>%
           split(.$Date) %>%
           map(~ .x$Sexes),
         AgeSex = timeline %>%
           group_by(Date, Sex, Age) %>%
           summarise(Individuals = list(tibble(ID, BirthYear, Location))) %>%
           group_by(Date) %>%
           summarise(Classes = split(Individuals, Sex, Age)) %>%
           split(.$Date) %>%
           map(~ .x$Classes),
         # default case (NULL)
         timeline %>%
           group_by(Date) %>%
           summarise(Individuals = list(tibble(ID, Sex, BirthYear, Age)), .groups = "drop") %>%
           split(.$Date))
}
#' Count births and track individual presence over time
#'
#' @param timeline A tibble with at least `ID`, `Date`, and `TypeEvent` columns
#' @param studbook A studbook tibble with birth/death info
#' @return A tibble of births per ID by year
#' @export
#'
#' @importFrom dplyr count
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate floor_date
#' @importFrom lubridate today
#' @importFrom lubridate year
#' @importFrom purrr map2
#' @importFrom tidyr replace_na
#' @importFrom tidyr unnest
count_births <- function(timeline, studbook) {
  births <- timeline %>%
    filter(TypeEvent == "Breed") %>%
    select(ID, Date) %>%
    distinct() %>%
    mutate(Year = year(Date)) %>%
    summarize(Births = n(), .by = c(ID, Year))

  counts <- studbook %>%
    select(
      ID,
      Sex,
      BirthYear,
      Start     = DateBirth,
      End       = DateDeath
    ) %>%
    filter(Sex != "U") %>%
    mutate(Start = floor_date(Start, "years"),
           End   = if_else(!is.na(End),
                           floor_date(End, "years"),
                           floor_date(today(), "years"))
    ) %>%
    mutate(Years = pmap(list(Start, End), \(x, y) seq(x, y, by = "years"))) %>%
    unnest(Years) %>%
    mutate(Year = year(Years),
           Age  = calculate_age(Start, Years)) %>%
    select(ID,
           Sex,
           BirthYear,
           Age,
           Year) %>%
    left_join(births, by = join_by(ID, Year)) %>%
    mutate(Births = replace_na(Births, 0)) %>%
    distinct() %>%
    select(ID,
           BirthYear,
           Sex,
           Age,
           Births)
}

#' Generate census summary by sex over time
#'
#' @param timeline A tibble of life events with `Date`, `ID`, and `TypeEvent`
#' @param studbook A tibble with `ID` and `Sex`
#' @param period "years" or "months"
#' @return A wide-format census summary with counts per sex and date
#' @export
#'
#' @importFrom dplyr across
#' @importFrom dplyr arrange
#' @importFrom dplyr count
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr join_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr rename
#' @importFrom dplyr row_number
#' @importFrom dplyr select
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate floor_date
#' @importFrom purrr map2
#' @importFrom purrr pmap
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unnest
#' @importFrom tidyselect where
census <- function(timeline, studbook, period) {
  counts <- timeline %>%
    filter(TypeEvent %in% c("Birth", "End")) %>%
    distinct(ID, Date, TypeEvent) %>%
    pivot_wider(id_cols     = "ID",
                names_from  = "TypeEvent",
                values_from = "Date") %>%
    distinct() %>%
    mutate(Birth = floor_date(Birth, "years"),
           End   = ceiling_date(End, "years")) %>%
    mutate(Dates = pmap(list(Birth, End), \(x, y) seq(x, y, by = "year"))) %>%
    unnest(Dates) %>%
    select(ID, Date = Dates) %>%
    mutate(Age = row_number() - 1, .by = ID) %>%
    left_join(select(studbook,
                     ID,
                     Sex), by = join_by(ID)) %>%
    summarize(N = n(), .by = c(Sex, Date)) %>%
    distinct() %>%
    arrange(Date) %>%
    pivot_wider(id_cols     = "Date",
                names_from  = "Sex",
                values_from = "N") %>%
    rename(Females      = F,
           Males        = M,
           Unidentified = U) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  return(counts)
}
