# life_tables.R
# Functions for calculating and formatting life tables
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
make_cohorts <- function(studbook,
                        Year_min    = NULL,
                        Year_max    = NULL,
                        span        = 10,
                        age_max     = NULL) {
  if (is.null(age_max)) {
    studbook_ages <- c(pull(studbook, age_event), pull(studbook, age_last))
    age_max <- max(studbook_ages)
  } else {
    age_max <- age_max
  }
  if (is.null(Year_min)) {
    Year_min <- min(studbook$Year_birth)
  } else {
    Year_min <- Year_min
  }
  if (is.null(Year_max)) {
    Year_max <- year(today())
  } else {
    Year_max <- Year_max
  }
  N_letters <- ceiling((Year_max - Year_min + 1) / span)
  tbl_join <- tibble(
    Year_birth   = Year_min:Year_max,
    Cohort_birth = rep(LETTERS[1:N_letters], each = span, length.out = length(Year_min:Year_max))
  )
  cohorts_bysex <- expand_grid(
    Age        = 0:age_max,
    Sex        = c("M", "F"),
    Year_birth = Year_min:Year_max
  ) %>%
    full_join(tbl_join, by = "Year_birth") %>%
    mutate(Cohort_min   = min(Year_birth),
           Cohort_max   = max(Year_birth),
           .by = Cohort_birth) %>%
    select(Cohort_min, Cohort_max, Cohort_birth, Year_birth, Sex, Age) %>%
    arrange(Cohort_birth, Year_birth, Sex, Age) %>%
    filter(Year_birth + Age <= year(today())) %>%
    mutate(Cohort = str_glue("{Sex}{Cohort_birth}"))

  cohorts <- cohorts_bysex %>%
    mutate(Sex    = "Total",
           Cohort = str_glue("T{Cohort_birth}")) %>%
    distinct() %>%
    bind_rows(cohorts_bysex)  %>%
    mutate(
      across(c(Cohort_min, Cohort_max), ~as.character(.)),
      Cohort_label = str_glue("{Cohort_min} - {Cohort_max}")) %>%
    arrange(Cohort_birth, Year_birth, Sex, Age) %>%
    select(Cohort, Cohort_label, Year_birth, Sex, Age)

  return(cohorts)
}

#' Label studbook rows by Birth Cohorts
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param Year_min Start year of cohorts (optional)
#' @param Year_max End year (optional)
#' @param span Years per cohort (optional)
#' @param age_max Max age to include (optional)
#' @return A joined and restructured tibble
#' @export
#'
#' @importFrom dplyr across arrange case_when filter full_join join_by mutate relocate right_join select
#' @importFrom lubridate year today
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid replace_na
#' @importFrom tidyselect where
#' @importFrom magrittr %>%

studbook_cohorts <- function(studbook,
                          Year_min    = NULL,
                          Year_max    = NULL,
                          span        = 10,
                          age_max     = NULL) {

  if (is.null(age_max)) {
    studbook_ages <- c(pull(studbook, age_event), pull(studbook, age_last))
    age_max <- max(studbook_ages)
  } else {
    age_max <- age_max
  }
  if (is.null(Year_min)) {
    Year_min <- min(studbook$Year_birth)
  } else {
    Year_min <- Year_min
  }
  if (is.null(Year_max)) {
    Year_max <- year(today())
  } else {
    Year_max <- Year_max
  }
  cohorts <- make_cohorts(studbook,
                          Year_min = Year_min,
                          Year_max = Year_max,
                          span     = span    ,
                          age_max  = age_max)
  cohorts_bysex <- filter(cohorts, Sex != "Total")
  cohorts_total <- filter(cohorts, Sex == "Total") %>%
    select(Total_Cohort       = Cohort,
           Total_Cohort_label = Cohort_label,
           Year_birth,
           age_event = Age)
  df <- studbook %>%
    left_join(cohorts_bysex, by = join_by(Year_birth, age_event == Age, Sex)) %>%
    left_join(cohorts_total, by = join_by(Year_birth, age_event)) %>%
    mutate(
      Cohort_label = if_else(
        is.na(name_spec), Cohort_label,
        as.character(str_glue("{Cohort_label} ({name_spec})"))),
      Total_Cohort_label = if_else(
        is.na(name_spec), Total_Cohort_label,
        as.character(str_glue("{Total_Cohort_label} ({name_spec})")))
      )
}

#' Create Special Cohort Labels based on values in `name_spec`
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param Year_min Start year of cohorts (optional)
#' @param Year_max End year (optional)
#' @param span Years per cohort (optional)
#' @param age_max Max age to include (optional)
#' @return A joined and restructured tibble
#' @export
#'
#' @importFrom dplyr across arrange case_when filter full_join join_by mutate relocate right_join select
#' @importFrom lubridate year today
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid replace_na
#' @importFrom tidyselect where
#' @importFrom magrittr %>%

special_cohorts <- function(studbook,
                            Year_min    = NULL,
                            Year_max    = NULL,
                            span        = 10,
                            age_max     = NULL) {

  if (is.null(age_max)) {
    studbook_ages <- c(pull(studbook, age_event), pull(studbook, age_last))
    age_max <- max(studbook_ages)
  } else {
    age_max <- age_max
  }
  if (is.null(Year_min)) {
    Year_min <- min(studbook$Year_birth)
  } else {
    Year_min <- Year_min
  }
  if (is.null(Year_max)) {
    Year_max <- year(today())
  } else {
    Year_max <- Year_max
  }

  cohorts_short <- make_cohorts(studbook,
                          Year_min = Year_min,
                          Year_max = Year_max,
                          span     = span    ,
                          age_max  = age_max
                          ) %>%
    select(-Age) %>% distinct()

  specials_total <- filter(studbook, !is.na(name_spec)) %>%
    studbook_short() %>%
    select(-Sex) %>%
    left_join(filter(cohorts_short, Sex == "Total"), by = join_by(Year_birth)) %>%
    mutate(Cohort_label = as.character(str_glue("{Cohort_label} ({name_spec})")))

  specials <- filter(studbook, !is.na(name_spec)) %>%
    studbook_short() %>%
    left_join(cohorts_short, by = join_by(Year_birth, Sex)) %>%
    mutate(Special_label = as.character(str_glue("{Cohort_label} ({name_spec})"))) %>%
    bind_rows(specials_total) %>%
    distinct(Cohort, Special_label)

  return(specials)
}

#' Create a version of the Cohorts Tibble with special labels
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param Year_min Start year of cohorts (optional)
#' @param Year_max End year (optional)
#' @param span Years per cohort (optional)
#' @param age_max Max age to include (optional)
#' @return A joined and restructured tibble
#' @export
#'
#' @importFrom dplyr across arrange case_when filter full_join join_by mutate relocate right_join select
#' @importFrom lubridate year today
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid replace_na
#' @importFrom tidyselect where
#' @importFrom magrittr %>%


annotated_cohorts <- function(studbook,
                             Year_min    = NULL,
                             Year_max    = NULL,
                             span        = 10,
                             age_max     = NULL) {
  if (is.null(age_max)) {
    studbook_ages <- c(pull(studbook, age_event), pull(studbook, age_last))
    age_max <- max(studbook_ages)
  } else {
    age_max <- age_max
  }
  if (is.null(Year_min)) {
    Year_min <- min(studbook$Year_birth)
  } else {
    Year_min <- Year_min
  }
  if (is.null(Year_max)) {
    Year_max <- year(today())
  } else {
    Year_max <- Year_max
  }

  specials <- special_cohorts(
    studbook,
    Year_min = Year_min,
    Year_max = Year_max,
    span     = span    ,
    age_max  = age_max
  )

  annotated_cohorts <- make_cohorts(
    studbook,
    Year_min = Year_min,
    Year_max = Year_max,
    span     = span    ,
    age_max  = age_max
  ) %>%
    left_join(specials, by = "Cohort") %>%
    mutate(Cohort_label = if_else(!is.na(Special_label), Special_label, Cohort_label), .keep = "unused") %>%
    distinct()

  return(annotated_cohorts)
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
  if (is.null(age_max)) {
    studbook_ages <- c(pull(studbook, age_event), pull(studbook, age_last))
    age_max <- max(studbook_ages)
  } else {
    age_max <- age_max
  }
  if (is.null(Year_min)) {
    Year_min <- min(studbook$Year_birth)
  } else {
    Year_min <- Year_min
  }
  if (is.null(Year_max)) {
    Year_max <- year(today())
  } else {
    Year_max <- Year_max
  }

  studbook_life <- filter(studbook, between(Year_birth, Year_min, Year_max))

   cohorts_make <- annotated_cohorts(
    studbook,
    Year_min = Year_min,
    Year_max = Year_max,
    span     = span    ,
    age_max  = age_max
    )

  life_table <- count_births(studbook_life) %>%
    mutate(Sex = "Total") %>%
    bind_rows(count_births(studbook_life)) %>%
    arrange(Year_birth, Sex, Age) %>%
    group_by(Year_birth, Sex, Age) %>%
    summarize(Births = sum(Births),
              Nx     = n()) %>%
    ungroup() %>%
    left_join(cohorts_make, by = join_by(Year_birth, Sex, Age)) %>%
    mutate(across(c(Births, Nx), ~replace_na(., 0))) %>%
    group_by(Cohort_label, Cohort, Sex, Age) %>%
    summarize(Births     = sum(Births),
              Nx         = sum(Nx)) %>%
    ungroup() %>%
    arrange(Cohort, Age)  %>%
    filter(!is.na(Cohort)) %>%
    lifeTab()

  return(life_table)
}

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
    mutate(Qx_risk = Nx,
           Mx_risk = Nx) %>%
    mutate(Deaths = if_else(Age == max(Age), Nx, Nx - lead(Nx)),
           N0     = first(Nx),
           N1     = nth(Nx, 2),
           .by = Cohort) %>%
    mutate(Px     = if_else(Nx == 0 | Age == max(Age), 0, lead(Nx)/Nx),
           .by = Cohort) %>%
    ungroup()  %>%
    rowwise() %>%
    mutate(Lx1 = if_else(N1 == 0, 0, Nx / N1),
           Lx  = if_else(N0 == 0, 0, Nx / N0),
           Qx  = if_else(Qx_risk == 0, 0, Deaths / Qx_risk),
           Mx  = if_else(Mx_risk == 0, 0, (Births / Mx_risk) / 2)) %>%
    mutate(Fx   = Mx * Lx) %>%
    mutate(numT = Age * Fx) %>%
    ungroup() %>%
    mutate(Qx_1        = first(Qx),
           MLE         = if_else(max(N1) > 0, mle(Lx1, Age), 0),
           repro_first = min(Age[Mx > 0]),
           repro_last  = max(Age[Mx > 0]),
           age_max     = max(Age[Deaths > 0]),
           R0          = sum(Fx),
           Tnum        = sum(numT),
           .by = Cohort) %>%
    rowwise() %>%
    mutate(T = if_else(R0 > 0, Tnum / R0, 0)) %>%
    mutate(lambda = if_else(R0 > 0 & T > 0, R0^(1/T), 0)) %>%
    ungroup() %>%
    select(
      Cohort_label,
      Cohort,
      Sex,
      Age,
      Births,
      Deaths,
      N1,
      Qx_risk,
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
      Fx
    )
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
    select(
      Cohort_label,
      Cohort,
      Sex,
      N0,
      Qx_1,
      R0,
      T,
      MLE,
      lambda,
      repro_first,
      repro_last,
      age_max
    ) %>%
    mutate(across(c(N0:age_max), ~ round(., 3))) %>%
    filter(N0 > 0) %>%
    distinct() %>%
    arrange(Cohort, Sex) %>%
    annotate_lambda()
}

