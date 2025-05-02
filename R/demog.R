# demog.R
# Functions for calculating and formatting demographic variables
#' Generate a list of default parameters for setting up cohort tibbles
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @return A list of named values to use for cohort defaults in other functions
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @export
#'
#' @importFrom dplyr filter pull
#' @importFrom lubridate year today
#' @importFrom magrittr %>%

cohort_defaults <- function(studbook, cohort_params = list(NULL, ...)) {
  captive_births <- filter(studbook, Type_birth == "Captive")
  studbook_ages  <- c(pull(studbook, age_event), pull(studbook, age_last))
  defaults <- list(Year_min = min(captive_births$Year_birth) - 1,
                   Year_max = year(today()) - 1,
                   span     = 10,
                   age_max  = max(studbook_ages)
                   )
  params <- modifyList(defaults, cohort_params)

  return(params)
}

#' Generate cohort-structured data
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @return A joined and restructured tibble
#' @noRd
#'
#' @importFrom dplyr across arrange filter full_join mutate select pull bind_rows
#' @importFrom lubridate year today
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid
#' @importFrom magrittr %>%
make_cohorts <- function(studbook, cohort_params = NULL) {
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  N_letters <- ceiling((params$Year_max - params$Year_min + 1) / params$span)
  tbl_join <- tibble(
    Year_birth   = params$Year_min:params$Year_max,
    Cohort_birth = rep(LETTERS[1:N_letters], each = params$span, length.out = length(params$Year_min:params$Year_max))
  )
  cohorts_bysex <- expand_grid(
    Age        = 0:params$age_max,
    Sex        = c("M", "F"),
    Year_birth = params$Year_min:params$Year_max
  ) %>%
    full_join(tbl_join, by = "Year_birth") %>%
    mutate(Cohort_min   = min(Year_birth),
           Cohort_max   = max(Year_birth),
           .by = Cohort_birth) %>%
    select(Cohort_min, Cohort_max, Cohort_birth, Year_birth, Sex, Age) %>%
    arrange(Cohort_birth, Year_birth, Sex, Age) %>%
    filter(Year_birth + Age <= year(today())) %>%
    mutate(Cohort = as.character(str_glue("{Sex}{Cohort_birth}")))

  cohorts <- cohorts_bysex %>%
    mutate(Sex    = "Total",
           Cohort = as.character(str_glue("T{Cohort_birth}"))) %>%
    distinct() %>%
    bind_rows(cohorts_bysex)  %>%
    mutate(
      across(c(Cohort_min, Cohort_max), ~as.character(.)),
      Cohort_label = as.character(str_glue("{Cohort_min} - {Cohort_max}"))) %>%
    arrange(Cohort_birth, Year_birth, Sex, Age) %>%
    select(Cohort, Cohort_label, Year_birth, Sex, Age)

  return(cohorts)
}

#' Label studbook rows by Birth Cohorts
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @return A joined and restructured tibble
#' @export
#'
#' @importFrom dplyr select left_join filter join_by mutate if_else
#' @importFrom lubridate year today
#' @importFrom stringr str_glue
#' @importFrom magrittr %>%

studbook_cohorts <- function(studbook, cohort_params = NULL) {
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  cohorts <- make_cohorts(studbook, cohort_params = params)
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
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @return A joined and restructured tibble
#' @noRd
#'
#' @importFrom dplyr select distinct left_join filter join_by mutate bind_rows
#' @importFrom lubridate year today
#' @importFrom stringr str_glue
#' @importFrom magrittr %>%

special_cohorts <- function(studbook, cohort_params = NULL) {
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  cohorts_short <- make_cohorts(studbook, cohort_params = params) %>%
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
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @return A joined and restructured tibble
#' @noRd
#'
#' @importFrom dplyr left_join mutate if_else distinct relocate
#' @importFrom lubridate year today
#' @importFrom stringr str_sub
#' @importFrom magrittr %>%
#'
annotated_cohorts <- function(studbook, cohort_params = NULL) {
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  specials       <- special_cohorts(studbook = studbook, cohort_params = params)
  annotated_cohorts <- make_cohorts(studbook = studbook, cohort_params = params) %>%
    left_join(specials, by = "Cohort") %>%
    mutate(Cohort_years = Cohort_label,
           Cohort_period   = as.character(str_sub(Cohort, 2L, 2L))) %>%
    mutate(Cohort_label = if_else(
      !is.na(Special_label),
      Special_label,
      Cohort_label),
           .keep = "unused") %>%
    relocate(Cohort_period, Cohort_years, Cohort, Sex, Cohort_label) %>%
    distinct()

  return(annotated_cohorts)
}

#' Generate demography summary of studbook data over cohorts
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @return A tibble with cohort life table summary
#' @export
#'
#' @importFrom dplyr pull filter between mutate bind_rows arrange group_by summarize ungroup left_join join_by n
#' @importFrom lubridate year today
#' @importFrom tidyr replace_na
#'
cohort_demog <- function(studbook, cohort_params = NULL) {
 params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
 captive_births <- filter(studbook, Type_birth == "Captive")
  studbook_life <- filter(studbook, between(Year_birth, params$Year_min, params$Year_max))

  cohorts_make <- annotated_cohorts(studbook, cohort_params = params)

  demog_table <- count_births(studbook_life) %>%
    mutate(Sex = "Total") %>%
    bind_rows(count_births(studbook_life)) %>%
    arrange(Year_birth, Sex, Age) %>%
    group_by(Year_birth, Sex, Age) %>%
    summarize(Births = sum(Births),
              Nx     = n()) %>%
    ungroup() %>%
    left_join(cohorts_make, by = join_by(Year_birth, Sex, Age)) %>%
    mutate(across(c(Births, Nx), ~replace_na(., 0))) %>%
    group_by(Cohort_period, Cohort_years, Cohort, Cohort_label, Sex, Age) %>%
    summarize(Births     = sum(Births),
              Nx         = sum(Nx)) %>%
    ungroup() %>%
    arrange(Cohort_period, Cohort, Age)  %>%
    filter(!is.na(Cohort)) %>%
    demog_tab()   %>%
    filter(N0 > 0) %>%
    annotate_lambda() %>%
    distinct()

  cohorts <- demog_table %>%
    pull(Cohort_period) %>%
    unique()

  cohort_colors <- as.list(paletteer_c("harrypotter::ronweasley2", length(cohorts))) %>%
    set_names(cohorts) %>%
    enframe(name = "Cohort_period", value = "Cohort_color") %>%
    mutate(Cohort_color = as.character(Cohort_color))

  data <- demog_table %>%
    left_join(cohort_colors, by = "Cohort_period")

  return(data)
}

#' Build a full demographic life table from cohort data
#'
#' @param df A cohort-formatted tibble with Births, Nx, Age, Cohort
#' @return A life table with survivorship, mortality, and reproductive values
#' @noRd
#'
#' @importFrom dplyr mutate if_else first nth lead ungroup rowwise select
demog_tab <- function(df) {
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
      group_by(Cohort) %>%
      arrange(desc(Age), .by_group = TRUE) %>%
      mutate(Tx = cumsum(Lx)) %>%
      arrange(Age, .by_group = TRUE) %>%
      mutate(ex = if_else(Lx > 0, Tx / Lx, NA)) %>%
      ungroup() %>%
      mutate(r = if_else(lambda > 0, log(lambda), NA)) %>%
    select(
      Cohort_period,
      Cohort_years,
      Cohort,
      Cohort_label,
      Sex,
      N0         ,
      N1         ,
      Qx_1       ,
      R0         ,
      T          ,
      MLE        ,
      repro_first,
      repro_last ,
      age_max    ,
      lambda     ,
      r,
      Age,
      Births,
      Deaths,
      Nx    ,
      Qx_risk,
      Qx    ,
      Lx    ,
      Lx1   ,
      Px    ,
      ex    ,
      Tx    ,
      Mx_risk,
      Mx    ,
      Fx    ,
      numT
    ) %>%
    distinct()
}

#' Build a full demographic life table for the population historic totals
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @return A life table with survivorship, mortality, and reproductive values that can be used as a footer row for comparison across cohorts
#' @noRd
#'
#' @importFrom dplyr mutate bind_rows arrange group_by summarize n ungroup if_else lead first nth rowwise desc select distinct filter
demog_ungrouped <- function(studbook) {
  count_births(studbook = studbook) %>%
    mutate(Sex = "Total") %>%
    bind_rows(count_births(studbook = studbook)) %>%
    arrange(Sex, Age) %>%
    group_by(Sex, Age) %>%
    summarize(Births = sum(Births),
              Nx     = n()) %>%
    ungroup() %>%
    arrange(Age)  %>%
    mutate(Qx_risk = Nx,
           Mx_risk = Nx) %>%
    mutate(Deaths = if_else(Age == max(Age), Nx, Nx - lead(Nx)),
           N0     = first(Nx),
           N1     = nth(Nx, 2),
           .by = Sex) %>%
    mutate(Px     = if_else(Nx == 0 | Age == max(Age), 0, lead(Nx)/Nx),
           .by = Sex) %>%
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
           .by = Sex) %>%
    rowwise() %>%
    mutate(T = if_else(R0 > 0, Tnum / R0, 0)) %>%
    mutate(lambda = if_else(R0 > 0 & T > 0, R0^(1/T), 0)) %>%
    ungroup() %>%
    group_by(Sex) %>%
    arrange(desc(Age), .by_group = TRUE) %>%
    mutate(Tx = cumsum(Lx)) %>%
    arrange(Age, .by_group = TRUE) %>%
    mutate(ex = if_else(Lx > 0, Tx / Lx, NA)) %>%
    ungroup() %>%
    mutate(r = if_else(lambda > 0, log(lambda), NA)) %>%
    select(
      Sex,
      N0         ,
      N1         ,
      Qx_1       ,
      R0         ,
      T          ,
      MLE        ,
      repro_first,
      repro_last ,
      age_max    ,
      lambda     ,
      r,
      Age,
      Births,
      Deaths,
      Nx    ,
      Qx_risk,
      Qx    ,
      Lx    ,
      Lx1   ,
      Px    ,
      ex    ,
      Tx    ,
      Mx_risk,
      Mx    ,
      Fx    ,
      numT
    ) %>%
    distinct()   %>%
    filter(N0 > 0) %>%
    annotate_lambda() %>%
    mutate(Cohort_years = "All years",
           Cohort_color = "black",
           sex_col      = "black")
}

#' Reduce full life table to summary stats for the population across all years and cohorts
#'
#' @param studbook Studbook tibble
#' @return Condensed summary table with lambda and vital rates (3 rows - Females, Males, and Overall stats)
#' @export
#'
#' @importFrom dplyr filter mutate select across ungroup distinct
lifetime_total <- function(studbook) {
  demog_ungrouped(studbook = studbook)  %>%
    filter(Sex == "Overall") %>%
    mutate(Sex = "Summary", N1 = NA) %>%
    select(
      Cohort_years,
      Cohort_color,
      Sex        ,
      sex_col    ,
      N0         ,
      N1         ,
      Qx_1       ,
      R0         ,
      T          ,
      MLE        ,
      repro_first,
      repro_last ,
      age_max    ,
      lambda     ,
      r
    ) %>%
    mutate(across(c(N0:r), ~ round(., 3))) %>%
    ungroup() %>%
    distinct()
}

#' Reduce full life table to summary format for the population across all years and cohorts
#'
#' @param studbook Studbook tibble
#' @return Nested, condensed summary table with stats calculated by age in list-cols
#' @export
#'
#' @importFrom dplyr distinct filter mutate select group_by summarize ungroup
#' @importFrom forcats fct_recode
demog_summary_total <- function(studbook) {
  demog_ungrouped(studbook = studbook)  %>%
    distinct()  %>%
    filter(N0 > 0) %>%
    annotate_lambda() %>%
    mutate(Sex          = fct_recode(Sex, Overall = "Total", Males = "M", Females = "F"),
           Cohort_years = "All years",
           Cohort_color = "black",
           sex_col      = "black") %>%
    filter(Sex == "Overall") %>%
    mutate(Sex = "Summary", N1 = NA) %>%
    select(
      Cohort_years ,
      Sex          ,
      sex_col      ,
      N1           ,
      Qx_1         ,
      R0           ,
      T            ,
      MLE          ,
      repro_first  ,
      repro_last   ,
      age_max      ,
      lambda       ,
      r            ,
      Age          ,
      Births       ,
      Deaths       ,
      Nx           ,
      Qx           ,
      Lx1          ,
      Px           ,
      ex           ,
      Tx           ,
      Mx           ,
      Fx           ,
      numT         ,
      Cohort_color
    ) %>%
    group_by(
      Cohort_years ,
      Cohort_color ,
      Sex          ,
      sex_col      ,
      N1           ,
      R0           ,
      T            ,
      MLE          ,
      repro_first  ,
      repro_last   ,
      age_max      ,
      lambda       ,
      r            ) %>%
    summarize(Age     = list(Age)         ,
              Births  = list(Births)      ,
              Deaths  = list(Deaths )     ,
              Nx      = list(Nx     )     ,
              Qx      = list(Qx     )     ,
              Lx1     = list(Lx1    )     ,
              Px      = list(Px     )     ,
              ex      = list(ex     )     ,
              Tx      = list(Tx     )     ,
              Mx      = list(Mx     )     ,
              Fx      = list(Fx     )     ,
              numT    = list(numT   )
    ) %>%
    ungroup()
}


#' Reduce full life table to summary format for visualization
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @return Nested summary table with stats calculated by age in list-cols
#' @export
#'
#' @importFrom dplyr mutate arrange case_match select group_by summarize ungroup
#' @importFrom forcats fct_recode fct_relevel
demog_summary <- function(studbook, cohort_params = NULL) {
  colors   <- set_colors()
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  data <- cohort_demog(studbook = studbook, cohort_params = params)  %>%
    mutate(Sex = fct_recode(Sex, Overall = "Total", Males = "M", Females = "F")) %>%
    mutate(Sex = fct_relevel(Sex, "Males", "Females", "Overall")) %>%
    arrange(Cohort_period, Sex) %>%
    mutate(sex_col = case_match(Sex,
                                "Overall" ~colors[["t"]],
                                "Males"   ~colors[["m"]],
                                "Females" ~colors[["f"]]
    )) %>%
    select(
      Cohort_years ,
      Cohort_color ,
      Sex          ,
      sex_col      ,
      N1           ,
      Qx_1         ,
      R0           ,
      T            ,
      MLE          ,
      repro_first  ,
      repro_last   ,
      age_max      ,
      lambda       ,
      r            ,
      Age          ,
      Births       ,
      Deaths       ,
      Nx           ,
      Qx           ,
      Lx1          ,
      Px           ,
      ex           ,
      Tx           ,
      Mx           ,
      Fx           ,
      numT
    ) %>%
    group_by(
      Cohort_years ,
      Cohort_color ,
      Sex          ,
      sex_col      ,
      N1           ,
      R0           ,
      T            ,
      MLE          ,
      repro_first  ,
      repro_last   ,
      age_max      ,
      lambda       ,
      r            ) %>%
    summarize(Age     = list(Age)         ,
              Births  = list(Births)      ,
              Deaths  = list(Deaths )     ,
              Nx      = list(Nx     )     ,
              Qx      = list(Qx     )     ,
              Lx1     = list(Lx1    )     ,
              Px      = list(Px     )     ,
              ex      = list(ex     )     ,
              Tx      = list(Tx     )     ,
              Mx      = list(Mx     )     ,
              Fx      = list(Fx     )     ,
              numT    = list(numT   )
    ) %>%
    ungroup()
  return(data)
}
