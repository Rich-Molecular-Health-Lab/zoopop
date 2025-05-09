#' Generate a summary of deaths across population history
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param window vector of start and end date (must be provided as `ymd()` values) to use as the demographic window for calculations
#' (default is the date of the first captive birth recorded and the most recent date of birth recorded)
#' @return Tibble summarizing the deaths across studbook records within the demographic window
#' (`ID` of deceased individual, `Sex` of individual, `Type_birth` indicating `Wild` for wild captures or
#'  `Captive` for captive-born individuals, `Qtr_born` is the date used to represent birth dates - which are converted to the starting date of the annual quarter,
#'  `Qtr_death`is the date used to represent death dates - which are converted to the starting date of the annual quarter, `x_death` is the approximate age class in years
#'  in which the individual dies based on the difference between the start of the quarter of their birth and the start of the quarter of their death)
#' @export
#' @importFrom dplyr filter pull between distinct rowwise mutate ungroup select
#' @importFrom lubridate quarter time_length interval
#'
mortality_history <- function(studbook, window = NULL) {
  first_captive <- filter(studbook, Type_birth == "Captive") %>% pull(Date_birth) %>% min()
  if (is.null(window)) {
    window <- c(first_captive, max(studbook$Date_birth))
  }
  deaths <- studbook %>%
    filter(between(Date_birth, window[1], window[2]) & Status == "D") %>%
    distinct(
      ID,
      Sex,
      Date_birth,
      Date_last,
      Type_birth
    ) %>%
    rowwise() %>%
    mutate(Qtr_born = quarter(Date_birth, type = "date_first"),
           Qtr_death = quarter(Date_last, type = "date_first"), .keep = "unused") %>%
    mutate(x_death = ceiling(time_length(interval(Qtr_born, Qtr_death), "year"))) %>%
    ungroup() %>%
    select(
      ID,
      Sex,
      Type_birth,
      Qtr_born,
      Qtr_death,
      x_death
    )
  return(deaths)
}
#' Generate a tibble with one row per year of life for each studbook ID born within a demographic window
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param window vector of start and end date (must be provided as `ymd()` values) to use as the demographic window for calculations
#' (default is the date of the first captive birth recorded and the most recent date of birth recorded)
#' @return Tibble with one row per year of life for each studbook ID so that the `nrow()` per id matches their age in years at death (or current age, if living)
#' Note that `x` represents the age in years, which is calculated based on start of annual quarters so that an individuals date of birth is rolled back to the start of a given annual quarter,
#' and `x_start` provides the date of that individual's birth quarter and `x_end` provides the last date within that age interval for the individual.
#' @export
#' @importFrom dplyr filter pull between distinct rowwise mutate ungroup select arrange
#' @importFrom lubridate quarter time_length interval years days
#' @importFrom purrr pmap
#' @importFrom tidyr unnest
#'
age_classes_ids <- function(studbook, window = NULL) {
  first_captive <- filter(studbook, Type_birth == "Captive") %>% pull(Date_birth) %>% min()
  if (is.null(window)) {
    window <- c(first_captive, max(studbook$Date_birth))
  }
  age_classes <- studbook %>%
    filter(between(Date_birth, window[1], window[2]) & Status %in% c("A", "D")) %>%
    mutate(Qtr_born = quarter(Date_birth, type = "date_first"),
           Qtr_last  = quarter(Date_last, type = "date_first")) %>%
    mutate(x_last = ceiling(time_length(interval(Qtr_born, Qtr_last), "year"))) %>%
    distinct(
      ID,
      Sex,
      Type_birth,
      Status,
      Qtr_born,
      x_last
      ) %>%
    mutate(x = pmap(list(0, x_last), \(x, y) seq(x, y, by = 1))) %>%
    unnest(x) %>%
    rowwise() %>%
    mutate(x_start = Qtr_born + years(x)) %>%
    mutate(x_end   = x_start + years(1) - days(1)) %>%
    ungroup() %>%
    select(
      ID,
      Sex,
      Type_birth,
      Status,
      x,
      x_start,
      x_end
    ) %>%
    arrange(ID, x_start, x)
  return(age_classes)
}

#' Generate a summary of reproductive events across population history
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param window vector of start and end date (must be provided as `ymd()` values) to use as the demographic window for calculations
#' (default is the date of the first captive birth recorded and the most recent date of birth recorded)
#' @return Tibble summarizing the reproductive events across studbook records within the demographic window
#' (`ID` of deceased individual, `Sex` of individual, `Type_birth` (represents the parent/ID, not offspring) indicating `Wild` for wild captures or
#'  `Captive` for captive-born individuals, `Qtr_born` is the date used to represent birth date of the parent/ID - which are converted to the starting date of the annual quarter,
#'  `Qtr_death`is the date used to represent death dates - which are converted to the starting date of the annual quarter, `x_birth` is the approximate age class in years
#'  in which the individual reproduces based on the difference between the start of the quarter of their birth and the start of the quarter of their offspring's birth,
#'  `sex_birth` is the sex of the offspring)
#' @export
#' @importFrom dplyr filter pull between distinct rowwise mutate ungroup select
#' @importFrom lubridate quarter time_length interval
#'
repro_history <- function(studbook, window = NULL) {
  first_captive <- filter(studbook, Type_birth == "Captive") %>% pull(Date_birth) %>% min()
  if (is.null(window)) {
    window <- c(first_captive, max(studbook$Date_birth))
  }
  births <- studbook %>%
    filter(between(Date_birth, window[1], window[2]) & Status %in% c("A", "D")) %>%
    mutate(Qtr_birth = quarter(Date_birth, type = "date_first")) %>%
    select(
      ID_birth  = ID,
      sex_birth = Sex,
      Qtr_birth,
      Sire,
      Dam
    ) %>%
    distinct() %>%
    pivot_longer(c(Sire, Dam), names_to = "parent", values_to = "ID") %>%
    filter(ID != 0) %>%
    select(ID, ID_birth, sex_birth, Qtr_birth) %>%
    arrange(ID, Qtr_birth, ID_birth)
  return(births)
}

#' Set up cohorts by birth year for population status
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param span integer representing the width of each birth-cohort in years
#' @return Tibble with `cohort` (numeric id), `cohort_start` (earliest birth year of cohort), `cohort_end` (last birth year of cohort),
#'  `born` (hypothetical birth year), `sex` (hypothetical sex), `x` (age in years), `x_n` (x + n for lifetable)
#' @export
#' @importFrom dplyr filter distinct mutate pull row_number rowwise select ungroup between arrange
#' @importFrom lubridate year today
#' @importFrom purrr pmap
#' @importFrom stringr str_glue
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#'
cohort_template <- function(studbook, span = 5) {
  studbook_dates <- studbook %>%
    filter(Sex != "U") %>%
    distinct(
      ID,
      Date_birth,
      Date_last,
      Sex
    ) %>%
    mutate(born = year(Date_birth),
           end  = year(Date_last), .keep = "unused") %>%
    mutate(age_end = as.integer(end) - as.integer(born))

  span     <- 5
  age_max  <- max(studbook_dates$age_end)
  year_min <- min(studbook_dates$born)
  year_max <- (year(today()) + 1)
  year     <- seq(year_min, year(today()), by = 1)
  start    <- seq(year_min, (year_max - 1), by = span)
  end      <- start + (span - 1)
  sex      <- unique(pull(studbook_dates, Sex))
  x        <- c(0:age_max)

  cohorts <- tibble(start, end) %>%
    mutate(cohort = row_number()) %>%
    mutate(born = pmap(list(start, end), \(x, y) seq(x, y, by = 1))) %>%
    unnest(born) %>%
    select(cohort,
           cohort_start = start,
           cohort_end   = end,
           born) %>%
    expand_grid(sex) %>%
    expand_grid(x) %>%
    rowwise() %>%
    mutate(x_n = x + 1) %>%
    ungroup() %>%
    filter(born <= year(today()) & (born + x) <= year(today())) %>%
    arrange(cohort, cohort_start, born, sex, x) %>%
    mutate(year = born + x) %>%
    select(
      cohort,
      cohort_start,
      born,
      cohort_end,
      sex,
      year,
      x,
      x_n
    )

  return(cohorts)
}
#' Match birth cohorts to studbook IDs
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param span integer representing the width of each birth-cohort in years
#' @return Tibble with each studbook ID matched to a birth cohort
#' @export
#' @importFrom dplyr filter distinct mutate pull row_number rowwise select ungroup between arrange
#' @importFrom lubridate year today
#' @importFrom purrr pmap
#' @importFrom stringr str_glue
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#'
cohort_studbook <- function(studbook, span = 5) {
  studbook_dates <- studbook %>%
    filter(Sex != "U") %>%
    distinct(
      ID,
      Date_birth,
      Date_last,
      Sex
    ) %>%
    mutate(born = year(Date_birth),
           end  = year(Date_last), .keep = "unused") %>%
    select(ID, sex = Sex, born, end)

  result   <- cohort_template(studbook, span)  %>%
    right_join(studbook_dates, by = join_by(born, sex)) %>%
    filter(year <= end) %>%
    arrange(cohort, cohort_start, born, ID, year, x) %>%
    select(
      cohort,
      cohort_start,
      born,
      cohort_end,
      ID,
      sex,
      year,
      x,
      x_n
    )
}

#' Count births per individual per year
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param span integer representing the width of each birth-cohort in years
#' @return Tibble with birth counts per studbook ID per year
#' @export
#' @importFrom dplyr filter distinct mutate pull row_number rowwise select ungroup between arrange
#' @importFrom lubridate year today
#' @importFrom purrr pmap
#' @importFrom stringr str_glue
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#'
births_annual <- function(studbook, span = 5) {
  cohort_studbook <- cohort_studbook(studbook, span)
  births <- studbook  %>%
    distinct(
      ID,
      Date_birth,
      Sire,
      Dam
    ) %>%
    mutate(year     = year(Date_birth),
           birth_id = ID,
           .keep = "unused") %>%
    pivot_longer(
      c(Sire, Dam),
      names_to  = "sex",
      values_to = "ID"
    ) %>%
    filter(ID != 0) %>%
    mutate(sex = case_match(sex, "Dam" ~ "F", "Sire" ~ "M")) %>%
    distinct() %>%
    group_by(year, sex, ID) %>%
    reframe(births = n()) %>%
    ungroup() %>%
    right_join(cohort_studbook, by = join_by(sex, ID, year)) %>%
    arrange(cohort, cohort_start, born, year, x) %>%
    select(
      cohort,
      cohort_start,
      born,
      cohort_end,
      ID,
      sex,
      year,
      x,
      x_n,
      births
    ) %>%
    mutate(births = replace_na(births, 0))
  return(births)
}

#' Count individuals by age and cohort
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param span integer representing the width of each birth-cohort in years
#' @param annual logical indicating whether to sum counts from each age class by year (or to sum across multi-year birth cohorts)
#' @param by_age logical indicating whether to count by age class or across all ages
#' @return Tibble with population counts
#' @export
#' @importFrom dplyr filter distinct mutate pull row_number rowwise select ungroup between arrange
#' @importFrom lubridate year today
#' @importFrom purrr pmap
#' @importFrom stringr str_glue
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#'
census_pop <- function(studbook, span = 5, annual = FALSE, by_age = TRUE) {
  cohorts        <- cohort_template(studbook, span)
  births         <- births_annual(studbook, span)
  studbook_dates <- studbook %>%
    filter(Sex != "U") %>%
    distinct(
      ID,
      Date_birth,
      Date_last,
      Sex
    ) %>%
    mutate(born = year(Date_birth),
           end  = year(Date_last), .keep = "unused")

  census <- cohorts %>%
    distinct(
      cohort,
      cohort_start,
      cohort_end,
      born,
      sex
    ) %>%
    right_join(studbook_dates, by = join_by(born, sex == Sex)) %>%
    mutate(year = pmap(list(born, end), \(x, y) seq(x, y, by = 1))) %>%
    unnest(year) %>%
    mutate(x = year - born) %>%
    left_join(births, by = join_by(cohort, cohort_start, born, cohort_end, sex, ID, year, x)) %>%
    group_by(born,
             sex,
             year,
             x) %>%
    summarize(pop = n(), births = sum(births)) %>%
    right_join(cohorts, by = join_by(sex, x, born, year)) %>%
    arrange(born, year, x, sex) %>%
    rowwise() %>%
    mutate(year = replace_na(year, born + x),
           across(c(pop, births), ~replace_na(., 0))) %>%
    ungroup() %>%
    mutate(deaths = if_else(x < max(x),
                            pop - lead(pop),
                            pop),
           .by = c(born, sex)) %>%
    pivot_wider(
      names_from   = "sex",
      names_sep    = "_",
      values_from  = c("pop", "births", "deaths")
    ) %>%
    rowwise() %>%
    mutate(pop    = sum(pop_M, pop_F),
           deaths = sum(deaths_M, deaths_F),
           births = sum(births_M, births_F))

  if (isTRUE(annual) && isTRUE(by_age)) {
    result <- census %>%
      select(
        born,
        x,
        x_n,
        year,
        pop_M,
        births_M,
        deaths_M,
        pop_F,
        births_F,
        deaths_F,
        pop,
        births,
        deaths
      )
  } else if (isFALSE(annual) && isTRUE(by_age)) {
    result <- census %>%
      group_by(cohort,
               cohort_start,
               cohort_end,
               x,
               x_n) %>%
      summarize(pop_M    = sum(pop_M),
                births_M = sum(births_M),
                deaths_M = sum(deaths_M),
                pop_F    = sum(pop_F),
                births_F = sum(births_F),
                deaths_F = sum(deaths_F),
                pop      = sum(pop),
                births   = sum(births),
                deaths   = sum(deaths))
  } else if (isTRUE(annual) && isFALSE(by_age)) {
    result <- census %>%
      group_by(cohort,
               cohort_start,
               cohort_end,
               year) %>%
      summarize(pop_M    = sum(pop_M),
                births_M = sum(births_M),
                deaths_M = sum(deaths_M),
                pop_F    = sum(pop_F),
                births_F = sum(births_F),
                deaths_F = sum(deaths_F),
                pop      = sum(pop),
                births   = sum(births),
                deaths   = sum(deaths))
  } else if (isFALSE(annual) && isFALSE(by_age)) {
    result <- census %>%
      group_by(year) %>%
      summarize(pop_M    = sum(pop_M),
                births_M = sum(births_M),
                deaths_M = sum(deaths_M),
                pop_F    = sum(pop_F),
                births_F = sum(births_F),
                deaths_F = sum(deaths_F),
                pop      = sum(pop),
                births   = sum(births),
                deaths   = sum(deaths))
  } else {
    result <- census
  }
  return(result)
}

#' Set up cohorts by birth year for population status
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param span integer representing the width of each birth-cohort in years
#' @return List of tibbles to organize data into birth cohorts
#' @export
#' @importFrom dplyr filter distinct mutate pull row_number rowwise select ungroup between arrange
#' @importFrom lubridate year today
#' @importFrom purrr pmap
#' @importFrom stringr str_glue
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#'
cohorts_set <- function(studbook, span = 5) {
  studbook_dates <- studbook %>%
    filter(Sex != "U") %>%
    distinct(
      ID,
      Date_birth,
      Date_last,
      Sex
    ) %>%
    mutate(born = year(Date_birth),
           end  = year(Date_last), .keep = "unused")

  age_max <- studbook_dates %>%
    mutate(age = as.integer(end) - as.integer(born)) %>%
    pull(age) %>%
    unique() %>%
    max()

  year_min <- pull(studbook_dates, born) %>% min()

  year_max <- (year(today()) + 1)
  year     <- seq(year_min, year(today()), by = 1)
  start    <- seq(year_min, (year_max - 1), by = span)
  end      <- start + (span - 1)

  cohorts <- tibble(start = start) %>%
    mutate(cohort = row_number()) %>%
    mutate(born   = pmap(list(start), \(x) seq(x, sum(x, (span - 1)), by = 1))) %>%
    unnest(born) %>%
    select(cohort, born)

  ages  <- expand_grid(year, cohorts) %>%
    arrange(born, year) %>%
    rowwise() %>%
    mutate(age = year - born) %>%
    ungroup() %>%
    filter(between(age, 0, age_max))

  labs <- tibble(cohort_start = start, cohort_end = end) %>%
    mutate(cohort       = row_number(),
           cohort_range = as.character(str_glue("{cohort_start} - {cohort_end}"))) %>%
    select(cohort, cohort_start, cohort_end, cohort_range)

  ids <- left_join(studbook_dates, cohorts, by = "born") %>%
    mutate(year = pmap(list(born, end), \(x, y) seq(x, y, by = 1))) %>%
    unnest(year) %>%
    select(cohort,
           born,
           ID,
           Sex,
           year) %>%
    arrange(cohort, year) %>%
    rowwise() %>%
    mutate(age = year - born) %>%
    ungroup()

  out <- list(cohorts = cohorts, ages = ages, labs = labs, ids = ids)
  return(out)
}

#' Count births for each individual across each of their age classes
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param span integer representing the width of each birth-cohort in years
#' @return Tibble with x rows per ID (with x being each age class integer) birth counts in columns
#' @export
#' @importFrom dplyr filter distinct mutate rename left_join join_by arrange group_by ungroup rowwise reframe right_join across
#' @importFrom tidyr pivot_longer pivot_wider replace_na
#' @importFrom tidyselect where starts_with
#'
cohort_births <- function(studbook, span = 5) {
  cohorts <- cohorts_set(studbook, span)

  births <- filter(studbook, Sex != "U") %>%
    distinct(
      ID,
      Date_birth,
      Sex,
      Sire,
      Dam
    ) %>%
    mutate(year = year(Date_birth), .keep = "unused") %>%
    rename(birth = ID, birth_sex = Sex) %>%
    pivot_longer(
      c(Sire, Dam),
      names_to  = "parent",
      values_to = "ID"
    ) %>%
    filter(ID != 0) %>%
    select(-parent) %>%
    left_join(cohorts$ids, by = join_by(ID, year)) %>%
    distinct(
      cohort,
      Sex,
      age,
      year,
      ID,
      birth,
      birth_sex
    ) %>%
    arrange(cohort, age, birth) %>%
    group_by(cohort, Sex, age, year, ID, birth_sex) %>%
    reframe(births = n()) %>%
    ungroup() %>%
    pivot_wider(
      names_from   = "birth_sex",
      names_prefix = "births_",
      values_from  = "births",
      values_fill  = 0
    ) %>%
    rowwise() %>%
    mutate(births = sum(births_M, births_F)) %>%
    ungroup() %>%
    right_join(cohorts$ids, by = join_by(cohort, ID, Sex, age, year)) %>%
    mutate(across(starts_with("births"), ~replace_na(., 0))) %>%
    arrange(cohort, born, age)

  return(births)
}

#' Caclulate basic vital stats to generate population projections and life tables
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param span integer representing the width of each birth-cohort in years
#' @return Tibble with one row per birth cohort and a tibble-column called `data` which contains
#' vital stats to compute life tabls and projections, where `x` = age class in years since birth,
#' `Sx` = census count, `Dx` = deaths as raw count, `births` = births as raw count,
#' `bx` = per-capita birthrate (`births/Sx`)
#' @export
#' @importFrom dplyr distinct group_by summarize ungroup right_join join_by rowwise across mutate rename left_join relocate if_else lead n pull
#' @importFrom tidyr expand_grid replace_na pivot_wider nest
#' @importFrom tidyselect starts_with
#'
vitals <- function(studbook, span = 5) {

  cohorts <- cohorts_set(studbook, span)

  age_max <- pull(cohorts$ages, age) %>% max()

  result <- cohort_births(studbook, span) %>%
    distinct(
      cohort,
      Sex,
      age,
      ID,
      births
    ) %>%
    group_by(cohort, Sex, age) %>%
    summarize(
      births = sum(births),
      Sx     = n()
    ) %>%
    ungroup() %>%
    right_join(expand_grid(distinct(cohorts$ages, cohort, age), Sex = c("M", "F")),
               by = join_by(cohort, age, Sex)) %>%
    arrange(Sex, cohort, age) %>%
    mutate(across(c(births, Sx), ~replace_na(., 0))) %>%
    mutate(Dx = if_else(
      age < age_max,
      Sx - lead(Sx),
      Sx), .by = c(cohort, Sex)) %>%
    mutate(Dx = replace_na(Dx, 0)) %>%
    rowwise() %>%
    mutate(bx = if_else(Sx > 0, births/Sx, 0)) %>%
    ungroup()  %>%
    pivot_wider(
      names_from   = "Sex",
      names_sep    = "",
      values_from  = c("Sx", "Dx", "bx", "births"),
      values_fill  = 0
    ) %>%
    rowwise() %>%
    mutate(Sx     = sum(SxM, SxF),
           Dx     = sum(DxM, DxF),
           births = sum(birthsM, birthsF),
           bx     = if_else(Sx > 0, births/Sx, 0)) %>%
    ungroup() %>%
    rename(x = age) %>%
    left_join(cohorts$labs, by = "cohort") %>%
    relocate(starts_with("cohort_"), .after = cohort) %>%
    nest(!starts_with("cohort"), .by = c(starts_with("cohort")))

  return(result)
}

#' Use `demogR` to create a list of life tables for each cohort
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param span integer representing the width of each birth-cohort in years
#' @return Tibble with one row per birth cohort and tibble-columns `lt` (tibble generated by `demogR::life.table`),
#' `life` (life table results joined to basic vital stats from `data` column produced by `vitals`),
#' as well as a nested list of vectors,  `les`, for computing leslie matrices.
#' @export
#' @importFrom demogR life.table
#' @importFrom dplyr mutate left_join summarize across
#' @importFrom purrr map map2
#' @importFrom tidyr hoist replace_na
#' @importFrom tidyselect where
#'
cohort_lifetab <- function(studbook, span = 5) {
  cohorts <- cohorts_set(studbook, span)
  df      <- vitals(studbook, span)   %>%
    filter((year(today()) - cohort_start) > 1)

  lifetab <- df %>%
    mutate(lt = map(data, \(i) life.table(
           x       = i$x,
           nDx     = i$Dx,
           nKx     = i$Sx,
           type    = "kf",
           iwidth  = 1,
           width12 = c(1,1)
         ))
  ) %>%
    mutate(
      lt = map(lt, \(x) mutate(x, across(where(is.numeric), ~ replace_na(., 0))))
    ) %>%
    mutate(
      life = map2(data, lt, \(i, j) left_join(i, j, by = "x"))
    ) %>%
    mutate(
      les = map(life, \(i) summarize(
        i,
        x   = list(x),
        nLx = list(nLx),
        lx  = list(lx),
        mx  = list(bx)
      ))
    ) %>%
    hoist(les,
          x   = list("x"),
          nLx = list("nLx"),
          lx  = list("lx"),
          mx  = list("mx"), .remove = FALSE) %>%
    hoist(
      life,
      Sx = list("Sx"),
      .remove = FALSE
    )
  return(lifetab)
}

#' Nest life table data into vectors listed by birth cohort
#'
#' @param lifetab tibble generated using the function `cohort_lifetab`
#' @return nested list with each birth cohort at the top level (name represents the cohort's minimum birth year),
#' and each of those lists containing `x` (age class), `Sx` (population count),
#' `nLx` (person-years lived in the interval `x` to `x+n`),
#' `lx` (cumulative survivorship), `mx` (per-capita birth rate)
#' @export
#' @importFrom dplyr pull
#' @importFrom purrr map set_names map_depth list_flatten
#'
#'
cohort_lifelist <- function(lifetab) {
  cohorts <- pull(lifetab, cohort_start)
  result  <- map(seq(1:nrow(lifetab)), \(i) list(
    x   = lifetab$x[[i]],
    Sx  = lifetab$Sx[[i]],
    nLx = lifetab$nLx[[i]],
    lx  = lifetab$lx[[i]],
    mx  = lifetab$mx[[i]]
  )) %>%
    set_names(cohorts) %>%
    map_depth(., 1, \(x) list_flatten(x, name_spec = "outer"))
  return(result)
}

#' Base function for `make_leslie` without `safely`
#'
#' @param cohort list item representing one birth cohort
#' @keywords internal
#' @noRd
#'
#' @importFrom demogR leslie.matrix
#'
base_leslie <- function(cohort) {
  age    <- cohort$x
  stages <- c(seq(from = min(age), to = max(age)))
  lx     <- cohort$nLx
  mx     <- cohort$mx
  matrix(
    leslie.matrix(
      lx           = lx,
      mx           = mx,
      peryear      = 1,
      infant.class = FALSE
    ),
    nrow = length(age),
    ncol = length(age),
    dimnames = list(stages, stages)
  )
}

#' Internal wrapper to re-organize and simplify results from `safely(base_leslie)`
#'
#' @param l list result of `safely(base_leslie)`
#' @keywords internal
#' @noRd
#'
#' @importFrom purrr map keep list_flatten
#'
fix_leslie <- function(l) {

  error_msg <- function(x) x[["error"]]$message
  errors    <- map(l, \(x) error_msg(x)) %>% keep(\(x) !is.null(x))

  result_A <- function(x) x[["result"]]
  mats <- map(l, \(x) result_A(x)) %>% keep(\(x) !is.null(x))

  leslies <- list(errors, mats) %>%
    list_flatten(name_spec = "inner")

  return(leslies)
}

#' Create a leslie matrix for each birth cohort using `demogR::leslie.matrix`
#'
#' @param stubook tibble organized and formatted originally using `read_studbook`
#' @param span integer representing the width of each birth-cohort in years
#' @return nested list with each birth cohort at the top level (name represents the cohort's minimum birth year),
#' and each of those lists containing a leslie matrix, or an error message if data were insufficient for computation
#' @export
#' @importFrom purrr safely imap
#'
cohort_leslie <- function(studbook, span = 5) {
  safe_leslie <- safely(base_leslie)
  result    <- cohort_lifetab(studbook, span) %>%
    cohort_lifelist() %>%
    imap(\(x, idx) safe_leslie(x)) %>%
    fix_leslie()
  return(result)
}

