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
count_births <- function(studbook, span = 5) {
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

  result <- count_births(studbook, span) %>%
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
  df      <- vitals(studbook, span)

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
    )  %>%
    filter((year(today()) - cohort_start) > 1)
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
cohorts_leslie <- function(studbook, span = 5) {
  safe_leslie <- safely(base_leslie)
  result    <- cohort_lifetab(studbook, span) %>%
    list_life() %>%
    imap(\(x, idx) safe_leslie(x)) %>%
    fix_leslie()
  return(result)
}

