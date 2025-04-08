
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
      name_spec,
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
           name_spec,
           Sex,
           Year_birth,
           Age,
           Year) %>%
    left_join(births, by = join_by(ID, Year)) %>%
    mutate(Births = replace_na(Births, 0)) %>%
    distinct() %>%
    select(ID,
           name_spec,
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

#' Return number of generations for individuals
#'
#' @param pedigree A pedigree object from `pedtools::ped()`
#' @return Named vector of generation numbers
#' @export
#'
#' @importFrom pedtools generations
gen_numbers <- function(pedigree) {
  gens <- generations(pedigree, what = "indiv")
  if (length(gens) == 0) {
    warning("No generation numbers returned using 'indiv'. Trying 'depth'.")
    gens <- generations(pedigree, what = "depth")
  }
  return(gens)
}
#' Get generation numbers for living individuals
#'
#' @param pedigree A pedigree object from `pedtools::ped()`
#' @param studbook Studbook tibble
#' @return Named vector of generations for living IDs
#' @export
#'
#' @importFrom tibble tibble
gen_numbers_living <- function(pedigree, studbook) {
  gens <- gen_numbers(pedigree)
  gens[names(gens) %in% living(studbook)]
}

#' Annotate lambda growth values with human-readable hover text
#'
#' @param df A data frame with a column `lambda`
#' @return The same data frame with a new `hover_lambda` column
#' @export
#'
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom glue glue
annotate_lambda <- function(df) {
  df %>%
    mutate(hover_lambda = abs(round((lambda - 1) * 100, digits = 1))) %>%
    mutate(
      hover_lambda = if_else(
        lambda >= 1,
        as.character(str_glue("Population growing by {hover_lambda}%")),
        as.character(str_glue("Population declining by {hover_lambda}%"))
      )
    )
}

#' Create a character vector from a studbook dataframe to represent location data in a tooltip
#'
#' @param df A data frame with rowwise location data containing the columns `iconLoc`, `Loc`, `Institution`, and `State_Province`
#' @param name A string to use as the name of the new column
#' @return The same data frame with a new string column
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_glue
loc_tooltip <- function(df, name) {
  df %>%
    mutate(name = as.character(str_glue("{iconLoc}{Loc}: {Institution}, {State_Province}")))
}

#' Create a character vector from a studbook dataframe to represent subject's status in the breeding population as a tooltip
#'
#' @param df A data frame with rowwise location data containing the following columns:
#' `exclude`, `ID`, `Sex`, `age_last`, and `Institution_last`, `State_Province_last`, `iconLoc_last`, `Loc_last`, `Date_birth`, `Institution_birth`, `State_Province_birth`, `iconLoc_birth`, `Loc_birth`
#' @param name A string to use as the name of the new column
#' @return The same data frame with a new string column
#' @export
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_glue
subj_tooltip <- function(df, name) {
  df %>%
    mutate(
      name    = case_when(
        exclude  == "n" ~ as.character(str_glue("{ID} {Sex}: Included in breeding population<br>Currently age {age_last} at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
        exclude  == "age" ~ as.character(str_glue("{ID} {Sex}: Excluded from breeding population due to age<br>Currently age {age_last} at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
        exclude  == "behavior" ~ as.character(str_glue("{ID} {Sex}: Excluded from breeding population for behavioral reasons<br>Currently age {age_last} at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
        exclude  == "deceased" ~ as.character(str_glue("{ID} {Sex}: Deceased (age {age_last}) at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
        exclude  == "hypothetical" ~ as.character(str_glue("{ID} {Sex}: Hypothetical ID created to represent missing parent at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}"))
      ))
}

#' Create a character vector from a studbook dataframe to represent the connector between parents and offspring in a pedigree
#'
#' @param df A data frame with rowwise location data containing the following columns:
#' `dad`, `mom`, `iconLoc`, and `Loc`
#' @param name A string to use as the name of the new column
#' @return The same data frame with a new string column
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_glue
fam_tooltip <- function(df, name) {
  df %>% mutate(name = as.character(str_glue("Parents: Sire {dad} + Dam {mom} at {iconLoc}{Loc}")))
}

#' Create a character vector from a studbook dataframe that matches some attribute vector to an individual's sex and status
#'
#' @param df A data frame with rowwise location data containing the following columns:
#' `Sex`, `exclude`
#' @param name A string to use as the name of the new column
#' @param u A character vector to assign to the column where `Sex` is undetermined
#' @param m_i A character vector to assign to the column where `Sex` is male and individual is included in the breeding population and therefore alive
#' @param f_i A character vector to assign to the column where `Sex` is female and individual is included in the breeding population and therefore alive
#' @param m_e A character vector to assign to the column where `Sex` is male and individual is alive but excluded from the breeding population
#' @param f_e A character vector to assign to the column where `Sex` is female and individual is alive but excluded from the breeding population
#' @param m_d A character vector to assign to the column where `Sex` is male and individual is deceased
#' @param f_d A character vector to assign to the column where `Sex` is female and individual is deceased
#' @return The same data frame with a new character vector column
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_glue
ped_attribute <- function(df, name, u, m_i, f_i, m_e, f_e, m_d, f_d) {
  df %>% mutate(
    name = case_when(
      Sex %in% c("M", "m", "male", "Male")     & exclude == "n"                    ~ m_i,
      Sex %in% c("M", "m", "male", "Male")     & exclude == "deceased"             ~ m_d,
      Sex %in% c("M", "m", "male", "Male")     & exclude %in% c("behavior", "age") ~ m_e,
      Sex %in% c("F", "f", "female", "Female") & exclude == "n"                    ~ f_i,
      Sex %in% c("F", "f", "female", "Female") & exclude == "deceased"             ~ f_d,
      Sex %in% c("F", "f", "female", "Female") & exclude %in% c("behavior", "age") ~ f_e,
      Sex %in% c("U", "u", "undetermined", "Undetermined")                         ~u
    )
  )
}


#' Assign generation levels to individuals in a pedigree
#'
#' @param pedigree A pedigree object from `pedtools::ped()`
#' @return A tibble of individual IDs and generation levels
#' @export
#'
#' @importFrom dplyr across distinct if_else mutate
#' @importFrom pedtools founders leaves
#' @importFrom purrr set_names
#' @importFrom tibble enframe
pedigree_levels <- function(pedigree) {
  levels <- gen_numbers(pedigree) %>%
    as.list() %>%
    set_names(pedigree$ID) %>%
    enframe(name = "id", value = "level") %>%
    mutate(
      level = as.integer(level),
      level = if_else(id %in% founders(pedigree), level, level + 2),
      level = if_else(id %in% leaves(pedigree), level + 1, level)
    ) %>% distinct()
  return(levels)
}

#' Extract and organize tibble of birth events from pedigree object
#'
#' @param pedigree A pedigree object
#' @param studbook A studbook tibble
#' @return A tibble of unique mating pairs with location info and ids to create nodes for a network
#' @export
#'
#' @importFrom dplyr across distinct left_join join_by mutate row_number transmute
#' @importFrom pedtools nonfounders parents
#' @importFrom purrr map set_names
#' @importFrom tidyr unnest_wider
#' @importFrom tibble enframe
#' @importFrom tidyselect ends_with
pedigree_births <- function(pedigree, studbook) {
  nonfounders <- as.list(nonfounders(pedigree))
  birth_info <- studbook_short(studbook) %>%
    mutate(offspring = as.character(ID)) %>%
    select(
      offspring,
      name_spec,
      Sex,
      ends_with("_birth")
    ) %>% distinct()
  trios <- map(nonfounders, \(x) as.list(parents(pedigree, x))) %>%
    set_names(., nonfounders) %>%
    enframe(name = "offspring", value = "parent") %>%
    unnest_wider(parent, names_sep = "_") %>%
    arrange(parent_1, parent_2, offspring) %>%
    left_join(birth_info, by = "offspring") %>%
    rename(dad = parent_1, mom = parent_2) %>%
    rename_with(~str_remove_all(.x, "_birth"), ends_with("_birth")) %>%
    arrange(Date) %>%
    mutate(pair = consecutive_id(dad, mom)) %>%
    select(-Type) %>%
    mutate(sibs = max(pair) + consecutive_id(pair))
  return(trios)
}

