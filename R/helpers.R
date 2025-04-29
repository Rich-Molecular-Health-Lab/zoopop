# helpers.R

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

#' Annotate lambda growth values with human-readable hover text
#'
#' @param df A data frame with a column `r`
#' @return The same data frame with a new `hover_lambda` column
#' @export
#'
#' @importFrom dplyr if_else mutate
#' @importFrom stringr str_glue
annotate_lambda <- function(df) {
  df %>%
    mutate(hover_lambda = abs(round(r * 100, digits = 1))) %>%
    mutate(
      hover_lambda = if_else(
        r > 0,
        as.character(str_glue("Population growing by {hover_lambda}%")),
        as.character(str_glue("Population declining by {hover_lambda}%"))
      )
    )
}

#' Pivot a life table wider for ease of plotting multiple traces
#'
#' @param df A data frame with demography stats for plotting
#' @param group The grouping variable(s) to plot as individual traces
#' @param x The variable(s) that will be plotted on the x axis
#' @param y The variable(s) that will be plotted on the y axis
#' @return Wider version with sexes in columns for plotting multiple traces
#' @export
#'
#' @importFrom tidyr pivot_wider

demog_wide <- function(df, group, x, y) {
    pivot_wider(
      df,
      id_cols     = x,
      names_from  = group,
      values_from = y
    )
}

#' Add a formatted figure caption to plotly plots
#'
#' @param caption Caption text to add
#' @param number Figure number to apply to caption (optional)
#' @return nested list to add to plotly layout function
#' @export
#'
caption_plotly <- function(caption, number) {
  cap_text <- paste0("Figure ", number, ". ", caption)
  caption    <- list(
    text       = cap_text,
    font       = list(size = 14),
    x          = 0,
    y          = -0.2,
    xref       = "paper",
    yref       = "paper",
    xanchor    = "left",
    yanchor    = "top",
    showarrow  = FALSE,
    automargin = TRUE
  )
  return(caption)
}
