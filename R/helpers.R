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

#' Generate a list with descriptions for values calculated by demographic functions
#'
#' @return nested list to annotate visuals
#' @export
#'
#'
demog_variables <- function() {
  list(
    N0          = list(title = "N Births", descr = "Total number of individuals at age 0 (i.e. total births in the cohort)."),
    N1          = list(title = "N at 1 year", descr = "Total number of individuals at age 1 (i.e. total surviving first year)."),
    Qx_1        = list(title = "Infant mortality rate", descr = "Infant mortality rate, computed as the ratio of observed deaths in the first age class to the initial cohort size."),
    R0          = list(title = "Net Reproductive Rate", descr = "Net Reproductive Rate, the sum of reproductive outputs (`Fx`) across all ages for the cohort."),
    MLE         = list(title = "Age at 50% survivorship", descr = "Maximum Likelihood Estimate of the age at 50% survivorship; the interpolated age where survival drops to 50%."),
    T           = list(title = "Mean generation time", descr = "Mean generation time (average age of reproduction), computed as `T = Tnum / R0` where `Tnum` is the age‐weighted reproductive output."),
    repro_first = list(title = "Min age of observed reproduction", descr = "The minimum age at which reproduction is observed, indicating the onset of reproductive activity."),
    repro_last  = list(title = "Max age of observed reproduction", descr = "The maximum age at which reproduction is observed in the cohort."),
    age_max     = list(title = "Max age of death", descr = "The highest age at which mortality is recorded, representing the maximum observed lifespan in the cohort."),
    lambda      = list(title = "Population growth rate", descr = "Finite rate of increase calculated as \u03BB `= R0^(1/T)`, representing the population growth rate."),
    r           = list(title = "Intrinsic population growth rate", descr = "Intrinsic rate of increase: the continuous growth rate of the population, computed as `r = log(\u03BB)`, where \u03BB is the finite rate of increase."),
    Age         = list(title = "Age in years", descr = "The age or age class (in years) for which all other demographic values are computed."),
    Births      = list(title = "N Births", descr = "The number of births recorded for that specific age class."),
    Deaths      = list(title = "N Deaths", descr = "The number of deaths occurring within that age interval (or age class)."),
    Nx          = list(title = "N alive at age start", descr = "The number of individuals alive at the beginning of the age class x."),
    Qx_risk     = list(title = "N alive at age start", descr = "The total number of individuals alive at the start of age x (`Nx`)"),
    Qx          = list(title = "Age-specific mortality rate", descr = "The age-specific mortality rate, calculated as the number of deaths in the age interval divided by the risk population (often `Nx`)."),
    Lx          = list(title = "Proportion surviving to age x (rel. to `N0`)", descr = "The proportion of the original cohort (`N0`) surviving to age x. It is a cumulative survival function."),
    Lx1         = list(title = "Proportion surviving to age x (rel. to `N1`)", descr = "The proportion of individuals surviving past the initial age interval (relative to `N1`), used to assess survival beyond infancy."),
    Px          = list(title = "Probability of survival to next age", descr = "The probability of surviving from age x to the next age class, computed as the ratio of survivors in the next age class to `Nx.`"),
    ex          = list(title = "Life expectancy at age x", descr = "Life expectancy at age x: the average number of additional years an individual is expected to live if they survive to age x. Typically calculated as `ex = Tx/Lx`."),
    Tx          = list(title = "Total future lifetime", descr = "Total future lifetime: the sum of survivors (`Lx`) from age x onward, representing the total person-years lived by the cohort beyond age x."),
    Mx_risk     = list(title = "N alive at age start", descr = "The total number of individuals capable of reproducing at age x (`Nx`)"),
    Mx          = list(title = "Age-specific fertility rate", descr = "The age-specific fertility rate (or production rate), representing the per-individual contribution of offspring at each age (often adjusted by 1/2 for biparental reproduction)."),
    Fx          = list(title = "Age-specific reproductive output", descr = "The age-specific reproductive output, computed as `Mx` multiplied by `Lx.` It represents the expected number of offspring produced by an individual at age x."),
    numT        = list(title = "Reproductive output weighted by age", descr = "The product of `Age` and `Fx`, which weights the reproductive output by age and is used to compute the mean generation time (`T`).")
  )
}

#' Generate formatted y-axis labels for demographic variables
#'
#' @return nested list of strings to use for labeling y-variables
#' @export
#'
#' @param variable Name of the column containing an age-specific variable to map onto the y-axis
#' options for variable: `N0`, `N1`, `R0`, `T`, `MLE`, `Repro_first`, `Repro_last`, `age_max`, `lambda`, `r`,
#' `Births`, `Deaths`, `Nx`, `Qx`, `Lx`, `Lx1`, `Px`, `ex`, `Tx`, `Mx`, `Fx`
#'
#' @importFrom stringr str_wrap str_replace_all
#' @importFrom purrr map_depth keep_at pluck
#'
demog_ylab <- function(variable) {
  var <- demog_variables() %>% keep_at(variable)
  y_short <- str_replace_all(variable, "lambda", "\u03BB")
  y_name  <- pluck(var, 1, "title")
  y_descr <- pluck(var, 1, "descr") %>%
    str_wrap(., width = 80) %>%
    str_replace_all(., "/n", "<br>")
  ylab <- paste0("<b>", y_short, "</b> (", y_name, ")") %>%
    str_wrap(., width = 40) %>%
    str_replace_all(., "/n", "<br>")
  y <- list(
    short     = y_short,
    lab       = ylab,
    descr     = y_descr
  )
  return(y)
}







