col.pal <- c("#B24422FF", "#C44D76FF", "#4457A5FF", "#13315FFF", "#B1A1CCFF", "#59386CFF", "#447861FF", "#7CAF5CFF")

lifevars <- list(
  N0          = "Total number of individuals at age 0 (i.e. total births in the cohort).",
  N1          = "Total number of individuals at age 1 (i.e. total surviving first year).",
  Qx_1        = "Infant mortality rate, computed as the ratio of observed deaths in the first age class to the initial cohort size.",
  R0          = "Net Reproductive Rate, the sum of reproductive outputs (Fx) across all ages for the cohort.",
  T           = "Mean generation time (average age of reproduction), computed as T = Tnum / R0 where Tnum is the age‐weighted reproductive output.",
  MLE         = "Maximum Likelihood Estimate of the age at 50% survivorship; the interpolated age where survival drops to 50%.",
  repro_first = "The minimum age at which reproduction is observed, indicating the onset of reproductive activity.",
  repro_last  = "The maximum age at which reproduction is observed in the cohort.",
  age_max     = "The highest age at which mortality is recorded, representing the maximum observed lifespan in the cohort.",
  lambda      = "Finite rate of increase calculated as lambda = R0^(1/T), representing the population growth rate.",
  r           = "Intrinsic rate of increase: the continuous growth rate of the population, computed as r = log(λ), where λ is the finite rate of increase."
)

agevars <- list(
  Age       = "The age or age class (in years) for which all other life table values are computed.",
  Births    = "The number of births recorded for that specific age class.",
  Deaths    = "The number of deaths occurring within that age interval (or age class).",
  Nx        = "The number of individuals alive at the beginning of the age class x.",
  Qx_risk   = "The total number of individuals alive at the start of age x (Nx)",
  Qx        = "The age-specific mortality rate, calculated as the number of deaths in the age interval divided by the risk population (often Nx).",
  Lx        = "The proportion of the original cohort (N0) surviving to age x. It is a cumulative survival function.",
  Lx1       = "The proportion of individuals surviving past the initial age interval (relative to N1), used to assess survival beyond infancy.",
  Px        = "The probability of surviving from age x to the next age class, computed as the ratio of survivors in the next age class to Nx.",
  ex        = "Life expectancy at age x: the average number of additional years an individual is expected to live if they survive to age x. Typically calculated as ex = Tx/Lx.",
  Tx        = "Total future lifetime: the sum of survivors (Lx) from age x onward, representing the total person-years lived by the cohort beyond age x.",
  Mx_risk   = "The total number of individuals capable of reproducing at age x (Nx)",
  Mx        = "The age-specific fertility rate (or production rate), representing the per-individual contribution of offspring at each age (often adjusted by 1/2 for biparental reproduction).",
  Fx        = "The age-specific reproductive output, computed as Mx multiplied by Lx. It represents the expected number of offspring produced by an individual at age x.",
  numT      = "The product of Age and Fx, which weights the reproductive output by age and is used to compute the mean generation time (T)."
)

#'
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom tippy tippy
#'
Cohort_years <- function() {
  colDef(
    header = tippy("Birth Cohort", tooltip = "Stats grouped based on birth-year spans"),
    style = JS("function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        if (!firstSorted || firstSorted.id === 'Cohort_years') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['Cohort_years'] === prevRow['Cohort_years']) {
            return { visibility: 'hidden' }
          }
        }
      }")
  )
}

#'
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom htmlwidgets JS
#'
rows_bySex <- function() {
  JS("
    function(rowInfo, state) {
      if (!rowInfo) return {};
      if (rowInfo.values['Sex'] === 'Females') {
      return { borderLeft: '4px solid #C44D76FF', boxShadow: 'inset 0 -2px 0 #C44D7633'  }
    }  else if (rowInfo.values['Sex'] === 'Males') {
      return { borderLeft: '4px solid #4457A5FF', boxShadow: 'inset 0 -2px 0 #4457A533' }
      } else if (rowInfo.values['Sex'] === 'Overall') {
      return { borderLeft: '4px solid transparent' , boxShadow: 'inset 0 -1.5px 0 #444444FF' }
      }
    }
  ")
}

#'
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom shiny icon
#' @importFrom tippy tippy
#'
intrins_rate <- function() {
  colDef(
    align  = "left",
    header = tippy("r", tooltip = "Intrinsic rate of increase: the continuous growth rate of the population, computed as r = log(\u03BB), where \u03BB is the finite rate of increase."),
    cell   = function(value, index) {
      icon <- if (value > 0) icon("up-long", style = "color: #7CAF5CFF") else icon("down-long", style = "color: #A40000FF")
      rate <- value * 100
      div(icon, " ", rate, "%")
    }
  )
}

#'
#' @param data Dataframe passed to reactable table function
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr icon_assign
#' @importFrom tippy tippy
#'
repro_first <- function(data) {
  colDef(
    align  = "left",
    header = tippy("First Birth", tooltip = "The minimum age at which reproduction is observed, indicating the onset of reproductive activity."),
    cell   = icon_assign(data,
                         fill_color  = "#13315FFF",
                         icon_size   = 9,
                         show_values = "right")
  )
}

#'
#' @param data Dataframe passed to reactable table function
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom tippy tippy
#'
age_ranges <- function(data) {
  colDef(
    align  = "left",
    header = tippy("Reproductive Years", tooltip = "The minimum age at which reproduction is observed,
                   indicating the onset of reproductive activity to the maximum age at which reproduction is observed in the cohort. Bottom number is the highest age at which mortality is recorded, representing the maximum observed lifespan in the cohort."),
    cell   = function(index, value) {
      min_repro <- value
      max_repro <- data$repro_last[index]
      max_age   <- data$age_max[index]
      div(
        div(
          style = "font-size: 0.85em; display: flex; justify-content: flex-start; align-items: flex-start",
          "Age ", min_repro, " - ", max_repro),
        div(
          style = "font-size: 0.75em; font-style: italic; display: flex; justify-content: flex-start; align-items: flex-end",
          "Max life: ", max_age)
      )

    }
  )
}


#'
#' @param data Dataframe passed to reactable table function
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr gauge_chart
#' @importFrom scales label_number
#' @importFrom tippy tippy
#'
gen_T <- function(data) {
  colDef(
    align  = "left",
    header = tippy("T", tooltip = "Mean generation time (average age of reproduction), computed as T = Tnum / R0 where Tnum is the age‐weighted reproductive output."),
    cell   = icon_assign(data,
                         fill_color  = "#13315FFF",
                         icon_size   = 9,
                         number_fmt  = label_number(accuracy = 0.1),
                         seq_by      = 2,
                         show_values = "right")
  )
}

#'
#' @param data Dataframe passed to reactable table function
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr gauge_chart
#' @importFrom scales label_number
#' @importFrom tippy tippy
#'
MLE <- function(data) {
  min <- min(pull(data, MLE))
  colDef(
    align  = "center",
    header = tippy("MLE", tooltip = "Maximum Likelihood Estimate of the age at 50% survivorship; the interpolated age where survival drops to 50%."),
    cell = icon_assign(
      data,
      fill_color  = "#13315FFF",
      icon_size   = 9,
      number_fmt  = label_number(accuracy = 0.1),
      seq_by      = 4,
      show_values = "right")
  )
}

#'
#' @param data Dataframe passed to reactable table function
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom reactablefmtr gauge_chart
#' @importFrom scales label_number
#' @importFrom tippy tippy
#'
r0 <- function(data) {
  min <- min(pull(data, age_max))
  colDef(
    header = tippy("R0", tooltip = "Net Reproductive Rate, the sum of reproductive outputs (Fx) across all ages for the cohort."),
    cell = data_bars(
      data,
      fill_color    = "#EEEEEE",
      min_value     = min,
      text_position = "outside-end",
      icon          = "ellipsis-vertical",
      icon_color    = "#13315FFF",
      icon_size     = 15,
      bar_height    = 13,
      text_color    = "#13315FFF",
      round_edges   = FALSE,
      text_size     = 12
    )
  )
}

