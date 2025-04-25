# coldefs.R
#'
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom tippy tippy
#' @importFrom htmlwidgets JS
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
#' @importFrom shiny icon
#' @importFrom htmltools div
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
#' @importFrom reactablefmtr color_scales
#' @importFrom scales label_number
#' @importFrom paletteer paletteer_dynamic
#' @importFrom tippy tippy
#'
n1 <- function(data) {
  colDef(
    align  = "left",
    header = tippy("N1", tooltip = "Total number of individuals at age 1 (i.e. total surviving first year)."),
    cell   = color_scales(data)
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
#' @importFrom reactablefmtr icon_assign
#' @importFrom scales label_number
#' @importFrom tippy tippy
#' @importFrom dplyr pull
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
#' @importFrom htmltools div
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
#' @importFrom reactablefmtr data_bars
#' @importFrom dplyr pull
#' @importFrom tippy tippy
#'
r0 <- function(data) {
  colDef(
    header = tippy("R0", tooltip = "Net Reproductive Rate, the sum of reproductive outputs (Fx) across all ages for the cohort."),
    cell = data_bars(
      data,
      fill_color    = "#EEEEEE",
      min_value     = min(pull(data, age_max)),
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


