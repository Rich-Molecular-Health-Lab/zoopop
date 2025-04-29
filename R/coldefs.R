# coldefs.R
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
    sticky   = "left",
    maxWidth = 110,
    header = tippy("Cohort", tooltip = "Stats grouped based on birth-year spans"),
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
#' @rdname react_cols
#' @export
#'
#' @importFrom reactable colDef
#' @importFrom tippy tippy
#' @importFrom htmlwidgets JS
#'
Sex <- function() {
  colDef(
    maxWidth = 80,
    header   = tippy("Sex", tooltip = "Stats grouped by males and females within a birth-year cohort and then calculated. 'Overall' values were calculated without grouping by sex within cohorts."),
    style =     JS("
    function(rowInfo) {
      if (rowInfo.values['Sex'] === 'Females') {
      return { color: '#C44D76FF', fontFamily: 'Veranda, sans-serif', fontVariant: 'small-caps'}
    }  else if (rowInfo.values['Sex'] === 'Males') {
      return { color: '#4457A5FF' , fontFamily: 'Veranda, sans-serif', fontVariant: 'small-caps'}
      } else if (rowInfo.values['Sex'] === 'Overall') {
      return { color: '#59386CFF', boxShadow: 'inset 0 -1.5px 0 #444444FF' , fontWeight: 'bold' , fontFamily: 'Veranda, sans-serif', fontVariant: 'small-caps'}
      } else if (rowInfo.values['Sex'] === 'Summary')  {
      return { color: '#000000', background: '#eeeeee', fontWeight: 'bold', fontFamily: 'Veranda, sans-serif', fontVariant: 'small-caps'}
      }
    }
  ")
  )
}

#'
#' @param data Dataframe passed to reactable table function
#' @param seq_by A numerical input that determines what number each icon represents.
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactablefmtr icon_assign
#' @importFrom scales label_number
#'
icon_cells <- function(data, seq_by) {
  icon_assign(data,
              icon         = "stop",
              fill_color   = "#23313CFF",
              fill_opacity = 0.7,
              icon_size    = 16,
              number_fmt   = label_number(accuracy = 0.1),
              seq_by       = seq_by,
              show_values  = "right"
  )
}

#'
#' @param data Dataframe passed to reactable table function
#'
#' @rdname react_cols
#' @export
#'
#' @importFrom reactablefmtr react_sparkline
#' @importFrom dataui dui_for_reactable
#'
sparkline_cells <- function(data) {
  react_sparkline(
    data,
    height           = 40,
    show_area        = TRUE,
    line_width       = 2,
    line_color_ref   = "sex_col",
    tooltip_type     = 2,
    decimals         = 2
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
    maxWidth = 90,
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
#' @importFrom paletteer paletteer_d
#' @importFrom tippy tippy
#'
n1 <- function(data) {
  colDef(
    align    = "center",
    maxWidth = 50,
    header   = tippy("N1", tooltip = "Total number of individuals at age 1 (i.e. total surviving first year)."),
    style    = color_scales(data, colors = paletteer_d("MoMAColors::Ernst", type = "continuous"))
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
nx <- function(data) {
  colDef(
    align    = "center",
    header   = tippy("Nx", tooltip = "Total number of individuals at age x."),
    cell     = sparkline_cells(data)
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
lx1 <- function(data) {
  colDef(
    align    = "center",
    header   = tippy("Lx1", tooltip = "The proportion of individuals surviving past the initial age interval (relative to N1), used to assess survival beyond infancy."),
    cell     = sparkline_cells(data)
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
px <- function(data) {
  colDef(
    align    = "center",
    header   = tippy("Px", tooltip = "The probability of surviving from age x to the next age class, computed as the ratio of survivors in the next age class to Nx."),
    cell     = sparkline_cells(data)
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
qx <- function(data) {
  colDef(
    align    = "center",
    header   = tippy("Qx", tooltip = "The age-specific mortality rate, calculated as the number of deaths in the age interval divided by the risk population (often Nx)."),
    cell     = sparkline_cells(data)
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
  colDef(
    maxWidth = 110,
    align  = "left",
    header = tippy("MLE", tooltip = "Maximum Likelihood Estimate of the age at 50% survivorship; the interpolated age where survival drops to 50%."),
    cell = icon_cells(data = data, seq_by = 4)
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
tx <- function(data) {
  colDef(
    align    = "center",
    header   = tippy("Tx", tooltip = "Total future lifetime: the sum of survivors (Lx) from age x onward, representing the total person-years lived by the cohort beyond age x."),
    cell     = sparkline_cells(data)
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
ex <- function(data) {
  colDef(
    align    = "center",
    header   = tippy("ex", tooltip = "Life expectancy at age x: the average number of additional years an individual is expected to live if they survive to age x. Calculated as ex = Tx/Lx."),
    cell     = sparkline_cells(data)
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
    maxWidth = 110,
    align  = "left",
    header = tippy("Span", tooltip = "The minimum age at which reproduction is observed,
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
#' @importFrom reactablefmtr icon_assign
#' @importFrom scales label_number
#' @importFrom tippy tippy
#'
gen_T <- function(data) {
  colDef(
    align  = "left",
    maxWidth = 120,
    header = tippy("T", tooltip = "Mean generation time (average age of reproduction), computed as T = Tnum / R0 where Tnum is the age‐weighted reproductive output."),
    cell   = icon_cells(data = data, seq_by = 2)
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
mx <- function(data) {
  colDef(
    align    = "center",
    header   = tippy("Mx", tooltip = "The age-specific fertility rate (or production rate), representing the per-individual contribution of offspring at each age (often adjusted by 1/2 for biparental reproduction)."),
    cell     = sparkline_cells(data)
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
fx <- function(data) {
  colDef(
    align    = "center",
    header   = tippy("Fx", tooltip = "The age-specific reproductive output, computed as Mx multiplied by Lx. It represents the expected number of offspring produced by an individual at age x."),
    cell     = sparkline_cells(data)
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
    align = "left",
    cell = data_bars(
      data,
      text_position  = "outside-end",
      text_size      = 10,
      text_color     = "#5b5b5b",
      fill_opacity   = 0.7,
      number_fmt     = label_number(accuracy = 0.01),
      fill_color_ref = "sex_col",
      box_shadow     = TRUE,
      tooltip        = TRUE
    )
  )
}


