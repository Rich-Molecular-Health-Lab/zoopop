#' Plot population trends based on census data calculated from studbook
#'
#' @param studbook A data frame containing studbook metadata.
#' @param caption The caption text for the plot (optional)
#' @param number The optional number to add to the plot caption (e.g., Figure 1.)
#'
#' @return A plotly plot object
#' @export
#'
#' @importFrom dplyr rowwise mutate
#' @importFrom plotly add_trace layout plot_ly config
#'
plot_census <- function(studbook, caption = NULL, number = NULL) {
  colors    <- set_colors()
  fill.col <- lighten_palette(colors, "26")
  symbols  <- set_markers()
  census_df <- census(studbook, "years") %>%
    rowwise() %>%
    mutate(Total = sum(Males, Females, Unidentified))
popline <- function(col) {
  list(
    color     = colors[[col]],
    shape     = "spline",
    smoothing = 0.8,
    width     = 1.7
  )
}
popmark <- function(col) {
  list(
    color  = colors[[col]],
    symbol = symbols[[col]],
    size   = 4
  )
}
trace_census <- function(p, col, ...) {
  fillcolor <- fill.col[[col]]
  if (col == "t") {
    fill <- "tonexty"
  } else {
    fill <- "tozeroy"
  }
  if (col == "u") {
    y    <- census_df$Unidentified
    name <- "Unidentified"
  } else if (col == "f") {
    y    <- census_df$Females
    name <- "Females"
  } else if (col == "m") {
    y    <- census_df$Males
    name <- "Males"
  } else if (col == "t") {
    y    <- census_df$Total
    name <- "Total"
  }
  add_trace(p          = p,
            data       = census_df,
            y          = y,
            x          = ~Date,
            name       = name,
            fill       = fill,
            type       = "scatter",
            mode       = "markers+lines",
            marker     = popmark(col = col),
            hoveron    = "points+fills",
            opacity    = 0.5,
            line       = popline(col = col),
            fillcolor  = fillcolor,
            ...)
}
if (is.null(caption)) { caption <- "Population Census by Year" }
if (is.null(number)) { number <- "" }
title <- caption_plotly(caption = caption, number = number)
plot <- plot_ly(census_df,
                type       = "scatter",
                mode       = "lines",
                hoveron    = "points+fills") %>%
  trace_census("u") %>%
  trace_census("f") %>%
  trace_census("m") %>%
  trace_census("t") %>%
  config(displaylogo = FALSE) %>%
  plotly::layout(
    plot_bgcolor = "#ffffff",
    title        = title,
    yaxis        = list(
      title      = "Count",
      showgrid   = FALSE,
      showline   = TRUE,
      showlegend = FALSE,
      rangemode  = "tozero"),
    xaxis        = list(
       title       = "Year",
       nticks      = 8,
       showgrid    = TRUE,
       gridcolor   = "#2323231A",
       showlegend  = FALSE,
       rangemode   = "tozero",
       rangeslider = list(visible = TRUE)
      )
  )
return(plot)
}

#' Add a horizontal reference line to plotly layout
#'
#' @param y Numeric y-intercept for the line
#' @param color Line color as a hex string
#' @return A plotly-compatible line specification
#' @keywords internal
hline <- function(y = 0, color = "#444444") {
  list(
    type = "line",
    x0   = 0,
    x1   = 1,
    xref = "paper",
    y0   = y,
    y1   = y,
    line = list(color = color, width = 0.9, dash = "dot")
  )
}

#' Plot lambda growth rates by cohort
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param caption The caption text for the plot (optional)
#' @param number The optional number to add to the plot caption (e.g., Figure 1.)
#'
#' @return A plotly object
#' @export
#'
#' @importFrom plotly layout plot_ly add_trace
plot_lambda <- function(studbook, cohort_params = NULL, caption = NULL, number = NULL) {
  annotation <- list(
    x          = 0.9,
    xref       = "paper",
    y          = 1,
    yref       = "y",
    text       = "Stable if \u03BB = 1.0",
    showarrow  = TRUE,
    arrowhead  = 4,
    arrowcolor = "#444444",
    arrowsize  = 1,
    arrowwidth = 0.7,
    ax         = 10,
    ay         = 40,
    font       = list(
      color = "#444444",
      size  = 12,
      style = "italic"
    )
  )
  data <- lifetime_tab(studbook      = studbook,
                       cohort_params = cohort_params) %>%
    demog_wide("Sex",
              c("Cohort_period",
                "Cohort_years"),
              c("lambda", "hover_lambda")
              )
  trace_lambda <- function(p, col, ...) {
    colors    <- set_colors()
    fill.col  <- lighten_palette(colors, "26")
    symbols   <- set_markers()
    fillcolor <- fill.col[[paste0(col)]]
    color     <- colors[[paste0(col)]]
    symbol    <- symbols[[paste0(col)]]
    line      <- list(
      shape     = "spline",
      smoothing = 0.8,
      width     = 1.5,
      color     = color
    )
    marker    <- list(
      size    = 7,
      opacity = 0.7,
      line    = list(width = 1, color = color),
      color   = fill.col,
      symbol  = symbol
    )
    if (col == "f") {
      y    <- data$lambda_F
      text <- data$hover_lambda_F
      name <- "Females"
    } else if (col == "m") {
      y    <- data$lambda_M
      text <- data$hover_lambda_M
      name <- "Males"
    } else if (col == "t") {
      y    <- data$lambda_Total
      text <- data$hover_lambda_Total
      name <- "Overall"
    }
    add_trace(p           = p,
              data        = data,
              y           = y,
              x           = ~Cohort_years,
              name        = name,
              type        = "scatter",
              mode        = "lines+markers",
              marker      = marker,
              line        = line,
              text        = text,
              connectgaps = TRUE,
              ...)

  }

  if (is.null(caption)) { caption <- "Population Lambda by Birth Year Cohort" }
  if (is.null(number)) { number <- "" }
  title <- caption_plotly(caption = caption, number = number)
  plot <- plot_ly(
                  data        = data,
                  type        = "scatter",
                  mode        = "lines+markers"
                  ) %>%
    trace_lambda("t") %>%
    trace_lambda("f") %>%
    trace_lambda("m") %>%
    config(displaylogo = FALSE) %>%
  plotly::layout(
           title        = title,
           plot_bgcolor = "#ffffff",
           shapes       = list(hline(1.0)),
           annotations  = annotation,
           yaxis        = list(
             title     = "Lambda (\u03BB)",
             rangemode  = "normal",
             showgrid   = FALSE,
             showline   = TRUE,
             showlegend = FALSE
             ),
           xaxis        = list(
             title      = "Birth Year Cohort",
             showgrid   = TRUE,
             gridcolor  = "#2323231A",
             showline   = TRUE,
             showlegend = FALSE
             )
    )
  return(plot)
}

