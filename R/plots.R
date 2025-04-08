#' Plot population trends using census data
#'
#' @param census_df A tibble with columns Date, Females, Males, Unidentified
#' @param colors A named list of colors for "f", "m", and "u" (optional)
#' @param title The title for the plot (optional)
#'
#' @return A plotly plot object
#' @export
#'
#' @importFrom plotly add_trace layout plot_ly
#'
plot_census <- function(census_df, colors = set_colors(), title = NULL) {
fill.col <- lighten_plotly_pal(colors, 26)
plot <- plot_ly(census_df,
                x          = ~Date,
                y          = ~Unidentified,
                name       = "Sex Unidentified",
                type       = "scatter",
                mode       = "lines",
                stackgroup = "one",
                hoveron    = "points+fills",
                opacity    = 0.5,
                line       = list(
                  color     = colors[["u"]],
                  shape     = "spline",
                  smoothing = 0.8,
                  width     = 1.5),
                fillcolor   = fill.col[["u"]]) %>%
  add_trace(y         = ~Females,
            name      = "Females",
            line      = list(
              color     = colors[["f"]],
              shape     = "spline",
              smoothing = 0.8,
              width     = 1.5),
            fillcolor = fill.col[["f"]]) %>%
  add_trace(y         = ~Males,
            name      = "Males",
            line      = list(
              color     = colors[["m"]],
              shape     = "spline",
              smoothing = 0.8,
              width     = 1.5),
            fillcolor = fill.col[["m"]]) %>%
  layout(title        = title,
         plot_bgcolor = "#ffffff",
         yaxis        = list(
           title    = "Individuals Alive",
           showline = TRUE,
           showgrid = FALSE
         ),
         xaxis         = list(
           title       = "Date",
           showline    = FALSE,
           showgrid    = TRUE,
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
#' @param life_table A tibble with lambda values and Sex, Cohort_label columns
#' @param title Plot title
#' @param palette A color palette in list format
#'
#' @return A plotly object
#' @export
#'
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
plot_lambda <- function(life_table, colors = NULL, title = NULL) {
  if (is.null(colors)) {
    colors <- set_colors()
  } else {
    colors <- colors
  }
  col.pal    <- set_plotly_pal(colors)
  fills      <- lighten_plotly_pal(col.pal, 33)
  static     <- lifeTab_static(life_table)
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
  plot <- plot_ly(
                  data        = filter(static, Sex != "Total"),
                  x           = ~Cohort_label,
                  y           = ~lambda,
                  color       = ~Sex,
                  type        = "scatter",
                  mode        = "lines+markers",
                  colors      = c(col.pal),
                  text        = ~hover_lambda,
                  connectgaps = TRUE,
                  line        = list(
                    shape     = "spline",
                    smoothing = 0.8,
                    width     = 1.5
                  ),
                  marker      = list(
                    size    = 6,
                    opacity = 0.7,
                    line    = list(width = 1)
                  )
  ) %>%
  plotly::layout(title        = title,
           plot_bgcolor = "#ffffff",
           shapes       = list(hline(1.0)),
           annotations  = annotation,
           yaxis        = list(
             title    = "Lambda (\u03BB)",
             showline = TRUE,
             showgrid = FALSE
           ),
           xaxis        = list(
             title    = "Birth Cohort",
             showline = FALSE,
             showgrid = TRUE
           )
    )
  return(plot)
}

