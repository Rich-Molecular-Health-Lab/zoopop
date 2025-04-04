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

