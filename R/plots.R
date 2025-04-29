#'
#'
#' Plot population trends based on census data calculated from studbook
#'
#' @param studbook A data frame containing studbook metadata.
#'
#' @return A plotly plot object
#' @export
#'
#' @importFrom dplyr rowwise mutate
#' @importFrom plotly add_trace layout plot_ly config
#'
plot_census <- function(studbook) {
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
#' Plot lifetime demographic variables by sex and birth year
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param variable Name of the column containing an age-specific variable to map onto the y-axis
#' options for variable: `N0`, `N1`, `R0`, `T`, `MLE`, `Repro_first`, `Repro_last`, `age_max`, `lambda`, `r`
#'
#' @return A plotly object
#' @export
#'
#' @importFrom dplyr filter rename distinct mutate select across pull
#' @importFrom forcats fct_recode fct_relevel
#' @importFrom plotly layout plot_ly add_trace
#' @importFrom stringr str_extract_all
#' @importFrom tidyselect where
#' @importFrom purrr map_depth
#'
#'
plot_demog <- function(studbook, cohort_params = NULL, variable) {
  colors <- set_colors()
  col <- list(Males   = colors$m,
              Females = colors$f,
              Overall = colors$t) %>%
    unlist()
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  total <- demog_ungrouped(studbook)  %>%
    filter(Sex == "Total") %>%
    rename(y_var = variable) %>%
    distinct()
  data <- cohort_demog(studbook = studbook, cohort_params = params)  %>%
    mutate(Sex = fct_recode(Sex,
                            Overall = "Total",
                            Males   = "M",
                            Females = "F")) %>%
    mutate(Sex = fct_relevel(Sex,
                             "Males",
                             "Females",
                             "Overall")) %>%
    rename(y_var = variable) %>%
    mutate(Cohort_start = as.integer(str_extract_all(Cohort_years, "^\\d{4}"))) %>%
    select(-c(
      Age          ,
      Births       ,
      Deaths       ,
      Nx           ,
      Qx           ,
      Lx1          ,
      Px           ,
      ex           ,
      Tx           ,
      Mx           ,
      Fx           ,
      numT
    )) %>%
    distinct() %>%
    mutate(across(where(is.numeric), ~round(., digits = 2)))

  if (variable == "N1") {
    overall_val <- mean(data$y_var)
  } else {
    overall_val <- pull(total, y_var) %>% unique()
  }


  ylab <- demog_ylab(variable)
  hover_template <-  paste0(ylab$short, " = %{y}<br><i>Birth Year %{x}</i>")

  plot <- plot_ly() %>%
    add_trace(
      data          = filter(data, Sex == "Overall"),
      x             = ~Cohort_years,
      y             = ~y_var,
      split         = ~Sex,
      name          = ~Sex,
      hovertemplate = hover_template,
      color         = I(colors$t),
      type          = "scatter",
      mode          = "lines+markers",
      line          = list(shape       = "spline",
                           smoothing   = 1.3,
                           width       = 3),
      marker        = list(symbol      = "x",
                           size        = 7),
      connectgaps   = TRUE
    ) %>%
    add_trace(
      data          = filter(data, Sex == "Males"),
      x             = ~Cohort_years,
      y             = ~y_var,
      split         = ~Sex,
      name          = ~Sex,
      hovertemplate = hover_template,
      color         = I(colors$m),
      opacity       = 0.8,
      type          = "scatter",
      mode          = "lines+markers",
      line          = list(shape       = "spline",
                           smoothing   = 1.3,
                           width       = 1),
      marker        = list(symbol      = "square",
                           size        = 8),
      connectgaps   = TRUE
    ) %>%
    add_trace(
      data          = filter(data, Sex == "Females"),
      x             = ~Cohort_years,
      y             = ~y_var,
      split         = ~Sex,
      name          = ~Sex,
      hovertemplate = hover_template,
      color         = I(colors$f),
      opacity       = 0.8,
      type          = "scatter",
      mode          = "lines+markers",
      line          = list(shape       = "spline",
                           smoothing   = 1.3,
                           width       = 1),
      marker        = list(symbol      = "circle",
                           size        = 8),
      connectgaps   = TRUE
    ) %>%
    plotly::layout(
      plot_bgcolor = "transparent",
      shapes       = list(
        list(
          type         = "line",
          xref         = "paper",
          yref         = "y",
          x0           = 0,
          x1           = 1,
          y0           = overall_val,
          y1           = overall_val,
          line         = list(dash    = "dot",
                              width   = 1.5,
                              color   = "#44444480")
          ),
          list(
            type         = "line",
            xref         = "paper",
            yref         = "paper",
            x0           = 0,
            x1           = 1,
            y0           = 0,
            y1           = 0,
            line         = list(width = 0.5)
        ),
        list(
          type         = "line",
          xref         = "paper",
          yref         = "paper",
          x0           = 0,
          x1           = 0,
          y0           = 0,
          y1           = 1,
          line         = list(width = 0.5)
        )
      ),
      annotations  = list(
        list(
          text       = paste0("All Cohorts<br>", ylab$short, " = ", round(overall_val, digits = 2)),
          hovertext  = "Intercept represents value calculated across all generations/cohorts/sexes",
          font       = list(size   = 12,
                            family = "sans serif",
                            style  = "italic",
                            opacity = 0.7),
          x          = 1,
          y          = overall_val,
          xref       = "paper",
          yref       = "y"    ,
          xanchor    = "right",
          yanchor    = "bottom",
          showarrow  = FALSE
          ),
        list(
          text       = "",
          hovertext  = ylab$descr,
          x          = 0,
          y          = 0.5,
          xref       = "paper",
          yref       = "paper",
          xanchor    = "right",
          yanchor    = "center",
          align      = "center",
          showarrow  = FALSE)
      ),
      yaxis        = list(
        title      = ylab$lab,
        showgrid   = FALSE,
        zeroline   = FALSE,
        showlegend = TRUE,
        nticks     = 6,
        ticks      = "inside"
      ),
      xaxis        = list(
        title      = "Cohorts by Birth Year",
        showgrid   = TRUE,
        gridcolor  = "#2323231A",
        gridwidth  = 0.5,
        showlegend = FALSE,
        zeroline   = FALSE
      )
    )



}

#' Plot age-specific demographic variables with animation by birth year
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param variable Name of the column containing an age-specific variable to map onto the y-axis
#' options for variable: `Births`, `Deaths`, `Nx`, `Qx`, `Lx`, `Lx1`, `Px`, `ex`, `Tx`, `Mx`, `Fx`
#' @param sex One of `"Males"`, `"Females"`, or `"Overall"` to plot stats by sex (default is `"Overall"`)
#'
#' @return A plotly object
#' @export
#'
#' @importFrom dplyr filter rename distinct mutate select across pull bind_rows
#' @importFrom forcats fct_recode fct_relevel
#' @importFrom paletteer paletteer_c
#' @importFrom plotly layout plot_ly add_trace animation_opts animation_slider add_annotations
#' @importFrom purrr set_names
#' @importFrom stringr str_extract_all
#' @importFrom tidyselect where
#'
#'
plot_demog_age <- function(studbook, cohort_params = NULL, variable, sex = NULL) {
  if (is.null(sex)) { sex <- "Overall" }
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  total <- demog_ungrouped(studbook)  %>%
    mutate(Sex = fct_recode(Sex,
                            Overall = "Total",
                            Males   = "M",
                            Females = "F")) %>%
    mutate(Sex = fct_relevel(Sex,
                             "Males",
                             "Females",
                             "Overall"),
           Cohort_period = "All Years")
  data <- cohort_demog(studbook, params)  %>%
    mutate(Sex = fct_recode(Sex,
                            Overall = "Total",
                            Males   = "M",
                            Females = "F")) %>%
    mutate(Sex = fct_relevel(Sex,
                             "Males",
                             "Females",
                             "Overall")) %>%
    bind_rows(total) %>%
    rename(y_var = variable) %>%
    mutate(across(where(is.numeric), ~round(., digits = 2)))
  cohorts <- data %>%
    pull(Cohort_period) %>%
    unique()
  cohort_colors <- as.list(paletteer_c("harrypotter::ronweasley2", length(cohorts))) %>%
    set_names(cohorts) %>%
    unlist()

  symbols <- c(
    Males   = "square",
    Females = "circle",
    Overall = "x"
  )


  ylab <- demog_ylab(variable)
  hover_template <-  paste0(ylab$short, " = %{y}<br>Age = %{x} yrs")

  range_y <- list(min(data$y_var), max(data$y_var))
  range_x <- list(0, max(data$Age))



  plot <- plot_ly() %>%
    add_trace(
      data          = filter(data, Sex == sex),
      x             = ~Age,
      y             = ~y_var,
      split         = ~Cohort_period,
      frame         = ~Cohort_years,
      name          = ~Cohort_years,
      hovertemplate = hover_template,
      color         = ~Cohort_period,
      colors        = cohort_colors,
      type          = "scatter",
      mode          = "lines+markers",
      line          = list(shape = "spline"),
      marker        = list(opacity = 0.5,
                           symbol  = ~Sex,
                           symbols = symbols)
    ) %>%
    add_annotations(
      x         = 0.5,
      y         = 1,
      text      = paste0(sex),
      xref      = "paper",
      yref      = "paper",
      xanchor   = "center",
      yanchor   = "bottom",
      showarrow = FALSE
    ) %>%
    animation_opts(frame = 2000, transition = 1000, redraw = FALSE) %>%
    animation_slider(currentvalue =
                       list(prefix  = "Born ",
                            xanchor = "left",
                            font    = list(
                              color   = ~Cohort_period,
                              colors  = cohort_colors,
                              family  = "sans serif",
                              variant = "small-caps",
                              weight  = 800)
                       )
    ) %>%
    add_annotations(
      x          = 0,
      y          = 0.5,
      text       = "",
      hovertext  = ylab$descr,
      xref       = "paper",
      yref       = "paper",
      xanchor    = "center",
      yanchor    = "bottom",
      showarrow  = FALSE
    ) %>%
    plotly::layout(
      plot_bgcolor = "#ffffff",
      yaxis        = list(
        title      = ylab$lab,
        range      = range_y,
        showgrid   = FALSE,
        showline   = TRUE,
        showlegend = FALSE,
        nticks     = 6,
        ticks      = "inside"
      ),
      xaxis        = list(
        title      = "Age in Years",
        range      = range_x,
        showgrid   = TRUE,
        gridcolor  = "#2323231A",
        gridwidth  = 0.5,
        showline   = TRUE,
        showlegend = FALSE,
        nticks     = round(params$age_max/2)
      )
    )
  return(plot)
}

#' Plot age-specific demographic variables with animation by birth year and sex by line color
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param variable Name of the column containing an age-specific variable to map onto the y-axis
#' options for variable: `Births`, `Deaths`, `Nx`, `Qx`, `Lx`, `Lx1`, `Px`, `ex`, `Tx`, `Mx`, `Fx`
#'
#' @return A plotly object
#' @export
#'
#' @importFrom dplyr filter rename distinct mutate select across pull bind_rows
#' @importFrom forcats fct_recode fct_relevel
#' @importFrom paletteer paletteer_c
#' @importFrom plotly layout plot_ly add_trace animation_opts animation_slider add_annotations
#' @importFrom purrr set_names
#' @importFrom stringr str_extract_all
#' @importFrom tidyselect where
#'
#'
demog_age_bysex <- function(studbook, cohort_params = NULL, variable) {
  params  <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  total <- demog_ungrouped(studbook)  %>%
    mutate(Sex = fct_recode(Sex,
                            Overall = "Total",
                            Males   = "M",
                            Females = "F")) %>%
    mutate(Sex = fct_relevel(Sex,
                             "Males",
                             "Females",
                             "Overall"),
           Cohort_period = "All Years")
  data <- cohort_demog(studbook, params)  %>%
    mutate(Sex = fct_recode(Sex,
                            Overall = "Total",
                            Males   = "M",
                            Females = "F")) %>%
    mutate(Sex = fct_relevel(Sex,
                             "Males",
                             "Females",
                             "Overall")) %>%
    bind_rows(total) %>%
    rename(y_var = variable) %>%
    mutate(across(where(is.numeric), ~round(., digits = 2)))
  cohorts <- data %>%
    pull(Cohort_period) %>%
    unique()
  cohort_colors <- as.list(paletteer_c("harrypotter::ravenclaw2", length(cohorts))) %>%
    set_names(cohorts) %>%
    unlist()


  ylab <- demog_ylab(variable)
  hover_template <-  paste0(ylab$short, " = %{y}<br>Age = %{x} yrs")

  range_y <- list(min(data$y_var), max(data$y_var))
  range_x <- list(0, max(data$Age))

  plot <- plot_ly() %>%
    add_trace(
      data    = filter(data, Sex == "Males"),
      x       = ~Age,
      y       = ~y_var,
      split   = ~Cohort_period,
      frame   = ~Cohort_years,
      name    = ~Sex,
      hovertemplate = hover_template,
      color   = ~Cohort_period,
      colors  = cohort_colors,
      opacity = 0.2,
      type    = "scatter",
      mode    = "lines+markers",
      line    = list(shape = "spline", dash = "dot"),
      marker  = list(shape = "square", size = 8)
    ) %>%
    add_trace(
      data    = filter(data, Sex == "Females"),
      x       = ~Age,
      y       = ~y_var,
      split   = ~Cohort_period,
      frame   = ~Cohort_years,
      name    = ~Sex,
      hovertemplate = hover_template,
      color   = ~Cohort_period,
      colors  = cohort_colors,
      opacity = 0.2,
      type    = "scatter",
      mode    = "lines+markers",
      line    = list(shape = "spline", dash = "dash"),
      marker  = list(shape = "circle", size = 8)
    ) %>%
    add_trace(
      data    = filter(data, Sex == "Overall"),
      x       = ~Age,
      y       = ~y_var,
      split   = ~Cohort_period,
      frame   = ~Cohort_years,
      name    = ~Sex,
      hovertemplate = hover_template,
      color   = ~Cohort_period,
      colors  = cohort_colors,
      type    = "scatter",
      mode    = "lines+markers",
      line    = list(shape = "spline", width = 3)
    ) %>%
    animation_opts(frame = 2000, transition = 1000, redraw = FALSE) %>%
    animation_slider(currentvalue =
                       list(prefix  = "Born ",
                            xanchor = "left",
                            font    = list(
                              color   = ~Cohort_period,
                              colors  = cohort_colors,
                              family  = "sans-serif",
                              variant = "small-caps",
                              weight  = 800)
                       )
    ) %>%
    add_annotations(
      x          = 0,
      y          = 0.5,
      text       = "",
      hovertext  = ylab$descr,
      xref       = "paper",
      yref       = "paper",
      xanchor    = "center",
      yanchor    = "bottom",
      showarrow  = FALSE
    ) %>%
    plotly::layout(
      plot_bgcolor = "#ffffff",
      yaxis        = list(
        title      = ylab$lab,
        range      = range_y,
        showgrid   = FALSE,
        showline   = TRUE,
        showlegend = FALSE,
        nticks     = 6,
        ticks      = "inside"
      ),
      xaxis        = list(
        title      = "Age (Yr)",
        range      = range_x,
        showgrid   = TRUE,
        gridcolor  = "#2323231A",
        gridwidth  = 0.5,
        showline   = TRUE,
        showlegend = FALSE,
        nticks     = round(params$age_max/2)
      )
    )
  return(plot)
}




