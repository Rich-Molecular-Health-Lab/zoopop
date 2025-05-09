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
#'
#' @noRd
#'
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

#' Prepare demographic tibble for plotting with plotly
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param variable Name of the column containing an age-specific variable to map onto the y-axis
#' age-specific options for variable: `Births`, `Deaths`, `Nx`, `Qx`, `Lx`, `Lx1`, `Px`, `ex`, `Tx`, `Mx`, `Fx`
#' options for lifetime variable: `N0`, `N1`, `R0`, `T`, `MLE`, `Repro_first`, `Repro_last`, `age_max`, `lambda`, `r`
#' @param sex One of `"Males"`, `"Females"`, or `"Overall"` to plot stats by sex (`NULL` produces a dataframe with all three categories)
#' @param log_trans Logical indicating whether to log-transform the y-axis variable
#' @param age_spec Logical indicating whether to create list for age-specific version or not
#'
#' @return A dataframe to use in plotly
#'
#' @export
#'
#' @importFrom dplyr mutate rename filter bind_rows distinct select
#' @importFrom forcats fct_recode fct_relevel
#' @importFrom stringr str_extract_all
#' @importFrom tidyselect where
#'
#'
plot_demog_prep <- function(studbook, cohort_params = NULL, variable, log_trans = FALSE, age_spec = TRUE, sex = NULL) {
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
           Cohort_period = "All Years") %>%
    rename(y_var = variable)

  demog <- cohort_demog(studbook, params)  %>%
    mutate(Sex = fct_recode(Sex,
                            Overall = "Total",
                            Males   = "M",
                            Females = "F")) %>%
    mutate(Sex = fct_relevel(Sex,
                             "Males",
                             "Females",
                             "Overall")) %>%
    mutate(across(where(is.numeric), ~round(., digits = 2))) %>%
    rename(y_var = variable)

  if (is.null(sex)) {
    total <- total
    demog <- demog
  } else {
    total <- filter(total, Sex == sex)
    demog <- filter(demog, Sex == sex)
  }

  if (isTRUE(age_spec)) {
    data <- demog %>% bind_rows(total)
  } else if (isFALSE(age_spec)) {
    data <- demog %>%
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
      distinct()
  }

  if (isFALSE(log_trans)) {
    out <- data
  } else if (isTRUE(log_trans)) {
    out <- data %>% mutate(y_var = round(log(y_var), digits = 2))
  }
  return(out)
}

#' Generate a list of attributes to use in plotly plots of demographic variables
#'
#' @param data A dataframe to use in plotly (generated internally using the function `plot_demog_prep`)
#' @param variable Name of the column containing a variable to map onto the y-axis
#' options for age-specific variable: `Births`, `Deaths`, `Nx`, `Qx`, `Lx`, `Lx1`, `Px`, `ex`, `Tx`, `Mx`, `Fx`
#' options for lifetime variable: `N0`, `N1`, `R0`, `T`, `MLE`, `Repro_first`, `Repro_last`, `age_max`, `lambda`, `r`
#' @param age_spec Logical indicating whether to create list for age-specific version or not
#' @param log_trans Logical indicating whether to log-transform the y-axis variable
#' @return A list of parameter values to use in plotly plots of demographic variables
#'
#' @export
#'
#' @importFrom dplyr pull
#' @importFrom paletteer paletteer_c
#' @importFrom purrr set_names
#'
plot_demog_attr <- function(data, variable, age_spec = TRUE, log_trans = FALSE) {
  colors <- set_colors()
  cohorts       <- data %>% pull(Cohort_period) %>% unique()
  cohort_colors <- as.list(paletteer_c("harrypotter::ronweasley2", length(cohorts))) %>%
    set_names(cohorts) %>% unlist()
  symbols       <- c(Males = "square", Females = "circle", Overall = "x")
  ylab          <- demog_ylab(variable, log_trans)

  if (isTRUE(age_spec)) {
    hover_template <-  paste0(ylab$short, " = %{y}<br>Age = %{x} yrs")
    range_x <- list(0, max(data$Age))
    col     <- cohort_colors
  } else if (isFALSE(age_spec)) {
    hover_template <-  paste0(ylab$short, " = %{y}<br><i>Birth Year %{x}</i>")
    range_x <- list(NULL)
    col     <- list(Males = colors$m, Females = colors$f, Overall = colors$t) %>% unlist()
  }
  range_y       <- list(min(data$y_var), max(data$y_var))

  attr <- list(hover_template = hover_template,
               range_x        = range_x,
               range_y        = range_y,
               col            = col,
               symbols        = symbols,
               ylab           = ylab)
  return(attr)
}

#' Calculate a y-intercept value to use for non-age-specific demographic variables
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param variable Name of the column containing an age-specific variable to map onto the y-axis
#' age-specific options for variable: `Births`, `Deaths`, `Nx`, `Qx`, `Lx`, `Lx1`, `Px`, `ex`, `Tx`, `Mx`, `Fx`
#' options for lifetime variable: `N0`, `N1`, `R0`, `T`, `MLE`, `Repro_first`, `Repro_last`, `age_max`, `lambda`, `r`
#' @param log_trans Logical indicating whether to log-transform the y-axis variable
#' @return A list of parameter values to use in plotly plots of demographic variables
#' @keywords internal
#'
#' @export
#'
#' @importFrom dplyr mutate rename pull
#' @importFrom forcats fct_recode fct_relevel
#'
#'
demog_intercept <- function(studbook, cohort_params = NULL, variable, log_trans = FALSE) {
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
           Cohort_period = "All Years") %>%
    rename(y_var = variable)

  data <- plot_demog_prep(studbook, params, variable, log_trans, age_spec = FALSE)

  if (isTRUE(log_trans)) {
    total <- total %>% mutate(y_var = log(y_var))
  }

  if (variable == "N1") {
    overall_val <- mean(data$y_var)
  } else {
    overall_val <- pull(total, y_var) %>% unique()
  }

  result <- mean(overall_val) %>% round(digits = 2)

  return(result)
}

#' Generate a list of shapes and annotations to use in plotly plots of demographic variables
#'
#' @param attr A list of attribute values created using the internal function `plot_demog_attr`
#' @param overall_val value of y when all cohorts are combined (`NULL` when plotting age-specific variables)
#' @param age_spec Logical indicating whether to create list for age-specific version or not
#' @param log_trans Logical indicating whether to log-transform the y-axis variable
#' @return A list of parameter values to use in plotly plots of demographic variables
#' @keywords internal
#'
#' @export
#'
plot_demog_annot <- function(attr, overall_val = NULL, age_spec = TRUE, log_trans = FALSE) {
  xline <- list(
    type         = "line",
    xref         = "paper",
    yref         = "paper",
    x0           = 0,
    x1           = 1,
    y0           = 0,
    y1           = 0,
    line         = list(width = 0.5)
  )
  yline <-
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
  yhover <- list(
    text       = attr$ylab,
    textangle  = -90,
    hovertext  = attr$ylab$descr,
    x          = 0,
    y          = 0.5,
    xref       = "paper",
    yref       = "paper",
    xanchor    = "center",
    yanchor    = "center",
    align      = "center",
    showarrow  = FALSE,
    visible    = FALSE
  )

  if (isTRUE(age_spec)) {
    shapes      <- list(xline, yline)
    annotations <- list(yhover)
  } else if (isFALSE(age_spec)) {
    shapes <- list(
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
      xline, yline
    )

    annotations <- list(
      list(
        text       = paste0("All Cohorts<br>", attr$ylab$short, " = ", overall_val),
        hovertext  = "Intercept represents value calculated across all generations/cohorts/sexes",
        font       = list(size   = 12,
                          family = "sans serif",
                          style  = "italic",
                          opacity = 0.7),
        x          = 1,
        y          = overall_val,
        xref       = "paper",
        yref       = "y"    ,
        xanchor    = "center",
        yanchor    = "top",
        showarrow  = TRUE
      ),
      yhover
    )
  }

  layers <- list(shapes = shapes, annotations = annotations)

  return(layers)
}

#' Add animation to age-specific plots over multiple generations
#'
#' @param plot plotly object to animate
#' @param data A dataframe to use in plotly (generated internally using the function `plot_demog_prep`)
#' @param attr A list of attribute values created using the internal function `plot_demog_attr`
#' @return A list of parameter values to use in plotly plots of demographic variables
#'
#' @export
#'
#' @importFrom plotly animation_opts animation_slider
#'
#'
animate_generations <- function(plot, data, attr) {
  plot %>%
    animation_opts(frame = 2000, transition = 1000, redraw = FALSE) %>%
    animation_slider(currentvalue =
                       list(prefix  = "Born ",
                            xanchor = "left",
                            font    = list(
                              color   = data$Cohort_period,
                              colors  = attr$col,
                              family  = "sans serif",
                              variant = "small-caps",
                              weight  = 800)
                       )
    )
}

#' Add a scatter trace for a demographic variable to a plotly object
#'
#' @param plot A plotly object
#' @param data A dataframe to use in plotly (generated internally using the function `plot_demog_prep`)
#' @param attr A list of attribute values created using the internal function `plot_demog_attr`
#' @param primary Logical indicating whether the trace will be primary in the plot (`TRUE`) or a background trace (`FALSE`)
#' @param age_spec Logical indicating whether the y-variable plotted is age-specific or not
#' @param sex Vector of sex classes to include in trace (default is all three)
#' @return A list of parameter values to use in plotly plots of demographic variables
#'
#' @export
#'
#' @importFrom plotly add_trace
#'
plot_demog_trace <- function(plot, data, attr, primary = TRUE, age_spec = TRUE, sex = NULL) {
  smoothing <- 1.3
  mark_size <- 8
  shape     <- "spline"
  type      <- "scatter"
  mode      <- "lines+markers"
  if (!is.null(sex) && sex == "Males") {
    dash <- "dash"
    data <- filter(data, Sex == "Males")
  } else if (!is.null(sex) && sex == "Females") {
      dash <- "dot"
      data <- filter(data, Sex == "Females")
  } else if (!is.null(sex) && sex == "Overall") {
        dash <- "solid"
        data <- filter(data, Sex == "Overall")
  } else if (is.null(sex)) {
    dash <- "solid"
        }
  if (isTRUE(primary)) {
    width   <- 3
    opacity <- 1
  } else if (isFALSE(primary)) {
    width   <- 1
    opacity <- 0.5
  }
  if (isTRUE(age_spec)) {
   out <- add_trace(
      p             = plot,
      data          = data,
      x             = ~Age,
      y             = ~y_var,
      split         = ~Cohort_period,
      frame         = ~Cohort_years,
      name          = ~Sex,
      hovertemplate = attr$hover_template,
      color         = ~Cohort_period,
      symbol        = ~Sex,
      colors        = attr$col,
      symbols       = attr$symbols,
      opacity       = opacity,
      type          = type,
      mode          = mode,
      line          = list(shape = shape, dash = dash, width = width),
      marker        = list(opacity = opacity, size = mark_size),
      connectgaps   = TRUE
    )
  } else if (isFALSE(age_spec)) {
   out <- add_trace(
      p             = plot,
      data          = data,
      x             = ~Cohort_years,
      y             = ~y_var,
      split         = ~Sex,
      name          = ~Sex,
      hovertemplate = attr$hover_template,
      color         = ~Sex,
      symbol        = ~Sex,
      colors        = attr$col,
      symbols       = attr$symbols,
      opacity       = opacity,
      type          = type,
      mode          = mode,
      line          = list(shape = shape, dash = dash, width = width),
      marker        = list(opacity = opacity, size = mark_size),
      connectgaps   = TRUE
    )
  }
  return(out)
}

#' Add a layout to a demographic plotly object
#'
#' @param plot A plotly object
#' @param layer A list of shapes and text annotations created using the internal function `plot_demog_annot`
#' @param attr A list of attribute values created using the internal function `plot_demog_attr`
#' @param age_spec Logical indicating whether the y-variable plotted is age-specific or not
#' @return A list of parameter values to use in plotly plots of demographic variables
#'
#' @export
#'
#' @importFrom plotly layout
#'
plot_demog_layout <- function(plot, layer, attr, age_spec = TRUE) {
  yaxis <- list(
    title      = attr$ylab$lab,
    showgrid   = FALSE,
    zeroline   = FALSE,
    showlegend = TRUE,
    nticks     = 6,
    ticks      = "inside"
  )

  if (isTRUE(age_spec)) {
    xaxis <- list(
      title      = "Age in Years",
      range      = attr$range_x,
      showgrid   = TRUE,
      gridcolor  = "#2323231A",
      gridwidth  = 0.5,
      showlegend = FALSE,
      zeroline   = FALSE
    )
  } else if (isFALSE(age_spec)) {
    xaxis <- list(
      title      = "Cohorts by Birth Year",
      showgrid   = TRUE,
      gridcolor  = "#2323231A",
      gridwidth  = 0.5,
      showlegend = FALSE,
      zeroline   = FALSE
    )
  }

  out <- plotly::layout(
    p            = plot,
    plot_bgcolor = "transparent",
    shapes       = layer$shapes,
    annotations  = layer$annotations,
    yaxis        = yaxis,
    xaxis        = xaxis
  )

  return(out)

}


#' Plot lifetime demographic variables by sex and birth year
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param variable Name of the column containing an age-specific variable to map onto the y-axis
#' options for variable: `N0`, `N1`, `R0`, `T`, `MLE`, `Repro_first`, `Repro_last`, `age_max`, `lambda`, `r`
#' @param log_trans Logical indicating whether to log-transform the y-axis variable
#'
#' @return A plotly object
#' @export
#'
#' @importFrom plotly plot_ly
#'
#'
plot_demog <- function(studbook, cohort_params = NULL, variable, log_trans = FALSE) {
  params      <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  data        <- plot_demog_prep(studbook, params, variable, log_trans, age_spec = FALSE)
  attr        <- plot_demog_attr(data, variable, age_spec = FALSE, log_trans)
  overall_val <- demog_intercept(studbook, params, variable, log_trans)
  layer       <- plot_demog_annot(attr = attr, overall_val = overall_val, age_spec = FALSE, log_trans)

  plot <- plot_ly() %>%
    plot_demog_trace(., data, attr, age_spec = FALSE) %>%
    plot_demog_layout(., layer, attr, age_spec = FALSE)

  return(plot)
}


#' Plot age-specific demographic variables with animation by birth year
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param variable Name of the column containing an age-specific variable to map onto the y-axis
#' options for variable: `Births`, `Deaths`, `Nx`, `Qx`, `Lx`, `Lx1`, `Px`, `ex`, `Tx`, `Mx`, `Fx`
#' @param sex One of `"Males"`, `"Females"`, or `"Overall"` to plot stats by sex (default is `"Overall"`)
#' @param log_trans Logical indicating whether to log-transform the y-axis variable
#'
#' @return A plotly object
#' @export
#'
#' @importFrom plotly plot_ly
#'
#'
plot_demog_age <- function(studbook, cohort_params = NULL, variable, sex = NULL, log_trans = FALSE) {
  if (is.null(sex)) { sex <- "Overall" }
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  data   <- plot_demog_prep(studbook, params, variable, log_trans, age_spec = TRUE, sex)
  attr   <- plot_demog_attr(data, variable, age_spec = TRUE, log_trans)
  layer  <- plot_demog_annot(attr, age_spec = TRUE, log_trans)

  plot <- plot_ly() %>%
    plot_demog_trace(., data, attr, sex) %>%
    animate_generations(., data, attr) %>%
    plot_demog_layout(., layer, attr)

  return(plot)
}

#' Plot age-specific demographic variables with animation by birth year and sex by line color
#'
#' @param studbook Studbook tibble
#' @param cohort_params A named list of the parameter values to use for cohorts (`Year_min`, `Year_max`, `span`, `age_max`) (optional)
#' @param variable Name of the column containing an age-specific variable to map onto the y-axis
#' options for variable: `Births`, `Deaths`, `Nx`, `Qx`, `Lx`, `Lx1`, `Px`, `ex`, `Tx`, `Mx`, `Fx`
#' @param log_trans Logical indicating whether to log-transform the y-axis variable
#'
#' @return A plotly object
#' @export
#'
#' @importFrom plotly plot_ly
#'
#'
plot_age_ts <- function(studbook, cohort_params = NULL, variable, log_trans = FALSE) {
  params <- cohort_defaults(studbook = studbook, cohort_params = cohort_params)
  data   <- plot_demog_prep(studbook, params, variable, log_trans, age_spec = TRUE)
  attr   <- plot_demog_attr(data, variable, age_spec = TRUE, log_trans)
  layer  <- plot_demog_annot(attr, age_spec = TRUE, log_trans)

  plot <- plot_ly() %>%
    plot_demog_trace(., data, attr, primary = FALSE, sex = "Males") %>%
    plot_demog_trace(., data, attr, primary = FALSE, sex = "Females") %>%
    plot_demog_trace(., data, attr, sex = "Overall") %>%
    animate_generations(., data, attr) %>%
    plot_demog_layout(., layer, attr)

  return(plot)
}




