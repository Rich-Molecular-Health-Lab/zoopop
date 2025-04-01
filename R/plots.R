# plots.R
# Functions for generating demographic and pedigree visualizations using plotly and pedtools

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
#' Annotate lambda growth values with human-readable hover text
#'
#' @param df A data frame with a column `lambda`
#' @return The same data frame with a new `hover_lambda` column
#' @export
#'
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom glue glue
annotate_lambda <- function(df) {
  df %>%
    mutate(hover_lambda = abs(round((lambda - 1) * 100, digits = 1))) %>%
    mutate(
      hover_lambda = if_else(
        lambda >= 1,
        as.character(glue::glue("Population growing by {hover_lambda}%")),
        as.character(glue::glue("Population declining by {hover_lambda}%"))
      )
    )
}

#' Plot population trends using census data
#'
#' @param census_df A tibble with columns Date, Females, Males, Unidentified
#' @param palette A named list of colors for "f", "m", and "u"
#' @param title The title of the plot
#'
#' @return A plotly plot object
#' @export
#'
#' @importFrom plotly add_trace
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
plot_census <- function(census_df, palette, title) {
  fill.col <- lighten_plotly_pal(palette, 26)

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
                    color     = palette[["u"]],
                    shape     = "spline",
                    smoothing = 0.8,
                    width     = 1.5),
                  fillcolor   = fill.col[["u"]]) %>%
    add_trace(y         = ~Females,
              name      = "Females",
              line      = list(
                color     = palette[["f"]],
                shape     = "spline",
                smoothing = 0.8,
                width     = 1.5),
              fillcolor = fill.col[["f"]]) %>%
    add_trace(y         = ~Males,
              name      = "Males",
              line      = list(
                color     = palette[["m"]],
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
           xaxis        = list(
             title    = "Date",
             showline = FALSE,
             showgrid = TRUE,
             rangeslider = list(
               visible = TRUE
               )
             )
           )
  return(plot)
}

#' Plot lambda growth rates by cohort
#'
#' @param life_table A tibble with lambda values and Sex, CohortLabel columns
#' @param title Plot title
#' @param palette A color palette in list format
#'
#' @return A plotly object
#' @export
#'
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
plot_lambda <- function(life_table, palette, title) {
  col.pal    <- set_plotly_pal(palette)
  fills      <- lighten_plotly_pal(col.pal, 33)
  annotated  <- lifeTab_static(life_table) %>% annotate_lambda()
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
  plot <- plot_ly(annotated %>% filter(Sex != "Total"),
                  x           = ~CohortLabel,
                  y           = ~lambda,
                  color       = ~Sex,
                  type        = "scatter",
                  mode        = "lines+markers",
                  colors      = col.pal,
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
    layout(title        = title,
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

# plot_helpers.R
# Pedigree plot generator using pedtools::plot

#' Plot a pedigree object using pedtools with custom formatting
#'
#' @param pedigree A single pedigree object (from `pedtools::ped()`)
#' @param name A title for the plot (used as label)
#' @param ped.palette A named list mapping individuals to colors by sex and status (from `zoolabs::set_ped_fills`)
#' @param studbook A tibble with `ID`, `Sex`, `Status`, and other metadata
#' @param ... Additional arguments passed to `pedtools::plot()`
#'
#' @return A pedigree plot (base graphics)
#' @export
#'
#' @importFrom htmltools plotTag
#'
plot_pedigree <- function(pedigree, name, studbook, ped.palette, ...) {
  deceased_ids <- deceased(studbook)
  plotTag(
   expr = plot(
      pedigree,
      title    = name,
      cex      = 0.4,
      deceased = deceased_ids,
      labs     = NULL,
      fill     = ped.palette,
      lwd      = 0.3,
      col      = "black",
      pty      = "m",
      ...
    ),
   alt    = "pedigree-plot",
   width  = 600,
   height = 600
  )
}
#' Capture a pedigree plot for exporting an image file
#'
#' @param pedigree A single pedigree object (from `pedtools::ped()`)
#' @param name A title for the plot (used as label)
#' @param ped.palette A named list mapping individuals to colors by sex and status (from `zoolabs::set_ped_fills`)
#' @param studbook A tibble with `ID`, `Sex`, `Status`, and other metadata
#' @param ... Additional arguments passed to `pedtools::plot()`
#'
#' @return A pedigree plot (base graphics)
#' @export
#'
#' @importFrom htmltools capturePlot
#'
capture_ped_file <- function(pedigree, name, studbook, ped.palette, ...) {
  deceased_ids <- deceased(studbook)
  pngpath      <- tempfile(fileext = ".png")
  capturePlot(
    expr = plot(
      pedigree,
      title    = name,
      cex      = 0.4,
      deceased = deceased_ids,
      labs     = NULL,
      fill     = ped.palette,
      lwd      = 0.3,
      col      = "black",
      pty      = "m",
      ...
    ),
    filename  = pngpath,
    width     = 600,
    height    = 600,
    res       = 96
  )
}
#' Plot a series of pedigree objects using pedtools with custom formatting
#'
#' @param ped.list A list of pedigree objects (from `pedtools::ped()`)
#' @param palette A named list of color values
#' @param studbook A tibble with `ID`, `Sex`, `Status`, and other metadata
#' @param ... Additional arguments passed to `pedtools::plot()`
#'
#' @return A series of pedigree plots (base graphics)
#' @export
#'
#' @importFrom purrr imap
plot_ped_series <- function(ped.list, palette, studbook) {
  ped.palette   <- set_ped_fills(ped.list, palette, studbook)
  imap(ped.list, \(x, idx) plot_pedigree(x, idx, studbook, ped.palette))
}

#' Annotate kinship matrix with hover text for mating pair comparisons
#'
#' @param matrix A kinship matrix (usually filtered to living individuals)
#' @param pedigree.living A pedigree object from `pedtools::ped()` for the living population
#' @param studbook A tibble containing metadata for individuals
#'
#' @return A matrix of hover text strings matching dimensions of `matrix`
#' @export
#'
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr intersect
#' @importFrom dplyr left_join
#' @importFrom tibble enframe
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom glue glue
annotate_kin_matrix <- function(matrix, pedigree.living, studbook) {
  F_vec <- F_vector(pedigree.living, studbook) %>%
    enframe(name = "ID", value = "F_vec") %>%
    mutate(ID = as.integer(ID))

  living.data <- family_history(pedigree.living, studbook)
  annotate <- living.data %>%
    mutate(ID = as.integer(ID)) %>%
    left_join(F_vec, by = join_by(ID)) %>%
    mutate(hoverText = glue::glue(
      "{LocCurrent}<br>{Age} yrs (Born {DateBirth})<br>"
      , "Mother: {Dam}, Father: {Sire}<br>"
      , "{N_Siblings} Siblings, {N_Children} Offspring, {N_Descendants} Descendants"
    )) %>%
    arrange(ID)

  kinship_btp  <- subset_matrix_living(matrix, studbook)
  text_males   <- intersect(as.character(living.males(studbook)), rownames(kinship_btp))
  text_females <- intersect(as.character(living.females(studbook)), colnames(kinship_btp))

  annotate.m <- annotate %>% filter(Sex == "Male"   & ID %in% text_males)   %>% select(ID, hoverText) %>% arrange(match(ID, text_males))
  annotate.f <- annotate %>% filter(Sex == "Female" & ID %in% text_females) %>% select(ID, hoverText) %>% arrange(match(ID, text_females))

  hover_matrix <- outer(
    annotate.m$hoverText,
    annotate.f$hoverText,
    FUN = function(m_text, f_text) {
      paste0("<br><b>Male:</b><br>", m_text, "<br><br><b>Female:</b><br>", f_text)
    }
  )
  rownames(hover_matrix) <- annotate.m$ID
  colnames(hover_matrix) <- annotate.f$ID
  return(hover_matrix)
}

#' Generate a kinship or relatedness heatmap using heatmaply
#'
#' @param matrix A numeric matrix of pairwise values (e.g., kinship)
#' @param title Title for the heatmap
#' @param key.title Title for the color bar legend
#' @param xlab Optional x-axis label
#' @param ylab Optional y-axis label
#'
#' @return A heatmaply widget
#' @export
#'
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 theme
#' @importFrom heatmaply heatmaply
#' @importFrom paletteer paletteer_d
#' @importFrom stringr str_remove_all
matrix.heatmap <- function(matrix, title, key.title, xlab = "Females", ylab = "Males") {
  filename <- paste0("heatmap_", stringr::str_remove_all(title, "\\s"), ".html")

  plot <- heatmaply::heatmaply(
    matrix,
    dendrogram       = "none",
    main             = title,
    scale            = "none",
    colors           = paletteer::paletteer_d("rcartocolor::Temps"),
    margins          = c(60, 100, 40, 20),
    grid_color       = "white",
    grid_width       = 0.00001,
    label_format_fun = function(value) round(value, digits = 3),
    titleX           = TRUE,
    hide_colorbar    = FALSE,
    key.title        = key.title,
    branches_lwd     = 0.1,
    fontsize_row     = 10,
    fontsize_col     = 10,
    labCol           = colnames(matrix),
    labRow           = rownames(matrix),
    heatmap_layers   = theme(axis.line = element_blank())
  )
  return(plot)
}

#' Subset a full kinship matrix to include only living males vs. living females
#'
#' @param matrix A symmetric kinship matrix
#' @param studbook A tibble with metadata for all individuals
#'
#' @return A matrix with rows = males, cols = females, both alive
#' @export
#'
#' @importFrom dplyr intersect
subset_matrix_living <- function(matrix, studbook) {
  living_males   <- intersect(as.character(living.males(studbook)), rownames(matrix))
  living_females <- intersect(as.character(living.females(studbook)), colnames(matrix))
  matrix[living_males, living_females]
}
