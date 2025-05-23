
studbook_nodes <- studbook_short(studbook) %>%
  mutate(id      = as.integer(ID))
colors   <- set_colors() %>%
  set_names( ~gsub("sire", "m_d", .)) %>%
  set_names( ~gsub("dam", "f_d", .))
node_trios    <- pedigree_births(ped, studbook)
connector_nodes <- trios %>%
  select(
    pair,
    sibs,
    fillcolor = colorLoc,
    Loc,
    Institution,
    State_Province,
    Country,
    iconLoc
  )  %>%
  pivot_longer(c("pair", "sibs"), names_to = "node_type", values_to = "id") %>%
  mutate(id = as.integer(id)) %>%
  arrange(id) %>% distinct() %>%
  relocate(id, node_type) %>%
  mutate(color   = fillcolor,
         tooltip = as.character(str_glue("{iconLoc}{Loc}: {Institution}, {State_Province}")),
         shape   = "point")

parent_nodes <- trios %>%
  select(m = dad, f = mom)  %>%
  pivot_longer(c("m", "f"), names_to = "node_type", values_to = "id") %>%
  mutate(id = as.integer(id)) %>%
  arrange(id) %>% distinct()

subject_nodes <- trios %>%
  select(id = offspring, node_type = Sex) %>%
  mutate(node_type = str_to_lower(node_type),
         id    = as.integer(id)) %>%
  bind_rows(parent_nodes) %>%
  distinct() %>%
  arrange(id) %>%
  left_join(studbook_nodes, by = "id") %>%
  mutate(node_type = if_else(
    Status == "A", node_type, str_glue("{node_type}_d")
  )) %>%
  mutate(
    image = case_match(
      node_type,
      "m"   ~ "inst/icons/icon_m.png",
      "f"   ~ "inst/icons/icon_f.png",
      "u"   ~ "inst/icons/icon_u.png",
      "m_d" ~ "inst/icons/icon_m_deceased.png",
      "f_d" ~ "inst/icons/icon_f_deceased.png"
    ),
    color = case_match(
      node_type,
      "m"   ~ colors[["m"]],
      "f"   ~ colors[["f"]],
      "u"   ~ colors[["u"]],
      "m_d" ~ colors[["m_d"]],
      "f_d" ~ colors[["f_d"]]
    ),
    fillcolor = color,
    label     = if_else(is.na(name_spec), as.character(id), as.character(str_glue("{id} ({name_spec})"))),
    fontsize  = if_else(is.na(name_spec), 8, 11),
    tooltip   = case_when(
      exclude == "n" ~ as.character(str_glue("{ID} {Sex}: Included in breeding population<br>Currently age {age_last} at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
      exclude == "age" ~ as.character(str_glue("{ID} {Sex}: Excluded from breeding population due to age<br>Currently age {age_last} at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
      exclude == "behavior" ~ as.character(str_glue("{ID} {Sex}: Excluded from breeding population for behavioral reasons<br>Currently age {age_last} at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
      exclude == "deceased" ~ as.character(str_glue("{ID} {Sex}: Deceased (age {age_last}) at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
      exclude == "hypothetical" ~ as.character(str_glue("{ID} {Sex}: Hypothetical ID created to represent missing parent at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}"))
    ))

nodes <- bind_rows(subject_nodes, connector_nodes) %>%
  distinct() %>%
  arrange(id) %>%
  mutate(node = row_number()) %>%
  select(
    node,
    id,
    label,
    shape,
    color,
    style,
    fillcolor,
    tooltip
  )





#' Generate nodes and edges from pedigree for a network object
#'
#' @param pedigree A pedigree object
#' @param studbook A studbook tibble
#' @return A tibble of edges with visual attributes
#' @export
#'
#' @importFrom dplyr bind_rows distinct left_join mutate pull transmute
#' @importFrom purrr map map2
#' @importFrom rlang set_names
#' @importFrom tibble enframe
#' @importFrom tidyr pivot_longer unnest_longer
build_ped_network <- function(pedigree, studbook) {
  trios     <- pedigree_births(pedigree, studbook)
  children <- distinct(trios, offspring, dad, mom, Sex, pair) %>%
    arrange(pair, offspring) %>%
    mutate(sibs      = max(pair) + consecutive_id(pair)) %>%
    mutate(across(c(sibs, pair), ~as.character(.)))
  parents <- distinct(trios, dad, mom, pair) %>%
    rename(m = dad, f = mom, to = pair) %>%
    pivot_longer(c("m", "f"), names_to = "sex", values_to = "from") %>%
    mutate(to = as.character(to))
  edges <- select(children,
                  from = pair,
                  to   = sibs)  %>%
    mutate(edge_type = "pairnode_sibnode") %>%
    distinct() %>%
    bind_rows(
      select(children,
             from = pair,
             to   = offspring)
    ) %>%
    mutate(edge_type = if_else(is.na(edge_type), "sibnode_offspring", edge_type)) %>%
    distinct() %>%
    bind_rows(select(parents, from, to)) %>%
    mutate(edge_type = if_else(is.na(edge_type), "parent_pairnode", edge_type))
  nodes <- select(children,
                  offspring,
                  dad,
                  mom,
                  connect_pair = pair,
                  connect_sibs = sibs) %>%
    pivot_longer(cols = c("offspring",
                          "dad",
                          "mom",
                          "connect_pair",
                          "connect_sibs"),
                 names_to  = "node_type",
                 values_to = "node") %>%
    mutate(node_type = if_else(node_type %in% c("offspring", "dad", "mom"), "individual", node_type))
}
#' Generate a node dataframe from the pedigree that functions within `DiagrammeR`
#'
#' @param pedigree A pedigree object
#' @param studbook A studbook tibble
#' @return A formatted node dataframe for `DiagrammeR`
#' @export
#'
#' @importFrom dplyr distinct arrange mutate across select consecutive_id if_else rename
#' @importFrom tidyr pivot_longer
#' @importFrom DiagrammeR create_node_df
ped_ndf <- function(pedigree, studbook) {
  studbook_nodes <- studbook_short(studbook) %>%
    mutate(id      = as.integer(ID))
  colors   <- set_colors() %>%
    set_names( ~gsub("sire", "m_d", .)) %>%
    set_names( ~gsub("dam", "f_d", .))
  node_trios    <- pedigree_births(pedigree, studbook)

  connector_nodes <- node_trios %>%
    select(
      pair,
      sibs,
      Loc,
      Institution,
      State_Province,
      Country,
      iconLoc
    )  %>%
    pivot_longer(c("pair", "sibs"), names_to = "node_type", values_to = "id") %>%
    mutate(id = as.integer(id)) %>%
    arrange(id) %>% distinct() %>%
    relocate(id, node_type) %>%
    mutate(style     = "invisible",
           height    = 0.1,
           width     = 0.1,
           fixedsize = TRUE,
           group     = "connector",
           tooltip   = as.character(str_glue("{iconLoc}{Loc}: {Institution}, {State_Province}")),
           shape     = "point")

  parent_nodes <- node_trios %>%
    select(m = dad, f = mom)  %>%
    pivot_longer(c("m", "f"), names_to = "node_type", values_to = "id") %>%
    mutate(id = as.integer(id)) %>%
    arrange(id) %>% distinct()

  subject_nodes <- node_trios %>%
    select(id = offspring, node_type = Sex) %>%
    mutate(node_type = str_to_lower(node_type),
           id    = as.integer(id)) %>%
    bind_rows(parent_nodes) %>%
    distinct() %>%
    arrange(id) %>%
    left_join(studbook_nodes, by = "id") %>%
    mutate(node_type = if_else(
      Status == "A", node_type, str_glue("{node_type}_d")
    )) %>%
    mutate(
      height    = 1.5,
      width     = 1.5,
      fixedsize = FALSE,
      style = case_match(
        node_type,
        "m"   ~ "solid, filled",
        "f"   ~ "solid, filled",
        "u"   ~ "solid, filled",
        "m_d" ~ "dashed, filled",
        "f_d" ~ "dashed, filled"
      ),
      shape = case_match(
        node_type,
        "m"   ~ "square",
        "f"   ~ "circle",
        "u"   ~ "diamond",
        "m_d" ~ "square",
        "f_d" ~ "circle"
      ),
      fillcolor = case_match(
        node_type,
        "m"   ~ colors[["m"]],
        "f"   ~ colors[["f"]],
        "u"   ~ colors[["u"]],
        "m_d" ~ colors[["m_d"]],
        "f_d" ~ colors[["f_d"]]
      ),
      color      = "black",
      fontsize   = 20,
      group      = "subject",
      label      = if_else(is.na(name_spec), as.character(id), as.character(str_glue("{id} ({name_spec})"))),
      tooltip    = case_when(
        exclude  == "n" ~ as.character(str_glue("{ID} {Sex}: Included in breeding population<br>Currently age {age_last} at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
        exclude  == "age" ~ as.character(str_glue("{ID} {Sex}: Excluded from breeding population due to age<br>Currently age {age_last} at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
        exclude  == "behavior" ~ as.character(str_glue("{ID} {Sex}: Excluded from breeding population for behavioral reasons<br>Currently age {age_last} at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
        exclude  == "deceased" ~ as.character(str_glue("{ID} {Sex}: Deceased (age {age_last}) at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}<br>Born {Date_birth} at {Institution_birth}, {State_Province_birth} ({iconLoc_birth}{Loc_birth})")),
        exclude  == "hypothetical" ~ as.character(str_glue("{ID} {Sex}: Hypothetical ID created to represent missing parent at {Institution_last}, {State_Province_last} ({iconLoc_last}{Loc_last}"))
      ))

  nodes <- bind_rows(subject_nodes, connector_nodes) %>%
    distinct() %>%
    arrange(id) %>%
    mutate(node = row_number()) %>%
    select(
      node,
      id,
      label,
      shape,
      color,
      style,
      group,
      fillcolor,
      fontsize,
      tooltip,
      fixedsize,
      width,
      height
    )
  return(nodes)
}


#' Create a pedigree graph using `DiagrammeR`
#'
#' @param nodes A node dataframe created using `ped_ndf`
#' @param edges An edge dataframe created using `ped_edf`
#' @return A formatted graphvis object for rendering in `DiagrammeR`
#' @export
#'
#' @importFrom dplyr distinct arrange mutate across select consecutive_id if_else rename
#' @importFrom DiagrammeR create_graph
#'
#'
ped_graph <- function(nodes, edges) {

  edges <- edges %>%
    left_join(select(nodes, id, node),
              by = join_by(from == id)) %>%
    mutate(from = node, .keep = "unused") %>%
    left_join(select(nodes, id, node), by = join_by(to == id)) %>%
    mutate(to = node, .keep = "unused") %>%
    mutate(across(c(id, from, to), ~as.character(.)),
           style     = "solid",
           penwidth  = 3
    )

  nodes <- nodes %>% mutate(across(c(node, label), ~as.character(.)))

  n   <- nrow(nodes)
  ndf <- create_node_df(
    n          = n,
    node       = nodes$node,
    label      = nodes$label,
    group      = nodes$group,
    shape      = nodes$shape,
    color      = nodes$color,
    style      = nodes$style,
    fixedsize  = nodes$fixedsize,
    fontsize   = nodes$fontsize,
    width      = nodes$width,
    height     = nodes$height,
    fillcolor  = nodes$fillcolor
  )

  edf <- create_edge_df(
    id          = edges$id,
    rel         = edges$rel,
    style       = edges$style,
    from        = edges$from,
    to          = edges$to,
    arrowhead   = edges$arrowhead,
    arrowtail   = edges$arrowtail,
    penwidth    = edges$penwidth
  )

  graph <- create_graph(nodes_df = ndf, edges_df = edf, attr_theme = "tb")
  output <- render_graph(graph)
  return(output)
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


