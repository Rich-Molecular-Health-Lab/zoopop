#' Create Numeric Versions of Sex for Pedigree Analysis
#'
#' This function converts the character values in the `Sex` column of a studbook
#' into numeric representations appropriate for different pedigree tools. The
#' `sex_ped` variable uses the convention for `pedtools` (where 0 represents
#' undetermined sex), and the `sex_kin` variable uses the convention for `kinship2`
#' (where 3 represents undetermined sex).
#'
#' @param studbook A data frame of studbook data produced by [read_studbook()].
#' @return An updated studbook with two new variables, `sex_ped` and `sex_kin`.
#' @export
#'
#' @importFrom dplyr mutate across
#' @importFrom magrittr %>%

pedigree_studbook <- function(studbook) {
  studbook <- studbook_short(studbook) %>%
    mutate(sex_ped = case_match(Sex,
                                "U" ~"0",
                                "M" ~"1",
                                "F" ~"2"),
           sex_kin = case_match(Sex,
                                "U" ~"3",
                                "M" ~"1",
                                "F" ~"2")) %>%
    mutate(across(c(sex_ped, sex_kin), ~as.numeric(.)))
  return(studbook)
}

#' Create a List of Pedigree Objects from the Studbook
#'
#' This function converts a studbook into a list of pedigree objects (using
#' `pedtools::ped()`) and excludes any singletons (pedigrees with a size of 1 or less).
#'
#' @param studbook A data frame of studbook data produced by [read_studbook()].
#' @return A list of pedigree objects excluding any singletons.
#' @export
#'
#' @importFrom pedtools ped pedsize
#' @importFrom purrr discard
build_ped_series <- function(studbook) {
  studbook <- pedigree_studbook(studbook)
  pedigree <- ped(
    id     = studbook$ID,
    fid    = studbook$Sire,
    mid    = studbook$Dam,
    sex    = studbook$sex_ped
  ) %>% discard(., \(x) pedtools::pedsize(x) <= 1)
  return(pedigree)
}

#' Plot a Pedigree Object with Custom Formatting
#'
#' This function plots a single pedigree object using custom formatting. It
#' applies a custom color palette and highlights deceased individuals.
#'
#' @param pedigree A single pedigree object created with `pedtools::ped()`.
#' @param studbook A tibble containing metadata (e.g., `ID`, `Sex`, `Status`, etc.).
#' @param name An optional title for the plot; if not provided, the global variable
#'   `FAMID` is used.
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @param labs_spec A logical vector indicating whether any individuals with special labels named under `name_spec` in the studbook should be labeled as such and rendered with emphasis in the plot.
#' @param ... Additional arguments passed to `pedtools::plot()`.
#' @return A pedigree plot as a base graphics object wrapped in a htmltools plotTag.
#' @export
#'
#' @importFrom dplyr setdiff filter select
#' @importFrom htmltools plotTag
#' @importFrom purrr set_names
plot_pedigree <- function(pedigree, studbook, name = NULL, palette = NULL, labs_spec = FALSE, ...) {
  ped_ids <- c(pedigree[["ID"]])
  if (is.null(palette)) {
    palette <- set_colors()
  }
  if (is.null(name)) {
    name <- pedtools::famid(pedigree)
  }
  if (labs_spec == TRUE) {
    labs <- studbook_short(studbook) %>% filter(!is.na(name_spec)) %>%
      select(name_spec, ID) %>%
      deframe()
    starred  <- labs
    lwd_spec <- labs %>% set_names(., 0.7)
    lwd_def  <- setdiff(ped_ids, labs) %>% set_names(., 0.3)
    lwd      <- as.list(c(lwd_spec, lwd_def))
  } else {
    labs    <- NULL
    starred <- NULL
    lwd     <- 0.3
  }
  ped.palette  <- set_ped_fills(palette, studbook)
  deceased_ids <- deceased(studbook)
  plotTag(
    expr = plot(
      pedigree,
      title    = name,
      cex      = 0.4,
      deceased = deceased_ids,
      labs     = labs,
      starred  = starred,
      fill     = ped.palette,
      lwd      = lwd,
      col      = "black",
      pty      = "m",
      ...
    ),
    alt    = "pedigree-plot",
    width  = 900,
    height = 900
  )
}

#' Plot a Series of Pedigree Objects with Custom Formatting
#'
#' This function plots a list of pedigree objects (from `pedtools::ped()`) using custom formatting.
#'
#' @param pedigree_series A list of pedigree objects.
#' @param studbook A tibble containing metadata (e.g., `ID`, `Sex`, `Status`, etc.).
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A list of pedigree plots (base graphics).
#' @export
#'
#' @importFrom purrr imap
plot_ped_series <- function(pedigree_series, studbook, palette = NULL, labs_spec = FALSE) {
  if (is.null(palette)) {
    palette <- set_colors()
  }
  imap(pedigree_series, \(x, idx) plot_pedigree(pedigree = x,
                                                studbook = studbook,
                                                name     = idx,
                                                palette  = palette,
                                                labs_spec= labs_spec)
       )
}

#' Extract a pedigree of interest as its own object
#'
#' For many of our operations, it will be easier to work with one pedigree object of interest and ignore the pedigrees not currently represented in the breeding population. The function below helps you extract the pedigree of interest as its own object with a new name.
#'
#' @param pedigree_series A list of pedigree objects.
#' @param name The name of the pedigree object to extract
#' @param title The new name to apply to the extracted pedigree (optional)
#' @return A single pedigree object representing the living population
#' @export
#'
#' @importFrom purrr imap
#' @importFrom pedtools famid
#'
extract_pedigree <- function(pedigree_series, name, title = NULL) {
  pedigree    <- keep_at(pedigree_series, name)
  pedigree    <- pedigree[[1]]
  if (!is.null(title)) {
    name <- pedtools::famid(pedigree)
  }
  return(pedigree)
}

#' Create download a local copy of a pedigree plot as a png file
#'
#' @param pedigree A single pedigree object (from `pedtools::ped()`)
#' @param name A title for the plot (used as label)
#' @param studbook A tibble with `ID`, `Sex`, `Status`, and other metadata
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#'
#' @return An html file containing a rendered pedigree plot saved to a local subdirectory called `zoopop_plots`
#' @export
#'
#' @importFrom htmltools save_html
#' @importFrom pedtools famid
#'
pedsave <- function(pedigree, studbook, name = NULL, palette = NULL, labs_spec = FALSE) {
  if (is.null(palette)) {palette <- set_colors()}
  if (is.null(name)) {name <- pedtools::famid(pedigree)}
  ped.palette  <- set_ped_fills(palette, studbook)
  deceased_ids <- deceased(studbook)
  out_dir <- file.path("zoopop_plots")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }
  plot <- plot_pedigree(pedigree, studbook, name = name, palette = palette, labs_spec = labs_spec)
  htmltools::save_html(plot, paste0(out_dir, "/ped_plot_", name, ".html"))
}

#' Create download a local copy of a pedigree plot as a png file
#'
#' @param pedigree_series A list of pedigree objects.
#' @param studbook A tibble with `ID`, `Sex`, `Status`, and other metadata
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#'
#' @return An html file containing a rendered pedigree plot saved to a local subdirectory called `zoopop_plots`
#' @export
#'
#' @importFrom htmltools save_html
#' @importFrom pedtools famid
#'
pedsave_series <- function(pedigree_series, studbook, palette = NULL, labs_spec = FALSE) {
  if (is.null(palette)) {
    palette <- set_colors()
  }
  imap(pedigree_series, \(x, idx) pedsave(pedigree = x,
                                          studbook = studbook,
                                          name     = idx,
                                          palette  = palette,
                                          labs_spec= labs_spec)
  )
}

#' Create a data frame with basic metadata to attach to nodes and edges of a pedigree diagram
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A new dataframe to use for building edge and node dataframes with `DiagrammeR`
#' @export
#'
#' @importFrom dplyr mutate cur_group_id
#' @importFrom stringr str_glue
#'
studbook_nodes <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {
    palette <- set_colors()
  }
  colors         <- palette %>%
    set_names( ~gsub("sire", "m_d", .)) %>%
    set_names( ~gsub("dam", "f_d", .))
  studbook_nodes <- studbook_short(studbook) %>%
    mutate(label              = as.character(ID),
           label_spec_subject = if_else(
             is.na(name_spec),
             label,
             as.character(name_spec)
           )
    ) %>%
    subj_tooltip() %>%
    rename(tooltip_subject = tooltip) %>%
    loc_tooltip() %>%
    rename(tooltip_connector = tooltip_loc) %>%
    ped_attribute(colors[["u"]],
                  colors[["m"]],
                  colors[["f"]],
                  colors[["m"]],
                  colors[["f"]],
                  colors[["m_d"]],
                  colors[["f_d"]]
    ) %>%
    rename(node_fillcolor_subject = attribute) %>%
    ped_attribute("black",
                  "black",
                  "black",
                  colors[["emp"]],
                  colors[["emp"]],
                  "darkgrey",
                  "darkgrey"
    ) %>%
    rename(node_color_subject = attribute) %>%
    ped_attribute("diamond"       ,
                  "square"        ,
                  "circle"        ,
                  "square"        ,
                  "circle"        ,
                  "square"        ,
                  "circle"
    ) %>%
    rename(node_shape_subject  = attribute,
           color_connector     = colorLoc_birth) %>%
    ped_group() %>%
    select(
      label,
      label_spec_subject,
      node_fillcolor_subject,
      node_color_subject,
      node_shape_subject,
      color_connector,
      tooltip_subject,
      subject_group,
      tooltip_connector,
      edge_group
    ) %>%
    distinct()
  return(studbook_nodes)

}

#' Create a data frame with labels and unique numeric ids to use for building nodes and edges of a pedigree diagram
#'
#' @param pedigree A pedigree object
#' @return A new dataframe to use for building edge and node dataframes with `DiagrammeR`
#' @export
#'
#' @importFrom dplyr mutate cur_group_id
#' @importFrom stringr str_glue
#' @importFrom pedtools nonfounders parents
set_ped_paths <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  studbook_edges <- studbook_nodes(studbook = studbook,
                                   pedigree = pedigree,
                                   palette  = palette) %>%
    select(offspring  = label,
           edge_color = color_connector)
  offspring_ids    <- as.list(pedtools::nonfounders(pedigree))
  paths <- map(offspring_ids, \(x) as.list(parents(pedigree, x))) %>%
    set_names(offspring_ids) %>%
    enframe(name = "offspring", value = "parent") %>%
    unnest_wider(parent, names_sep = "_") %>%
    arrange(parent_1, parent_2, offspring) %>%
    rename(dad = parent_1, mom = parent_2) %>%
    left_join(studbook_edges, by = "offspring") %>%
    mutate(edge_color   = last(edge_color),
           .by = c(dad, mom)) %>%
    distinct() %>%
    mutate(parents_to = consecutive_id(dad, mom)) %>%
    mutate(offspring_from = max(parents_to) + consecutive_id(parents_to))
  return(paths)
}

#' Create a data frame with basic metadata attached to edges connecting individuals
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A new dataframe to use for building edge and node dataframes with `DiagrammeR`
#' @export
#'
#' @importFrom dplyr mutate cur_group_id
#' @importFrom stringr str_glue
#'
edges_parents <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  key <- studbook_nodes(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette
  ) %>%
    mutate(from = as.integer(label),
           .keep = "unused") %>%
    select(
      from,
      group     = subject_group,
      tooltip   = tooltip_subject
    )

  edges <- set_ped_paths(
                studbook = studbook,
                pedigree = pedigree,
                palette  = palette) %>%
    distinct(dad, mom, parents_to) %>%
    pivot_longer(c(dad, mom),
                 names_to  = "parent",
                 values_to = "from")  %>%
    distinct() %>%
    mutate(rel        = "parents",
           edge_color = case_match(parent,
                                   "dad" ~ palette[["m"]],
                                   "mom" ~ palette[["f"]]),
           minlen     = 0.0,
           weight     = 2.0,
           from       = as.integer(from)) %>%
    left_join(key, by = "from") %>%
    select(from,
           to = parents_to,
           rel,
           edge_color,
           group,
           tooltip,
           minlen,
           weight)
  return(edges)
}
#' Create a data frame with basic metadata attached to edges connecting individuals
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A new dataframe to use for building edge and node dataframes with `DiagrammeR`
#' @export
#'
#' @importFrom dplyr mutate cur_group_id
#' @importFrom stringr str_glue
#'
edges_connectors <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  key <- studbook_nodes(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette
  ) %>%
    mutate(offspring = as.character(label),
           .keep = "unused") %>%
    select(
      offspring,
      group     = edge_group,
      tooltip   = tooltip_connector
    )


  edges <- set_ped_paths(
                studbook = studbook,
                pedigree = pedigree,
                palette  = palette) %>%
    left_join(key, by = "offspring") %>%
    select(from = parents_to,
           to   = offspring_from,
           edge_color,
           group,
           tooltip) %>%
    group_by(from, to) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    distinct() %>%
    mutate(rel    = "connector",
           minlen = 2.5,
           weight = 0)
  return(edges)
}
#' Create a data frame with basic metadata attached to edges connecting individuals
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A new dataframe to use for building edge and node dataframes with `DiagrammeR`
#' @export
#'
#' @importFrom dplyr mutate cur_group_id
#' @importFrom stringr str_glue
#'
edges_offspring <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  key <- studbook_nodes(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette
  ) %>%
    mutate(to    = as.integer(label),
           .keep = "unused") %>%
    select(
      to,
      group     = subject_group,
      tooltip   = tooltip_subject
    )

  edges <- set_ped_paths(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette) %>%
    select(from = offspring_from,
           to   = offspring,
           edge_color) %>%
    distinct() %>%
    mutate(rel    = "offspring",
           minlen = 2.5,
           weight = 1.5,
           to     = as.integer(to)) %>%
    left_join(key, by = "to")
  return(edges)
}
#' Create a data frame with basic metadata attached to edges connecting individuals
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A new dataframe to use for building edge and node dataframes with `DiagrammeR`
#' @export
#'
#' @importFrom dplyr mutate cur_group_id
#' @importFrom stringr str_glue
#'
ped_edges <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  node_ids <- ped_ndf(studbook = studbook,
                      pedigree = pedigree,
                      palette  = palette) %>%
    select(id, node)
  offspring <- edges_offspring(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette)
  parents <- edges_parents(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette)
  connectors <- edges_connectors(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette)
  edges <- bind_rows(offspring, parents, connectors) %>%
    rename(color   = edge_color) %>%
    mutate(
      arrowhead = "none",
      arrowtail = "none",
      style     = "solid",
      penwidth  = 3,) %>%
    left_join(node_ids, by = join_by(from == node)) %>%
    mutate(from = id, .keep = "unused") %>%
    left_join(node_ids, by = join_by(to == node)) %>%
    mutate(to = id, .keep = "unused") %>%
    arrange(from, to)
  return(edges)
}


#' Create a data frame with basic metadata to attach to nodes
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A new dataframe to use for building edge and node dataframes with `DiagrammeR`
#' @export
#'
#' @importFrom dplyr mutate cur_group_id
#' @importFrom stringr str_glue
#'
ped_nodes <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  key_subjects   <- studbook_nodes(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette
  ) %>%
    mutate(node     = as.integer(label),
           label    = label_spec_subject,
           .keep = "unused") %>%
    select(
      node,
      label,
      group     = subject_group,
      color     = node_color_subject,
      fillcolor = node_fillcolor_subject,
      shape     = node_shape_subject,
      tooltip   = tooltip_subject
    )

  key_connectors   <- edges_connectors(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette
  ) %>%
    select(parents   = from,
           offspring = to,
           color     = edge_color,
           group,
           tooltip) %>%
    pivot_longer(c(parents, offspring),
                 names_to  = "type",
                 values_to = "node") %>%
    mutate(group = as.character(str_glue("connector_{type}"))) %>%
    distinct(node, color, group, tooltip) %>%
    arrange(node)

  offspring <- edges_offspring(
            studbook = studbook,
            pedigree = pedigree,
            palette  = palette) %>%
    select(connector = from, subject = to)

  nodes_long <- edges_parents(studbook = studbook,
                         pedigree = pedigree,
                         palette  = palette) %>%
    select(connector = to, subject = from) %>%
    bind_rows(offspring) %>%
    pivot_longer(
      everything(),
      names_to  = "type",
      values_to = "node"
    ) %>%
    arrange(node) %>%
    distinct()

  nodes_subjects <- nodes_long %>%
    filter(type == "subject") %>%
    mutate(fixedsize = FALSE,
           style     = "filled",
           width     = 1.5,
           height    = 1.5,
           fontsize  = 22) %>%
    left_join(key_subjects, by = "node")
  nodes_connectors <- nodes_long %>%
    filter(type == "connector") %>%
    left_join(key_connectors, by = "node") %>%
    mutate(shape     = "point",
           fixedsize = TRUE,
           width     = 0.02,
           height    = 0.02,
           style     = "invisible")
  nodes <- bind_rows(nodes_subjects, nodes_connectors) %>%
    arrange(node)


  return(nodes)
}

#' Create a node data frame to plot a pedigree using DiagrammeR
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A formatted node dataframe for graphing with `DiagrammeR`
#' @export
#'
#' @importFrom dplyr mutate cur_group_id
#' @importFrom DiagrammeR create_node_df
#'
ped_ndf <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}

  nodes <- ped_nodes(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette
  )

  ndf <- create_node_df(
    n         = length(unique(nodes$node)),
    node      = nodes$node     ,
    label     = nodes$label    ,
    style     = nodes$style    ,
    fontsize  = nodes$fontsize,
    fillcolor = nodes$fillcolor,
    color     = nodes$color    ,
    shape     = nodes$shape    ,
    fixedsize = nodes$fixedsize,
    width     = nodes$width    ,
    height    = nodes$height
  )
  return(ndf)
}

#' Create an edge data frame to plot a pedigree using DiagrammeR
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A formatted edge dataframe for graphing with `DiagrammeR`
#' @export
#'
#' @importFrom dplyr mutate cur_group_id
#' @importFrom DiagrammeR create_edge_df
#'
ped_edf <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  edges <- ped_edges(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette)
  edf <- create_edge_df(
    from      = edges$from     ,
    to        = edges$to       ,
    color     = edges$color    ,
    arrowhead = edges$arrowhead,
    arrowtail = edges$arrowtail,
    style     = edges$style    ,
    penwidth  = edges$penwidth
  )

  return(edf)
}

#' Create a pedigree graph using DiagrammeR
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A graphviz object rendered with `DiagrammeR`
#' @export
#'
#' @importFrom dplyr mutate cur_group_id
#' @importFrom DiagrammeR create_graph render_graph
#'
ped_graphv <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  edf <- ped_edf(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette)
  ndf <- ped_ndf(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette)
  graph <- create_graph(
    nodes_df      = ndf,
    edges_df      = edf,
    attr_theme    = "tb"
  )
  return(graph)
}
#' Create a pedigree vertex dataframe that will work with `igraph`
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A dataframe of vertices to use with `igraph`
#' @export
#'
#' @importFrom dplyr mutate
#'
igraph_vertices <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  vertices <- ped_ndf(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette
  ) %>%
    mutate(frame.color = color) %>%
    mutate(color        = if_else(is.na(fillcolor), color, fillcolor),
           shape        = case_when(shape == "point" ~ "circle", shape == "diamond" ~ "vrectangle", .default = shape),
           size         = if_else(style == "invisible", 1, 7),
           label.color  = if_else(str_ends(fillcolor, "FF"), "white", "black"),
           label.font   = 1,
           label.cex    = 0.25,
           label.family = "Helvetica") %>%
    select(
      id,
      vertex = node,
      label,
      label.color,
      label.font,
      label.cex,
      label.family,
      color,
      frame.color,
      shape,
      size
    )

  return(vertices)
}


#' Create a pedigree edge dataframe that will work with `igraph`
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A dataframe of edges to use with `igraph`
#' @export
#'
#' @importFrom dplyr mutate
#'
igraph_edges <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  edges <- ped_edges(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette
  ) %>%
    mutate(arrow.size = 0,
           curved     = 0,
           width      = 1) %>%
    select(
      from,
      to,
      color,
      lty = style,
      width,
      arrow.size,
      curved
    )

  return(edges)
}


#' Create a pedigree graph using `igraph`
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A graph object built with `igraph`
#' @export
#'
#' @importFrom dplyr mutate cur_group_id
#' @importFrom igraph graph_from_data_frame
#'
ped_igraph <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  edges <- igraph_edges(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette
  )
  vertices <- igraph_vertices(
    studbook = studbook,
    pedigree = pedigree,
    palette  = palette
  )

  graph <- graph_from_data_frame(edges, vertices = vertices)

  return(graph)
}

#' Create a pedigree node dataframe that works with `visNetwork`
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return A node dataframe that works with `visNetwork`
#' @export
#'
#' @importFrom dplyr mutate case_when select left_join if_else
#'
ped_visNodes <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  node_ids <- ped_ndf(studbook = studbook,
                      pedigree = pedigree,
                      palette  = palette) %>%
    select(id, node)

  nodes <- ped_nodes(studbook, pedigree) %>%
    left_join(node_ids, by = "node") %>%
    mutate(color = if_else(is.na(fillcolor), color, fillcolor)) %>%
    select(
      id,
      node,
      label,
      group,
      title = tooltip
    )
  return(nodes)
}

#' Create a pedigree edge dataframe that works with `visNetwork`
#'
#' @param studbook A studbook tibble
#' @param pedigree A pedigree object
#' @param palette An optional named list mapping individuals to colors by sex and
#'   status; if not provided, [set_colors()] is used.
#' @return An edge dataframe that works with `visNetwork`
#' @export
#'
#' @importFrom dplyr mutate case_when select
#' @importFrom stringr str_starts
#'
ped_visEdges <- function(studbook, pedigree, palette = NULL) {
  if (is.null(palette)) {palette <- set_colors()}
  edges <- ped_edges(studbook = studbook,
                     pedigree = pedigree,
                     palette  = palette) %>%
    mutate(width = 1.5,
           group = case_when(rel == "connector" ~ rel,
                             rel == "parents" & str_starts(group, "female") ~ "mom",
                             rel == "parents" & str_starts(group, "male")   ~ "dad",
                             rel == "offspring" ~ group,
                             .default = group)) %>%
    select(from,
           to,
           color,
           group,
           width,
           title = tooltip)
  return(edges)
}

#' Add formatted groups to a `visNetwork` pedigree object
#'
#' @param visNetwork A `visNetwork` object
#' @return A `visNetwork` object with group-based attributes
#' @export
#'
#' @importFrom visNetwork visGroups
#'
ped_visGroups <- function(visNetwork) {
  image_path <- "https://github.com/Rich-Molecular-Health-Lab/zoopop/tree/main/inst/icons/"
  network <- visNetwork %>% visGroups(
      groupname = "connector",
      shape     = "circularImage",
      image     = paste0(image_path, "connector.png"),
      size      = 5
    ) %>%
  visGroups(
    groupname = "female_deceased",
    shape     = "circularImage",
    image     = paste0(image_path, "female_deceased.png"),
    size      = 35
    ) %>%
    visGroups(
      groupname = "female_excluded",
      shape     = "circularImage",
      image     = paste0(image_path, "female_excluded.png"),
      size      = 35
    ) %>%
    visGroups(
      groupname = "female_included",
      shape     = "circularImage",
      image     = paste0(image_path, "female_included.png"),
      size      = 40
    ) %>%
    visGroups(
      groupname = "male_deceased",
      shape     = "image",
      image     = paste0(image_path, "male_deceased.png"),
      size      = 35
    ) %>%
    visGroups(
      groupname = "male_excluded",
      shape     = "image",
      image     = paste0(image_path, "male_excluded.png"),
      size      = 35
    ) %>%
    visGroups(
      groupname = "male_included",
      shape     = "image",
      image     = paste0(image_path, "male_included.png"),
      size      = 40
    )

  return(network)
}

