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

#' Generate edge list for pedigree visNetwork plot
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
pedigree_edges <- function(pedigree, studbook) {
  pairs <- pedigree_pairs(pedigree, studbook) %>%
    arrange(pid)
  sire  <- pull(pairs, fid, pid)
  dam   <- pull(pairs, mid, pid)
  pairs.edf <- pairs  %>%
    pivot_longer(cols = c("fid", "mid"), values_to = "from", names_to = NULL) %>%
    arrange(from, pid) %>%
    select(from, to = pid, color) %>%
    mutate(dashes = TRUE,
           shadow = FALSE,
           width  = 0.5,
           arrows = "to") %>%
    distinct()
  combined         <- c(sire, dam)
  combined_grouped <- split(combined, names(combined)) %>% map(unlist)
  offspring.edf    <- combined_grouped %>%
    imap(function(x, idx) {
      list(idx, as.list(commonDescendants(pedigree, ids = x, maxGen = 2)))
    }) %>%
    enframe(name = NULL, value = "id") %>%
    unnest_wider(id, names_sep = "_") %>%
    unnest_longer(id_2, values_to = "id") %>%
    select(from = id_1, to = id) %>%
    arrange(from, to) %>%
    distinct() %>%
    left_join(select(pairs, from = pid, color), by = join_by(from)) %>%
    mutate(dashes = FALSE,
           shadow = TRUE,
           width  = 0.7,
           arrows = NULL)

  edges <- bind_rows(pairs.edf, offspring.edf)
  return(edges)
}

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

#' Generate an edge dataframe from the pedigree that functions within `DiagrammeR`
#'
#' @param pedigree A pedigree object
#' @param studbook A studbook tibble
#' @return A formatted edge dataframe for `DiagrammeR`
#' @export
#'
#' @importFrom dplyr distinct arrange mutate across select consecutive_id if_else rename
#' @importFrom tidyr pivot_longer
#' @importFrom DiagrammeR create_edge_df
ped_edf <- function(pedigree, studbook) {
  edge_trios    <- pedigree_births(pedigree, studbook) %>%
    mutate(tooltip_pair = as.character(str_glue("Pair M {dad} + F {mom} at {iconLoc}{Loc}")),
           tooltip_sibs = as.character(str_glue("Offspring of M {dad} + F {mom} (born at {iconLoc}{Loc})")),
           tooltip_offs = tooltip_sibs
    ) %>%
    select(
      sibs_to_offspring = offspring,
      dad_to_pair       = dad,
      mom_to_pair       = mom,
      pair,
      sibs,
      tooltip_pair,
      tooltip_sibs,
      tooltip_offs,
      color = colorLoc
    ) %>%
    distinct() %>%
    mutate(across(c(sibs_to_offspring:sibs), ~as.integer(.))) %>%
    arrange(pair, sibs)

  edges_pairs <- select(
    edge_trios,
    dad_to_pair,
    mom_to_pair,
    to      = pair,
    color
  ) %>%
    pivot_longer(c("dad_to_pair", "mom_to_pair"), names_to = "edge_type", values_to = "from") %>%
    mutate(edge_type = "parents") %>%
    distinct() %>%
    arrange(to, from)

  edges_offspring <- select(
    edge_trios,
    to      = sibs_to_offspring,
    from    = sibs,
    color
  ) %>%
    mutate(edge_type = "offspring") %>%
    distinct() %>%
    arrange(to, from)

  edges_connectors <- select(
    edge_trios,
    to      = sibs,
    from    = pair,
    color
  ) %>%
    mutate(edge_type = "connector") %>%
    distinct() %>%
    arrange(to, from)

  edges <- bind_rows(edges_pairs, edges_offspring, edges_connectors) %>%
    distinct() %>%
    arrange(from, to) %>%
    mutate(id = row_number(),
           arrowtail    = "none",
           arrowhead    = "none") %>%
    select(
      id,
      from,
      to,
      color,
      arrowtail,
      arrowhead,
      rel = edge_type
    )

return(edges)

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






