nodes_2dgm <- function(subnuc, studbook, pedigree) {
  colors <- set_colors()
  cols.light <- lighten_palette(colors, "CC")
  nodes <- subnucs_nodes(subnuc   = subnuc,
                         studbook = studbook,
                         pedigree = pedigree) %>%
    mutate(
      shape = case_when(
        str_starts(group, "female")         ~ "circle",
        str_starts(group, "male")           ~ "square",
        str_starts(group, "undetermined")   ~ "diamond",
        str_starts(group, "connector")      ~ "point"
        ),
      fillcolor = case_when(
        group %in% c("female_excluded"    , "female_included") ~ colors[["f"]],
        group %in% c("male_excluded"      , "male_included"  ) ~ colors[["m"]],
        group %in% c("female_hypothetical", "female_deceased") ~ cols.light[["f"]],
        group %in% c("male_hypothetical"  , "male_deceased"  ) ~ cols.light[["m"]],
        group == "undetermined"                                ~ colors[["u"]],
        group ==  "connector"                                  ~ "gray"
      ),
      color = case_when(
        str_ends(group, "included")           ~ "black",
        str_ends(group, "excluded")           ~ colors[["emp"]],
        str_ends(group, "deceased")           ~ "gray",
        .default = "black"
      ),
      height = if_else(
        group == "connector", 0.02, 0.7
      ),
      penwidth = 2.0
    ) %>%
    mutate(width = height) %>%
    select(from = id,
           label,
           level,
           group,
           color,
           fillcolor,
           shape,
           penwidth,
           height,
           width)
  return(nodes)
}

edges_2dgm <- function(subnuc, studbook, pedigree) {
  nodes <- nodes_2dgm(subnuc   = subnuc,
                      studbook = studbook,
                      pedigree = pedigree)
  ndf_from   <- create_node_df(
    n     = length(pull(nodes, from)),
    from  = pull(nodes, from)
  )
  ndf_to   <- create_node_df(
    n     = length(pull(nodes, from)),
    to    = pull(nodes, from)
  )
  edges <- subnucs_edges(subnuc = subnuc) %>%
    select(
      from,
      to
    ) %>%
    left_join(select(ndf_from, id, from), by = join_by(from)) %>%
    mutate(from = id, .keep = "unused") %>%
    left_join(select(ndf_to, id, to), by = join_by(to)) %>%
    mutate(to = id, .keep = "unused")
  return(edges)
}

map_nodes_2dgm <- function(studbook, pedigree) {
  subnucs <- subnucs_nested(pedigree = pedigree)
  nodes <- map_depth(subnucs, 1, \(x) nodes_2dgm(x, studbook = studbook, pedigree = pedigree))
  return(nodes)
}

map_edges_2dgm <- function(studbook, pedigree) {
  subnucs <- subnucs_nested(pedigree = pedigree)
  edges <- map_depth(subnucs, 1, \(x) edges_2dgm(x, studbook = studbook, pedigree = pedigree))
  return(edges)
}

subnucs_dgm_data <- function(studbook, pedigree) {
  nodes <- map_nodes_2dgm(studbook = studbook, pedigree = pedigree)
  edges <- map_edges_2dgm(studbook = studbook, pedigree = pedigree)

  merged <- map2(nodes, edges, \(x, y) list(nodes = x, edges = y))
  return(merged)
}

diagrammeSubnucs_base <- function(nodes, edges) {
  ndf <- create_node_df(
    n         = nrow(nodes),
    label     = c(pull(nodes, label  )) ,
    level     = c(pull(nodes, level  )) ,
    group     = c(pull(nodes, group  )) ,
    fillcolor = c(pull(nodes, fillcolor  )) ,
    color     = c(pull(nodes, color  )) ,
    shape     = c(pull(nodes, shape  )),
    penwidth  = c(pull(nodes, penwidth)),
    height    = c(pull(nodes, height  )),
    width     = c(pull(nodes, width  ))
  )
  edf <- create_edge_df(
    from = c(pull(edges, from)),
    to   = c(pull(edges, to))
  )
  graph <- create_graph(
                        nodes_df      = ndf,
                        edges_df      = edf,
                        directed      = TRUE,
                        graph_name    = NULL,
                        attr_theme    = "tb",
                        display_msgs  = FALSE
                        )
  return(graph)
}

diagrammeSubnucs <- function(studbook, pedigree) {
  data    <- subnucs_dgm_data(studbook = studbook, pedigree = pedigree)
  graph   <- map_depth(data, 1, \(x) as.list(diagrammeSubnucs_base(nodes = x[["nodes"]], edges = x[["edges"]]))) %>%
    map_depth(., 1, \(x) as.list(render_graph(x, layout = "tree")))
  return(graph)
}

merge_subnucs <- function(studbook, pedigree) {
  data     <- subnucs_dgm_data(studbook = studbook, pedigree = pedigree)
  graphs   <- map_depth(data, 1, \(x) as.list(diagrammeSubnucs_base(nodes = x[["nodes"]], edges = x[["edges"]])))
  graph    <- reduce(graphs, combine_graphs)
  return(graph)
}
