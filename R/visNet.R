
ped_visNodes <- function(studbook, pedigree) {
  subjects <- nodes_subjects(studbook = studbook,
                             pedigree = pedigree) %>%
    select(id,
           label,
           group,
           level = generation,
           title
    )
  subnucs  <- nodes_subnucs(pedigree = pedigree) %>%
    select(id,
           group,
           level = generation,
           value = n_kids)
  nodes <- bind_rows(subjects, subnucs) %>%
    arrange(id)
  return(nodes)
}

ped_ndf <- function(studbook, pedigree) {
  nodes <- ped_visNodes(studbook = studbook,
                        pedigree = pedigree)
  ndf <- create_node_df(
    n     = max(nodes$id),
    id    = nodes$id,
    label = nodes$label,
    level = nodes$level,
    group = nodes$group,
    title = nodes$title
  )
  return(ndf)
}

ped_visEdges <- function(studbook, pedigree) {
  edges <- ped_edges(studbook = studbook,
                     pedigree = pedigree) %>%
    select(from,
           to,
           color,
           rel) %>%
    mutate(width      = 1.5)
  return(edges)
}


ped_edf <- function(studbook, pedigree) {
  edges <- ped_visEdges(studbook = studbook,
                     pedigree = pedigree)
  edf <- create_edge_df(
    from  = edges$from,
    to    = edges$to,
    color = edges$color,
    width = edges$width,
    rel   = edges$rel
  )
  return(edf)
}

pedNode_visGroup <- function(graph, groupname) {
  path <- "https://rich-molecular-health-lab.github.io/zoopop/inst/icons/"
  file <- paste0(path, groupname, ".png")
  if (groupname == "connector") { size <- 5 } else { size <- 40 }
  visGroups(graph     = graph,
            groupname = groupname,
            shape     = "image",
            image     = file,
            size      = size)
}

ped_visGroups <- function(graph) {
  pedNode_visGroup(graph = graph,   groupname = "female_deceased"    ) %>%
    pedNode_visGroup(               groupname = "male_deceased"      ) %>%
    pedNode_visGroup(               groupname = "female_included"    ) %>%
    pedNode_visGroup(               groupname = "male_included"      ) %>%
    pedNode_visGroup(               groupname = "female_excluded"    ) %>%
    pedNode_visGroup(               groupname = "male_excluded"      ) %>%
    pedNode_visGroup(               groupname = "female_hypothetical") %>%
    pedNode_visGroup(               groupname = "male_hypothetical"  ) %>%
    pedNode_visGroup(               groupname = "undetermined"       ) %>%
    pedNode_visGroup(               groupname = "offspring"          ) %>%
    pedNode_visGroup(               groupname = "parents"            )
}

ped_visNet <- function(studbook, pedigree) {
  nodes <- ped_visNodes(
    studbook = studbook,
    pedigree = pedigree
    ) %>%
    mutate(n = max(id))
  edges <- ped_visEdges(
    studbook = studbook,
    pedigree = pedigree
    )
  graph <- visNetwork(nodes = nodes, edges = edges) %>%
    ped_visGroups() %>%
    visInteraction(tooltipDelay = 7) %>%
    visIgraphLayout(layout = "layout_with_sugiyama")
  return(graph)
}

