subnucs_toD3 <- function(studbook, pedigree) {
  data     <- subnucs_dgm_data(studbook = studbook, pedigree = pedigree)
  graphs   <- map_depth(data, 1, \(x) as.list(diagrammeSubnucs_base(nodes = x[["nodes"]], edges = x[["edges"]]))) %>%
    map_depth(., 1, \(x) igraph_to_networkD3(x, "group", what = "both"))
  return(graphs)
}

subnucs_D3Links <- function(subnuc) {
  edges <- subnucs_edges(subnuc = subnuc) %>%
    mutate(Value = case_when(
      rel %in% c("mom", "dad") ~1,
      rel == "hub"             ~3,
      rel == "kid"             ~2
    )) %>%
    select(
      Source = from,
      Target = to,
      Value
    )
  return(edges)
}

subnucs_D3Nodes <- function(subnuc, studbook, pedigree) {
  nodes <- subnucs_nodes(subnuc   = subnuc,
                         studbook = studbook,
                         pedigree = pedigree) %>%
    mutate(Nodesize = if_else(
      group == "connector", 0.02, 0.7
    )) %>%
    select(NodeID = id,
           Group  = group,
           Nodesize)
  return(nodes)
}

map_D3Nodes <- function(studbook, pedigree) {
  subnucs <- subnucs_nested(pedigree = pedigree)
  nodes <- map_depth(subnucs, 1, \(x) subnucs_D3Nodes(x, studbook = studbook, pedigree = pedigree))
  return(nodes)
}

map_D3Links <- function(pedigree) {
  subnucs <- subnucs_nested(pedigree = pedigree)
  edges <- map_depth(subnucs, 1, \(x) subnucs_D3Links(x))
  return(edges)
}

subnucs_D3data <- function(studbook, pedigree) {
  nodes <- map_D3Nodes(studbook = studbook, pedigree = pedigree)
  edges <- map_D3Links(pedigree = pedigree)

  merged <- map2(nodes, edges, \(x, y) list(Nodes = x, Links = y))
  return(merged)
}

subnucs_D3base <- function(subnucs) {
  graph <- forceNetwork(
    Links        = as.data.frame(subnucs[["Links"]]),
    Nodes        = as.data.frame(subnucs[["Nodes"]]),
    Source       = "Source",
    Target       = "Target",
    Value        = "Value",
    NodeID       = "NodeID",
    Nodesize     = "Nodesize",
    Group        = "Group",
    linkDistance = JS("function(d){return d.Value * 10}"),
    opacity      = 0.6,
    zoom         = TRUE
  )
}

subnucs_D3 <- function(studbook, pedigree) {
  data    <- subnucs_D3data(studbook = studbook, pedigree = pedigree)
  graphs   <- map_depth(data, 1, \(x) as.list(subnucs_D3base(x)))
  return(graphs)
}



