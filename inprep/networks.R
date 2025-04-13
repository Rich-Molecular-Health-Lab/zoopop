ped_ids <- function(pedigree) {
  ped_members  <- as.list(pedigree[["ID"]])
  internal_ids <- map(ped_members, \(x) internalID(pedigree, x)) %>%
    set_names(ped_members) %>%
    enframe("label", "id") %>%
    mutate(across(everything(), ~as.numeric(.))) %>%
    select(id_subject    = id,
           label_subject = label)
  return(internal_ids)
}

subject_levels <- function(generation) {
  level <- 1 + (generation - 1) * 3
}

ped_levels <- function(pedigree) {
  as.list(generations(pedigree, what = "indiv")) %>%
    enframe("label_subject", "generation") %>%
    mutate(across(everything(), ~as.numeric(.))) %>%
    arrange(generation) %>%
    mutate(level_subject = 1 + (generation - 1) * 3) %>%
    left_join(ped_ids(pedigree = pedigree)) %>%
    select(id_subject,
           label_subject,
           level_subject) %>%
    arrange(level_subject, id_subject)
}

ped_subnucs <- function(pedigree) {
  subnucs <- subnucs(pedigree) %>% map_depth(., 1, \(x) unclass(x)) %>%
    enframe(name = "id_subnuc") %>%
    mutate(id_subnuc = as.numeric(id_subnuc))
  return(subnucs)
}

ped_families <- function(pedigree) {
  internal_ids <- ped_ids(pedigree = pedigree)
  subnucs <- ped_subnucs(pedigree = pedigree)
  families <- subnucs %>%
    mutate(type = "connector", rel = "parents") %>%
    bind_rows(subnucs) %>%
    mutate(type = replace_na(type, "connector"), rel = replace_na(rel, "offspring")) %>%
    arrange(id_subnuc) %>%
    mutate(node = max(internal_ids$id) + row_number())
  return(families)
}

edges_connectors <- function(pedigree) {
  edges <- ped_families(pedigree = pedigree)  %>%
    select(node, rel, id_subnuc) %>%
    pivot_wider(names_from  = "rel",
                values_from = "node") %>%
    select(id_subnuc,
           id_from = parents,
           id_to   = offspring) %>%
    mutate(group = "connector",
           color = pedEdge_colors("connector"))
  return(edges)
}

edges_offspring <- function(pedigree) {
  edges <- ped_families(pedigree = pedigree) %>%
    filter(rel == "offspring") %>%
    select(id_from = node,
           value,
           id_subnuc) %>%
    hoist(value, id_to = c("children")) %>%
    select(id_subnuc,
           id_from,
           id_to) %>%
    unnest_longer(id_to) %>%
    mutate(group = "offspring",
           color = pedEdge_colors("offspring"))
  return(edges)
}

edges_dads <- function(pedigree) {
  edges <- ped_families(pedigree = pedigree) %>%
    filter(rel == "parents") %>%
    select(id_to = node,
           value,
           id_subnuc) %>%
    hoist(value, id_from = c("father")) %>%
    select(id_subnuc,
           id_from,
           id_to) %>%
    mutate(group = "dads",
           color = pedEdge_colors("dads"))
  return(edges)
}

edges_moms <- function(pedigree) {
  edges <- ped_families(pedigree = pedigree) %>%
    filter(rel == "parents") %>%
    select(id_to = node,
           value,
           id_subnuc) %>%
    hoist(value, id_from = c("mother")) %>%
    select(id_subnuc,
           id_from,
           id_to) %>%
    mutate(group = "moms",
           color = pedEdge_colors("moms"))
  return(edges)
}

pedEdge_colors <- function(group) {
  colors <- set_colors()

  edge_colors <- list(
    moms       = colors[["f"]],
    dads       = colors[["m"]],
    connectors = "#ABB6B4FF",
    offspring  = "#ABB6B4FF"
  ) %>%
    map_depth(., 1, ~ gsub("FF", "99", .x))

  color  <- pluck(edge_colors, group)
  return(color)
}

ped_edges <- function(pedigree) {
  connectors <- edges_connectors(pedigree = pedigree)
  offspring  <- edges_offspring(pedigree = pedigree)
  dads       <- edges_dads(pedigree = pedigree)
  moms       <- edges_moms(pedigree = pedigree)

  edges <- bind_rows(
    connectors,
    offspring ,
    dads      ,
    moms
  ) %>%
    arrange(id_subnuc, id_from, id_to) %>%
    distinct() %>%
    mutate(id_edge = row_number()) %>%
    select(id_edge,
           id_subnuc,
           group,
           color,
           from = id_from,
           to   = id_to)
}

nodes_subjects <- function(studbook, pedigree) {
  nodes <- ped_levels(pedigree = pedigree) %>%
    select(id_node,
           label_node,
           level_node = level) %>%
    distinct() %>%
    left_join(studbook_short(studbook = studbook), by = join_by(label_node == ID)) %>%
    mutate(title = case_when(
      !is.na(name_spec) ~ as.character(str_glue("{name_spec} ({label_node} {Sex}): Currently at {Institution_last} (Age {age_last})")),
      exclude == "deceased" ~ as.character(
        str_glue("{label_node} ({Sex}): Deceased at {Institution_last} (Age {age_last})")),
      exclude == "hypothetical" ~ as.character(
        str_glue("{label_node} ({Sex}): Hypothetical ID created to fill pedigree gap")),
      exclude == "age" ~ as.character(
        str_glue("{label_node} ({Sex}): Currently at {Institution_last} (Age {age_last})\nExcluded due to age")),
      exclude == "behavior" ~ as.character(
        str_glue("{label_node} ({Sex}): Currently at {Institution_last} (Age {age_last})\nExcluded due to behavior")),
      exclude == "n" ~ as.character(
        str_glue("{label_node} ({Sex}): Currently at {Institution_last} (Age {age_last})\nIncluded in breeding population"))
    ),
    group = case_when(
      Sex == "M" & exclude == "n"                    ~"male_included",
      Sex == "F" & exclude == "n"                    ~"female_included",
      Sex == "M" & exclude == "deceased"             ~"male_deceased",
      Sex == "F" & exclude == "deceased"             ~"female_deceased",
      Sex == "M" & exclude %in% c("age", "behavior") ~"male_excluded",
      Sex == "F" & exclude %in% c("age", "behavior") ~"female_excluded",
      Sex == "M" & exclude == "hypothetical"         ~"male_hypothetical",
      Sex == "F" & exclude == "hypothetical"         ~"female_hypothetical",
      Sex == "U" ~"undetermined"
    )
    ) %>%
    select(
      id_node,
      level_node,
      label_node,
      group,
      title
    )
  return(nodes)
}

connectors_parents <- function(pedigree) {
  connectors <- ped_edges(pedigree = pedigree) %>%
    filter(group %in% c("moms", "dads")) %>%
    select(id_subnuc,
           group,
           id_subject   = from,
           id_connector = to) %>%
    left_join(ped_levels(pedigree = pedigree), by = join_by(id_subject == id_node)) %>%
    select(id_subnuc,
           group,
           id_subject,
           id_connector,
           level) %>%
    group_by(id_subnuc, id_connector) %>%
    summarize(level_connector = max(level) + 1) %>%
    ungroup()
  return(connectors)
}

connectors_locations <- function(studbook, pedigree) {
  ped_levels(pedigree = pedigree) %>%
    select(id_node,
           label_node,
           level_node = level) %>%
    distinct() %>%
    left_join(studbook_short(studbook = studbook), by = join_by(label_node == ID)) %>%
    mutate(title = as.character(str_glue(
      "Births at {Institution_birth}, {State_Province_birth} {iconLoc_birth}"
    ))) %>%
    arrange(Date_birth) %>%
    select(id_subject    = id_node,
           level_subject = level_node,
           title)
}

connectors_offspring <- function(studbook, pedigree) {
  connectors <- edges_offspring(pedigree = pedigree) %>%
    select(id_subject   = id_to,
           id_connector = id_from) %>%
    left_join(connectors_locations(studbook = studbook,
                                   pedigree = pedigree),
              by = "id_subject") %>%
    select(level_subject,
           title,
           id_to  = id_connector) %>%
    arrange(id_to, level_subject) %>%
    group_by(id_to) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    left_join(edges_connectors(pedigree = pedigree), by = "id_to") %>%
    left_join(connectors_parents(pedigree = pedigree),
              by = join_by(id_from == id_connector, id_subnuc)) %>%
    select(id_subnuc,
           id_from,
           level_from      = level_connector,
           id_connector    = id_to,
           level_offspring = level_subject,
           title) %>%
    distinct() %>%
    rowwise() %>%
    mutate(level_connector = level_offspring - 1) %>%
    select(id_node    = id_connector,
           level_node = level_connector,
           title) %>%
    mutate(group = "connector")
  return(connectors)
}

ped_nodes <- function(studbook, pedigree) {
  nodes <- connectors_parents(pedigree = pedigree) %>%
    select(id_node    = id_connector,
           level_node = level_connector) %>%
    mutate(group = "connector") %>%
    bind_rows(connectors_offspring(studbook = studbook,
                                   pedigree = pedigree)) %>%
    bind_rows(nodes_subjects(studbook = studbook,
                             pedigree = pedigree)) %>%
    arrange(id_node) %>%
    select(
      id    = id_node,
      label = label_node,
      level = level_node,
      group,
      title
    )
  return(nodes)
}

ped_ndf <- function(studbook, pedigree) {
  nodes <- ped_nodes(studbook = studbook,
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

ped_edf <- function(pedigree) {
  edges <- ped_edges(pedigree = pedigree)
  edf <- create_edge_df(
    from  = edges$from,
    to    = edges$to,
    group = edges$group
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
    pedNode_visGroup(graph = graph, groupname = "female_deceased"    ) %>%
    pedNode_visGroup(               groupname = "male_deceased"      ) %>%
    pedNode_visGroup(               groupname = "female_included"    ) %>%
    pedNode_visGroup(               groupname = "male_included"      ) %>%
    pedNode_visGroup(               groupname = "female_excluded"    ) %>%
    pedNode_visGroup(               groupname = "male_excluded"      ) %>%
    pedNode_visGroup(               groupname = "female_hypothetical") %>%
    pedNode_visGroup(               groupname = "male_hypothetical"  ) %>%
    pedNode_visGroup(               groupname = "undetermined"       ) %>%
    pedNode_visGroup(               groupname = "connector"          )
}

ped_visNet <- function(studbook, pedigree) {
  nodes <- ped_nodes(studbook = studbook,
                     pedigree = pedigree)
  edges <- ped_edges(pedigree = pedigree) %>%
    select(from, to, color) %>%
    distinct()
  graph <- visNetwork(nodes, edges) %>%
    visEdges(width = 1.5) %>%
    ped_visGroups() %>%
    visInteraction(tooltipDelay = 7)
  return(graph)
}

