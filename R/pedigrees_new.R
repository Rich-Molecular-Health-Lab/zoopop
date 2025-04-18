ids_ped <- function(pedigree) {
  ped_members  <- as.list(pedigree[["ID"]])
  internal_ids <- map(ped_members, \(x) internalID(pedigree, x)) %>%
    set_names(ped_members) %>%
    enframe("label", "id") %>%
    mutate(across(everything(), ~as.numeric(.))) %>%
    select(id,
           label)
  return(internal_ids)
}


levels_ped <- function(pedigree) {
  as.list(generations(pedigree, what = "indiv")) %>%
    enframe("label", "generation") %>%
    mutate(across(everything(), ~as.numeric(.))) %>%
    arrange(generation) %>%
    left_join(ids_ped(pedigree = pedigree)) %>%
    select(id,
           label,
           generation) %>%
    arrange(generation, id)
}

nodes_ped <- function(studbook, pedigree) {
  colors     <- set_colors()
  cols.light <- lighten_palette(colors, "CC")
  levels     <- levels_ped(pedigree = pedigree)
  peels <-  peelingOrder(pedigree) %>%
    map_depth(1, \(x) unclass(x)) %>%
    set_names(seq_along(.)) %>%
    map_depth(1, \(x) assign_in(x, "subnuc", list(father = x$father, mother = x$mother, children = x$children))) %>%
    map_depth(1, \(x) keep_at(x, c("link", "subnuc"))) %>%
    map_depth(1, \(x) modify_in(x, "subnuc", \(y) enframe(y, name = "type", value = "id"))) %>%
    map_depth(1, \(x) modify_in(x, "subnuc", \(y) unnest(y, id))) %>%
    map_depth(1, \(x) modify_in(x, "subnuc", \(y) mutate(y,
                                                         type = case_match(type,
                                                                           "mother"   ~"mom",
                                                                           "father"   ~"dad",
                                                                           "children" ~"kid",
                                                                           .default = type)))) %>%
    enframe(name = "famid") %>%
    mutate(famid = as.integer(famid)) %>%
    hoist(value, link = c("link")) %>%
    unnest(value) %>%
    unnest(value) %>%
    left_join(levels, by = "id") %>%
    left_join(studbook_short(studbook = studbook), by = join_by(label == ID))  %>%
    mutate(label_spec = if_else(
      is.na(name_spec), as.character(label),
      as.character(str_glue("{name_spec} ({as.character(label)})"))),
      group = case_when(
        Sex == "M" & exclude == "n"                    ~"male_included"      ,
        Sex == "F" & exclude == "n"                    ~"female_included"    ,
        Sex == "M" & exclude == "deceased"             ~"male_deceased"      ,
        Sex == "F" & exclude == "deceased"             ~"female_deceased"    ,
        Sex == "M" & exclude %in% c("age", "behavior") ~"male_excluded"      ,
        Sex == "F" & exclude %in% c("age", "behavior") ~"female_excluded"    ,
        Sex == "M" & exclude == "hypothetical"         ~"male_hypothetical"  ,
        Sex == "F" & exclude == "hypothetical"         ~"female_hypothetical",
        Sex == "U" ~"undetermined"
      )) %>%
    mutate(
      tip_breed = case_when(
        exclude == "hypothetical" ~ "Hypothetical individual to fill missing data",
        exclude == "behavior"     ~ "Living but excluded from breeding (behavior)",
        exclude == "age"          ~ "Living but excluded from breeding (due to age)",
        exclude == "n"            ~ "Living and included for breeding",
        exclude == "deceased"     ~ "Deceased (so excluded from breeding)"
      ),
      tip_id       = as.character(str_glue("{Sex} - {label_spec}")),
      tip_last = as.character(
        str_glue("age {age_last} at {Institution_last} {iconLoc_last} ({State_Province_last})")
      ),
      tip_birth = as.character(
        str_glue("Born {Year_birth} at {Institution_birth} {iconLoc_birth} ({State_Province_birth})")
      ),
      tip_connector = as.character(
        str_glue("{Institution_birth} {iconLoc_birth} ({State_Province_birth})")
      ),
      color_connector = colorLoc_last,
      label_connector = Loc_last
    ) %>%
    mutate(
      tip_status = if_else(
        exclude == "deceased",
        as.character(str_glue("Death - {tip_last}")),
        as.character(str_glue("Currently {tip_last}"))
      )
    ) %>%
    mutate(
      color = case_when(
        group %in% c("female_excluded"    , "female_included") ~ colors[["f"]],
        group %in% c("male_excluded"      , "male_included"  ) ~ colors[["m"]],
        group %in% c("female_hypothetical", "female_deceased") ~ cols.light[["f"]],
        group %in% c("male_hypothetical"  , "male_deceased"  ) ~ cols.light[["m"]],
        group == "undetermined"                                ~ colors[["u"]]
      ),
      shape = case_when(
        str_starts(group, "male")         ~"square",
        str_starts(group, "female")       ~"circle",
        str_starts(group, "undetermined") ~"vrectangle"
      ),
      frame.color = case_when(
        str_ends(group, "included") ~"black",
        str_ends(group, "excluded") ~colors[["emp"]],
        str_ends(group, "deceased") | str_ends(group, "hypothetical") ~"gray"
      ),
      size      = 7,
      label.cex = 0.25,
      value     = 2,
      title = as.character(str_glue(
        "<h4>{tip_id}</h4><p>{tip_breed}</p><p>{tip_birth}</p><p>{tip_status}</p>"
      ))
    ) %>%
    rename(id_stud = label) %>%
    select(
      famid,
      link,
      id,
      id_stud,
      label = label_spec,
      generation,
      group,
      type,
      value,
      color,
      shape,
      frame.color,
      size,
      label.cex,
      color_connector,
      label_connector,
      title,
      starts_with("tip_")
    ) %>%
    filter(!is.na(id)) %>%
    arrange(generation, id_stud) %>%
    mutate(famid = as.character(famid)) %>%
    mutate(subnuc_id = fct_inorder(famid)) %>%
    mutate(subnuc_id = fct_relabel(subnuc_id, ~as.character(seq_along(.)))) %>%
    mutate(famid = as.integer(subnuc_id), .keep = "unused") %>%
    arrange(famid, id)

  connectors_children <- peels %>%
    filter(type == "kid") %>%
    slice_tail(n = 1, by = famid) %>%
    mutate(type        = "children",
           group       = "hub",
           value       = 3,
           label       = label_connector,
           id_stud     = NA,
           id          = NA,
           size        = 1,
           shape       = "circle",
           fillcolor   = "gray",
           frame.color = "gray",
           title       = tip_connector) %>%
    select(
      famid,
      link,
      id,
      id_stud,
      label,
      generation,
      group,
      type,
      value,
      color,
      shape,
      frame.color,
      size,
      label.cex,
      title
    )

  connectors <- peels %>%
    filter(type == "mom") %>%
    mutate(type = "parents",
           group       = "hub",
           value       = 1,
           label       = NA,
           id_stud     = NA,
           id          = NA,
           size        = 1,
           shape       = "circle",
           fillcolor   = "gray",
           frame.color = "gray",
           title       = NA) %>%
    select(
      famid,
      link,
      id,
      id_stud,
      label,
      generation,
      group,
      type,
      value,
      color,
      shape,
      frame.color,
      size,
      label.cex,
      title
    ) %>%
    bind_rows(connectors_children) %>%
    arrange(famid) %>%
    group_by(famid) %>%
    fill(label, title, .direction = "downup") %>%
    ungroup()


  nodes <- peels %>%
    select(
      famid,
      link,
      id,
      id_stud,
      label,
      generation,
      group,
      type,
      value,
      color,
      shape,
      frame.color,
      size,
      label.cex,
      title
    ) %>%
    bind_rows(connectors) %>%
    mutate(type = factor(type, levels = c("mom", "dad", "parents", "children", "kid"), ordered = TRUE),
           level = case_when(
             type == "parents"  ~ generation + 0.5,
             type == "children" ~ generation - 0.5,
             type == "kid"      ~ generation + 1,
             .default = generation
           )) %>%
    arrange(famid, type, id) %>%
    distinct() %>%
    mutate(id_node = row_number()) %>%
    relocate(id_node)

  return(nodes)
}

edges_links <- function(studbook, pedigree) {
  nodes <- nodes_ped(studbook = studbook,
                     pedigree = pedigree)
  parents <- nodes %>%
    filter(type %in% c("mom", "dad")) %>%
    select(to_node  = id_node,
           famid,
           label,
           id)
  edges_links <- nodes %>%
    filter(link == id) %>%
    select(from_node  = id_node,
           id) %>%
    left_join(parents, by = "id") %>%
    select(from = from_node,
           to   = to_node,
           label,
           famid) %>%
    arrange(from, to) %>%
    filter(from != to) %>%
    distinct() %>%
    mutate(
      arrow.size = 1,
      lty        = 3,
      curved     = TRUE,
      dashes     = TRUE,
      shadow     = FALSE,
      arrows     = "to",
      color      = "#00000080",
      value      = 10
    )
  return(edges_links)
}

edges_hubs <- function(studbook, pedigree) {
  nodes_ped(studbook = studbook,
            pedigree = pedigree) %>%
    filter(group == "hub") %>%
    arrange(famid, generation) %>%
    select(famid,
           id_node,
           type
    ) %>%
    pivot_wider(
      names_from = "type",
      values_from = "id_node"
    ) %>%
    select(from = parents,
           to   = children,
           famid)  %>%
    distinct() %>%
    mutate(
      from       = as.integer(from),
      to         = as.integer(to),
      arrow.size = 0,
      lty        = 1,
      curved     = FALSE,
      dashes     = FALSE,
      shadow     = TRUE,
      color      = "#5b5b5bFF",
      value      = 2
    ) %>%
    arrange(from, to)
}

edges_moms <- function(studbook, pedigree) {
  nodes_ped(studbook = studbook,
            pedigree = pedigree) %>%
    filter(type %in% c("mom", "parents")) %>%
    arrange(famid, type) %>%
    select(famid,
           id_node,
           type
    ) %>%
    pivot_wider(
      names_from = "type",
      values_from = "id_node"
    ) %>%
    select(from = mom,
           to   = parents,
           famid)  %>%
    distinct() %>%
    mutate(
      from       = as.integer(from),
      to         = as.integer(to),
      arrow.size = 0,
      lty        = 1,
      curved     = FALSE,
      dashes     = FALSE,
      shadow     = TRUE,
      color      = "#5b5b5bFF",
      value      = 1
    ) %>%
    arrange(from, to)
}

edges_dads <- function(studbook, pedigree) {
  nodes_ped(studbook = studbook,
            pedigree = pedigree) %>%
    filter(type %in% c("dad", "parents")) %>%
    arrange(famid, type) %>%
    select(famid,
           id_node,
           type
    ) %>%
    pivot_wider(
      names_from = "type",
      values_from = "id_node"
    ) %>%
    select(from = dad,
           to   = parents,
           famid)  %>%
    distinct() %>%
    mutate(
      from       = as.integer(from),
      to         = as.integer(to),
      arrow.size = 0,
      lty        = 1,
      curved     = FALSE,
      dashes     = FALSE,
      shadow     = TRUE,
      color      = "#5b5b5bFF",
      value      = 1
    ) %>%
    arrange(from, to)
}

edges_kids <- function(studbook, pedigree) {
  nodes_ped(studbook = studbook,
            pedigree = pedigree) %>%
    filter(type %in% c("children", "kid")) %>%
    arrange(famid, type) %>%
    select(famid,
           id_node,
           type
    ) %>%
    pivot_wider(
      names_from  = "type",
      values_from = "id_node",
      values_fn   = list
    ) %>%
    select(from = children,
           to   = kid,
           famid)  %>%
    distinct() %>%
    unnest(to) %>%
    mutate(
      from       = as.integer(from),
      to         = as.integer(to),
      arrow.size = 0,
      lty        = 1,
      curved     = FALSE,
      dashes     = FALSE,
      shadow     = TRUE,
      color      = "#5b5b5bFF",
      value      = 3
    ) %>%
    arrange(from, to)
}

edges_ped <- function(studbook, pedigree) {
  links <- edges_links(studbook = studbook, pedigree = pedigree)
  hubs  <- edges_hubs(studbook = studbook, pedigree = pedigree)
  moms  <- edges_moms(studbook = studbook, pedigree = pedigree)
  dads  <- edges_dads(studbook = studbook, pedigree = pedigree)
  kids  <- edges_kids(studbook = studbook, pedigree = pedigree)
  edges <- bind_rows(
    links,
    hubs,
    moms,
    dads,
    kids
  ) %>%
    arrange(famid, from, to) %>%
    mutate(length = value) %>%
    select(
      from,
      to,
      value,
      length,
      curved,
      dashes,
      color,
      shadow,
      lty,
      arrow.size,
      arrows,
      label
    )
  return(edges)
}

igraph_ped <- function(studbook, pedigree) {
  vertices <- nodes_ped(studbook = studbook, pedigree = pedigree) %>%
    select(
      id = id_node,
      label,
      level,
      color,
      shape,
      frame.color,
      size,
      label.cex
    )
  edges <- edges_ped(studbook = studbook, pedigree = pedigree) %>%
    select(
      from,
      to,
      value,
      curved,
      color,
      lty,
      arrow.size,
      label
    )
  graph <- graph_from_data_frame(edges, directed = FALSE, vertices = vertices)
  return(graph)
}

subgraphs_ped <- function(studbook, pedigree) {
  igraph <- igraph_ped(studbook = studbook, pedigree = pedigree)
  famids <- nodes_ped(studbook = studbook, pedigree = pedigree) %>%
    select(
      id = id_node,
      famid
    ) %>%
    group_by(famid) %>%
    summarize(vids = list(id)) %>%
    ungroup() %>%
    deframe()
  graphs <- map(famids, \(x) subgraph(igraph, x))
  return(graphs)
}

visPed <- function(studbook, pedigree) {
  nodes <- nodes_ped(studbook = studbook, pedigree = pedigree) %>%
    select(
      id = id_node,
      label,
      level,
      group,
      title,
      famid
    ) %>%
    mutate(n = max(id))
  edges <- edges_ped(studbook = studbook, pedigree = pedigree) %>%
    select(from,
           to,
           value,
           title = label,
           length,
           dashes,
           color,
           shadow,
           arrows
           )
  graph <- visNetwork(nodes = nodes, edges = edges, width = "100%") %>%
    ped_visGroups() %>%
    visInteraction(tooltipDelay = 10,
                   tooltipStyle = "visibility:hidden")
  return(graph)
}

