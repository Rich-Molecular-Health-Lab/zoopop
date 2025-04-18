ids_ped <- function(pedigree) {
  ped_members  <- as.list(pedigree[["ID"]])
  internal_ids <- map(ped_members, \(x) internalID(pedigree, x)) %>%
    set_names(ped_members) %>%
    enframe("label", "id_ped") %>%
    mutate(across(everything(), ~as.numeric(.))) %>%
    select(id_ped,
           label)
  return(internal_ids)
}


generations_ped <- function(pedigree) {
  as.list(generations(pedigree, what = "indiv")) %>%
    enframe("label", "generation") %>%
    mutate(across(everything(), ~as.numeric(.))) %>%
    arrange(generation) %>%
    left_join(ids_ped(pedigree = pedigree)) %>%
    select(id_ped,
           label,
           generation) %>%
    arrange(generation, id_ped)
}

fams_ped <- function(pedigree) {
  peelingOrder(pedigree) %>%
    map_depth(1, \(x) unclass(x)) %>%
    set_names(seq_along(.)) %>%
    map_depth(1, \(x) assign_in(x, "subnuc", list(father = x$father, mother = x$mother, children = x$children))) %>%
    map_depth(1, \(x) keep_at(x, c("link", "subnuc"))) %>%
    map_depth(1, \(x) modify_in(x, "subnuc", \(y) enframe(y, name = "type", value = "id_ped"))) %>%
    map_depth(1, \(x) modify_in(x, "subnuc", \(y) unnest(y, id_ped))) %>%
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
    mutate(id_count = n(), .by = id_ped)
}

nodes_indiv <- function(studbook, pedigree) {
  colors     <- set_colors()
  cols.light <- lighten_palette(colors, "CC")
  nodes <-  fams_ped(pedigree = pedigree) %>%
    left_join(generations_ped(pedigree = pedigree), by = "id_ped") %>%
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
    mutate(fam_year = max(Year_birth), .by = famid) %>%
    arrange(fam_year) %>%
    rename(id_stud = label) %>%
    mutate(level = if_else(
      type == "kid",
      dense_rank(fam_year) + 1,
      dense_rank(fam_year) - 2
    )) %>%
    mutate(level = level + 2) %>%
    select(
      famid,
      link,
      id_count,
      id_ped,
      id_stud,
      label = label_spec,
      generation,
      fam_year,
      level,
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
    filter(!is.na(id_ped)) %>%
    arrange(fam_year, id_stud) %>%
    mutate(famid = as.character(famid)) %>%
    mutate(subnuc_id = fct_inorder(famid)) %>%
    mutate(subnuc_id = fct_relabel(subnuc_id, ~as.character(seq_along(.)))) %>%
    mutate(famid = as.integer(subnuc_id), .keep = "unused") %>%
    arrange(famid, id_ped)
  return(nodes)
}

nodes_connectors <- function(studbook, pedigree) {
  individuals <- nodes_indiv(studbook = studbook, pedigree = pedigree)
  connectors_children <- individuals %>%
    filter(type == "kid") %>%
    slice_tail(n = 1, by = famid) %>%
    mutate(type        = "children",
           group       = "hub",
           level       = level - 1,
           value       = 3,
           label       = label_connector,
           id_count    = 1,
           id_stud     = NA,
           id_ped      = NA,
           size        = 1,
           shape       = "circle",
           fillcolor   = "gray",
           frame.color = "gray",
           title       = tip_connector) %>%
    select(
      famid,
      link,
      id_ped,
      id_stud,
      label,
      level,
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

  connectors <- individuals %>%
    filter(type == "mom") %>%
    mutate(type = "parents",
           group       = "hub",
           value       = 1,
           level       = level + 0.5,
           label       = NA,
           id_count    = 1,
           id_stud     = NA,
           id_ped      = NA,
           size        = 1,
           shape       = "circle",
           fillcolor   = "gray",
           frame.color = "gray",
           title       = NA) %>%
    select(
      famid,
      link,
      id_count,
      id_ped,
      id_stud,
      label,
      level,
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
  return(connectors)
}

nodes_ped <- function(studbook, pedigree) {
  nodes_indiv(studbook = studbook, pedigree = pedigree) %>%
    select(
      famid,
      link,
      id_count,
      id_ped,
      id_stud,
      label,
      level,
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
    bind_rows(nodes_connectors(studbook = studbook, pedigree = pedigree)) %>%
    mutate(type = factor(type, levels = c("mom", "dad", "parents", "children", "kid"), ordered = TRUE)) %>%
    arrange(famid, type, id_ped) %>%
    distinct() %>%
    mutate(id_node = row_number()) %>%
    relocate(id_node)
}

edges_links <- function(studbook, pedigree) {
  nodes <- nodes_ped(studbook = studbook,
                     pedigree = pedigree)

  kids    <- nodes %>% filter(type == "kid")
  parents <- nodes %>%
    filter(type %in% c("mom", "dad")) %>%
    select(to_node = id_node, id_ped, label, famid)
  edges <- kids %>%
    filter(id_count > 1) %>%
    select(from_node  = id_node,
           id_ped,
           color) %>%
    left_join(parents, by = "id_ped") %>%
    select(from = from_node,
           to   = to_node,
           color,
           label,
           famid) %>%
    mutate(color = if_else(str_ends(color, "CC"),
                           str_replace_all(color, "CC", "80"),
                           str_replace_all(color, "FF", "99"))) %>%
    arrange(from, to) %>%
    filter(from != to) %>%
    distinct() %>%
    mutate(
      arrow.size = 0.7,
      width      = 0.8,
      lty        = 3,
      curved     = TRUE,
      dashes     = TRUE,
      shadow     = FALSE,
      arrows     = "to",
      value      = 10
    )
  return(edges)
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
      width      = 2,
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
  colors     <- set_colors()
  cols.light <- lighten_palette(colors, "CC")
  edges <- nodes_ped(studbook = studbook,
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
      width      = 2,
      lty        = 1,
      curved     = FALSE,
      dashes     = FALSE,
      shadow     = TRUE,
      color      = colors[["f"]],
      value      = 1
    ) %>%
    arrange(from, to)
  return(edges)
}

edges_dads <- function(studbook, pedigree) {
  colors     <- set_colors()
  cols.light <- lighten_palette(colors, "CC")
  edges <- nodes_ped(studbook = studbook,
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
      width      = 2,
      lty        = 1,
      curved     = FALSE,
      dashes     = FALSE,
      shadow     = TRUE,
      color      = colors[["m"]],
      value      = 1
    ) %>%
    arrange(from, to)
  return(edges)
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
      width      = 2,
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
      width,
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
           title = label,
           length,
           width,
           dashes,
           color,
           shadow,
           arrows
           )
  graph <- visNetwork(nodes = nodes, edges = edges, width = "100%") %>%
    ped_visGroups() %>%
    visInteraction(tooltipDelay = 10,
                   tooltipStyle = "visibility:hidden") %>%
    visHierarchicalLayout(
      nodeSpacing     = 175,
      levelSeparation = 200,
      sortMethod      = "directed",
      shakeTowards    = "roots"
    ) %>%
    visPhysics(enabled = FALSE) %>%
    visOptions(highlightNearest = list(enabled =TRUE, degree = 2, hover = T),
               selectedBy       = list(variable = "label", multiple = T))
  return(graph)
}

