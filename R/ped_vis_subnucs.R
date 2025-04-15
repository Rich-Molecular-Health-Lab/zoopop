
subnuc_metadata <- function(studbook, pedigree) {
  colors <- set_colors()
  ped_ids(pedigree = pedigree) %>%
    left_join(studbook_short(studbook = studbook),
              by = join_by(label_subject == ID)) %>%
    mutate(label_spec = if_else(
      is.na(name_spec), as.character(label_subject),
      as.character(str_glue("{name_spec} ({as.character(label_subject)})"))),
      color_subj = case_match(
        Sex,
        "M" ~ colors[["m"]],
        "F" ~ colors[["f"]],
        "U" ~ colors[["u"]]
      ),
      shape_subj = case_match(
        Sex,
        "M" ~ "square",
        "F" ~ "circle",
        "U" ~ "diamond"
      ),
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
      tip_subnuc = as.character(
        str_glue("{Institution_birth} {iconLoc_birth} ({State_Province_birth})")
      ),
      color_subnuc    = colorLoc_birth,
      label_subnuc    = Loc_birth
    ) %>%
    mutate(
      tip_status = if_else(
        exclude == "deceased",
        as.character(str_glue("Death - {tip_last}")),
        as.character(str_glue("Currently {tip_last}"))
      )
    ) %>%
    select(
      id = id_subject,
      label = label_spec,
      group,
      color_subj,
      shape_subj,
      color_subnuc,
      label_subnuc,
      starts_with("tip_")
    )
}

subnucs_nested <- function(pedigree) {
  connectors <- tibble(
    type = "hub",
    id   = c(1000, 2000)
  )
  subnucs(pedigree) %>%
    map_depth(., 1, \(x) unclass(x)) %>%
    map(\(x) enframe(x, name = "type", value = "id")) %>%
    map_depth(1, \(x) unnest_longer(x, id)) %>%
    map_depth(1, \(x) mutate(x,
                             type = case_match(type,
                                               "mother"   ~"mom",
                                               "father"   ~"dad",
                                               "children" ~"kid",
                                               .default = type))) %>%
    map_depth(1, \(x) bind_rows(x, connectors))
}

subnucs_edges <- function(subnuc) {
  parents <- filter(subnuc, type %in% c("mom", "dad")) %>%
    mutate(to   = 1000) %>%
    rename(from = id, rel = type)
  children <- filter(subnuc, type == "kid") %>%
    mutate(from = 2000) %>%
    rename(to = id, rel = type)
  connectors <- tibble(
    from = 1000,
    to   = 2000,
    rel  = "hub"
  )
  edges <- bind_rows(
    parents,
    children,
    connectors
  ) %>%
    select(
      from,
      to,
      rel
    ) %>%
    arrange(from, to)
  return(edges)
}

subnucs_nodes <- function(subnuc, studbook, pedigree) {
  metadata <- subnuc_metadata(studbook = studbook,
                              pedigree = pedigree)

  nodes_subjects  <- select(metadata,
                        id,
                        label,
                        group,
                        color = color_subj,
                        starts_with("tip_")) %>%
    mutate(title = as.character(str_glue(
      "<h4>{tip_id}</h4><p>{tip_breed}</p><p>{tip_birth}</p><p>{tip_status}</p>"
    ))) %>%
    select(-starts_with("tip_")) %>%
    right_join(filter(subnuc, type != "hub"), by = "id") %>%
    mutate(level = if_else(type == "kid", 3, 1))

  nodes_hubs <- filter(subnuc, type == "kid") %>%
    select(id) %>%
    left_join(metadata, by = "id") %>%
    arrange(id) %>%
    slice_tail(n = 1) %>%
    select(title = tip_subnuc,
           label = label_subnuc) %>%
    mutate(type  = "hub",
           group = "connector") %>%
    distinct()

  nodes <- nodes_hubs %>%
    mutate(id = 1000, level = 1) %>%
    bind_rows(nodes_hubs) %>%
    mutate(id = replace_na(id, 2000), level = replace_na(level, 2)) %>%
    bind_rows(nodes_subjects) %>%
    arrange(id) %>%
    select(id,
           level,
           label,
           type,
           group,
           title
           ) %>%
    distinct()
  return(nodes)
}

map_subnucs_edges <- function(pedigree) {
  subnucs <- subnucs_nested(pedigree = pedigree)

  edges <- map_depth(subnucs, 1, \(x) subnucs_edges(x))
  return(edges)
}

map_subnucs_nodes <- function(studbook, pedigree) {
  subnucs <- subnucs_nested(pedigree = pedigree)

  nodes <- map_depth(subnucs, 1, \(x) subnucs_nodes(x, studbook = studbook, pedigree = pedigree))
  return(nodes)

}

subnucs_net_data <- function(studbook, pedigree) {
  nodes <- map_subnucs_nodes(studbook = studbook, pedigree = pedigree)
  edges <- map_subnucs_edges(pedigree = pedigree)

  merged <- map2(nodes, edges, \(x, y) list(nodes = x, edges = y))
  return(merged)
}

visSubnucs_base <- function(nodes, edges) {
  graph <- visNetwork(nodes  = nodes,
                      edges  = edges,
                      width  = "100%") %>%
    visEdges(width = 1.5, color = "inherit") %>%
    ped_visIcons() %>%
    visInteraction(tooltipDelay = 10,
                   tooltipStyle = "visibility:hidden") %>%
    visHierarchicalLayout()
  return(graph)
}

visSubnucs <- function(studbook, pedigree) {
  data    <- subnucs_net_data(studbook = studbook, pedigree = pedigree)
  graph   <- map_depth(data, 1, \(x) as.list(visSubnucs_base(nodes = x[["nodes"]], edges = x[["edges"]])))
  return(graph)
}
