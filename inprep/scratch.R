edge_trios    <- pedigree_births(ped, studbook) %>%
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



nonfounders <- as.list(nonfounders(ped))
birth_info <- studbook_short(studbook) %>%
  mutate(offspring = as.character(ID)) %>%
  select(
    offspring,
    name_spec,
    Sex,
    ends_with("_birth")
  ) %>% distinct()

trios <- map(nonfounders, \(x) as.list(parents(ped, x))) %>%
  set_names(., nonfounders) %>%
  enframe(name = "offspring", value = "parent") %>%
  unnest_wider(parent, names_sep = "_") %>%
  arrange(parent_1, parent_2, offspring) %>%
  rename(dad = parent_1, mom = parent_2) %>%
  distinct()

subjects <-





  mutate(pair = consecutive_id(dad, mom)) %>%
  mutate(sibs = max(pair) + consecutive_id(pair))
left_join(birth_info, by = "offspring") %>%
%>%
  rename_with(~str_remove_all(.x, "_birth"), ends_with("_birth"))

edge_trios    <- pedigree_births(ped, studbook) %>%
  select(
    offspring,
    dad,
    mom,
    pair,
    sibs,
    color = colorLoc
  ) %>%
  distinct() %>%
  mutate(across(c(offspring:sibs), ~as.integer(.))) %>%
  arrange(pair, sibs)


node_trios    <- pedigree_births(ped, studbook)


edges_pairs <- select(
  trios,
  dad_to_pair,
  mom_to_pair,
  to      = pair,
  tooltip = tooltip_pair,
  arrowtail,
  arrowhead,
  color
) %>%
  pivot_longer(c("dad_to_pair", "mom_to_pair"), names_to = "edge_type", values_to = "from") %>%
  distinct() %>%
  arrange(to, from)

edges_offspring <- select(
  trios,
  to      = pair_to_offspring,
  from    = sibs,
  tooltip = tooltip_offs,
  arrowtail,
  arrowhead,
  color
) %>%
  mutate(edge_type = "pair_to_offspring") %>%
  distinct() %>%
  arrange(to, from)

edges_connectors <- select(
  trios,
  to      = sibs,
  from    = pair,
  tooltip = tooltip_sibs,
  arrowtail,
  arrowhead,
  color
) %>%
  mutate(edge_type = "connector") %>%
  distinct() %>%
  arrange(to, from)

edges <- bind_rows(edges_pairs, edges_offspring, edges_connectors) %>%
  distinct() %>%
  arrange(from, to) %>%
  mutate(id = row_number()) %>%
  select(
    id,
    from,
    to,
    color,
    arrowtail,
    arrowhead,
    tooltip
  )

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

