pedigree_studbook <- function(studbook) {
  studbook_short(studbook) %>%
    mutate(sex_ped = case_match(Sex,
                                "U" ~"0",
                                "M" ~"1",
                                "F" ~"2"),
           sex_kin = case_match(Sex,
                                "U" ~"3",
                                "M" ~"1",
                                "F" ~"2")) %>%
    mutate(across(c(sex_ped, sex_kin), ~as.numeric(.)))
}

build_ped_series <- function(studbook) {
  studbook <- pedigree_studbook(studbook)
  pedigree <- ped(
    id     = studbook$ID,
    fid    = studbook$Sire,
    mid    = studbook$Dam,
    sex    = studbook$sex_ped
  ) %>%
    discard(., \(x) pedtools::pedsize(x) <= 1)
  return(pedigree)
}


choose_pedigree <- function(pedigree_series) {
  depth <- as.list(generations(pedigree_series, what = "compMax"))
  name  <- names(which.max(unlist(depth)))
  return(name)
}

extract_pedigree <- function(pedigree_series, name = NULL, title = NULL) {
  if (is.null(name)) {
    name <- choose_pedigree(pedigree_series = pedigree_series)
  }
  pedigree    <- keep_at(pedigree_series, name)
  pedigree    <- pedigree[[1]]
  if (!is.null(title)) {
    name <- pedtools::famid(pedigree)
  }
  return(pedigree)
}

featured_pedigree <- function(studbook) {
  series   <- build_ped_series(studbook = studbook)
  pedigree <- extract_pedigree(pedigree_series = series)
  return(pedigree)
}

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

ped_subnucs <- function(pedigree) {
  subnucs <- subnucs(pedigree) %>% map_depth(., 1, \(x) unclass(x)) %>%
    enframe(name = "id_subnuc") %>%
    mutate(id_subnuc = as.numeric(id_subnuc)) %>%
    hoist(value, dad = c("father")) %>%
    hoist(value, mom = c("mother")) %>%
    hoist(value, kid = c("children")) %>%
    unnest_longer(kid) %>%
    left_join(ped_levels(pedigree = pedigree),
              by = join_by(kid == id_subject)) %>%
    select(id_subnuc,
           dad,
           mom,
           kid,
           kid_generation = generation_subject,
           kid_level      = level_subject) %>%
    left_join(ped_levels(pedigree = pedigree),
              by = join_by(mom == id_subject)) %>%
    select(id_subnuc,
           dad,
           mom,
           kid,
           mom_generation = generation_subject,
           kid_generation,
           mom_level      = level_subject,
           kid_level) %>%
    left_join(ped_levels(pedigree = pedigree),
              by = join_by(dad == id_subject)) %>%
    select(id_subnuc,
           dad,
           mom,
           kid,
           dad_generation = generation_subject,
           mom_generation,
           kid_generation,
           dad_level      = level_subject,
           mom_level,
           kid_level) %>%
    arrange(kid_generation, dad_generation, mom_generation, dad, mom) %>%
    mutate(id_subnuc = consecutive_id(dad, mom)) %>%
    relocate(id_subnuc)
  return(subnucs)
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
           level_subject,
           generation_subject = generation) %>%
    arrange(level_subject, id_subject)
}

ped_metadata <- function(studbook, pedigree) {
  metadata  <- ped_levels(pedigree = pedigree) %>%
    left_join(ped_subnucs(pedigree = pedigree),
              by = join_by(id_subject == kid)) %>%
    select(id_subject,
           label_subject,
           id_subnuc,
           level_subject,
           generation_subject
    ) %>%
    right_join(studbook_short(studbook = studbook),
               by = join_by(label_subject == ID)) %>%
    mutate(label_spec = if_else(
      is.na(name_spec), as.character(label_subject),
      as.character(str_glue("{name_spec} ({as.character(label_subject)})"))),
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
    select(
      id_subject,
      id_subnuc,
      label = label_spec,
      level_subject,
      generation_subject,
      group,
      color_connector,
      label_connector,
      starts_with("tip_")
    ) %>%
    filter(!is.na(id_subject))
  return(metadata)
}


summarize_subnucs <- function(pedigree) {
  ped_subnucs(pedigree = pedigree) %>%
    group_by(id_subnuc,
             dad,
             mom,
             dad_generation,
             mom_generation,
             kid_generation,
             dad_level,
             mom_level,
             kid_level) %>%
    summarize(n_kids = n()) %>%
    ungroup()
}

edges_subnucs <- function(studbook, pedigree) {
  nodes_subnucs(pedigree = pedigree) %>%
    select(
      subnuc,
      group,
      id
    ) %>%
    pivot_wider(
      names_from = "group",
      values_from = "id"
    ) %>%
    select(
      from = parents,
      to   = offspring
    ) %>%
    left_join(
      edges_children(studbook = studbook,
                     pedigree = pedigree),
      by = join_by(to == from)
    ) %>%
    select(
      from,
      to,
      color
    ) %>%
    mutate(rel = "subnucs")
}

edges_children <- function(studbook, pedigree) {
  nodes_subnucs(pedigree) %>%
    filter(group == "offspring") %>%
    select(-group) %>%
    left_join(ped_metadata(studbook = studbook,
                           pedigree = pedigree),
              by = join_by(subnuc == id_subnuc)) %>%
    select(
      from  = id,
      to    = id_subject,
      color = color_connector
    ) %>%
    mutate(rel = "children")
}

edges_moms <- function(pedigree) {
  color <- pluck(set_colors(), "f")
  nodes_subnucs(pedigree = pedigree) %>%
    filter(group == "parents") %>%
    select(-group) %>%
    select(
      from  = mom,
      to    = id
    ) %>%
    mutate(rel   = "mom",
           color = color)
}

edges_dads <- function(pedigree) {
  color <- pluck(set_colors(), "m")
  nodes_subnucs(pedigree = pedigree) %>%
    filter(group == "parents") %>%
    select(-group) %>%
    select(
      from  = dad,
      to    = id
    ) %>%
    mutate(rel   = "dad",
           color = color)
}

ped_edges <- function(studbook, pedigree) {
  bind_rows(
    edges_children(studbook = studbook,
                   pedigree = pedigree),
     edges_subnucs(studbook = studbook,
                   pedigree = pedigree),
    edges_moms(pedigree = pedigree),
    edges_dads(pedigree = pedigree)
  ) %>%
    arrange(from, to) %>%
    distinct() %>%
    mutate(id    = row_number()) %>%
    relocate(id)
}

nodes_subjects <- function(studbook, pedigree) {
  ped_metadata(studbook = studbook,
               pedigree = pedigree)  %>%
    mutate(title = as.character(str_glue(
      "<h4>{tip_id}</h4><p>{tip_breed}</p><p>{tip_birth}</p><p>{tip_status}</p>"
    ))) %>%
    select(id = id_subject,
           group,
           generation = generation_subject,
           subnuc = id_subnuc,
           label,
           level = level_subject,
           title)
}

nodes_subnucs <- function(pedigree) {
  max_id <- max(pull(ped_ids(pedigree = pedigree), id_subject))
  summarize_subnucs(pedigree = pedigree) %>%
    mutate(connect_parents   = id_subnuc,
           connect_offspring = consecutive_id(id_subnuc) + max(id_subnuc)) %>%
    pivot_longer(starts_with("connect_"),
                 names_to     = "group",
                 names_prefix = "connect_",
                 values_to    = "id") %>%
    arrange(id_subnuc, id) %>%
    mutate(id = row_number() + max_id) %>%
    mutate(generation = if_else(
      group == "offspring", kid_generation - 0.25, kid_generation - 0.5
    )) %>%
    select(subnuc = id_subnuc,
           group,
           id,
           generation,
           dad,
           mom,
           n_kids)
}

