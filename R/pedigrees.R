# pedigrees.R
#' Create Numeric Versions of Sex Variables for Pedigree Analysis
#'
#' This function converts the character values in the \code{Sex} column of a studbook
#' into numeric representations used by pedigree tools. It creates two new variables,
#' \code{sex_ped} and \code{sex_kin}, using different conventions.
#'
#' @param studbook A data frame containing studbook data with a \code{Sex} column.
#' @return A data frame with additional numeric columns \code{sex_ped} and \code{sex_kin}.
#' @export
#' @importFrom dplyr mutate across case_match
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

#' Build a Series of Pedigree Objects from a Studbook
#'
#' This function converts a studbook into a list of pedigree objects using
#' \code{pedtools::ped()} and discards any singletons (pedigrees with size one or less).
#'
#' @param studbook A data frame containing studbook data.
#' @return A list of pedigree objects.
#' @export
#' @importFrom pedtools ped pedsize
#' @importFrom purrr discard
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

#' Choose a Pedigree Based on Maximum Depth
#'
#' This function selects the pedigree name corresponding to the highest computed depth
#' from a series of pedigrees.
#'
#' @param pedigree_series A list of pedigree objects.
#' @return A character string representing the selected pedigree name.
#' @export
#' @importFrom pedtools generations
choose_pedigree <- function(pedigree_series) {
  depth <- as.list(generations(pedigree_series, what = "compMax"))
  name  <- names(which.max(unlist(depth)))
  return(name)
}

#' Extract a Specific Pedigree from a Pedigree Series
#'
#' This function extracts a pedigree object from a list of pedigrees based on a provided name.
#' If no name is supplied, the pedigree with maximum depth is chosen.
#'
#' @param pedigree_series A list of pedigree objects.
#' @param name Optional. The name of the pedigree object to extract.
#' @param title Optional. A new title to assign to the extracted pedigree.
#' @return A single pedigree object.
#' @export
#' @importFrom purrr keep_at
#' @importFrom pedtools famid
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

#' Get the Featured Pedigree from a Studook
#'
#' This function builds the pedigree series from the studbook and extracts the featured pedigree.
#'
#' @param studbook A data frame containing studbook data.
#' @return A pedigree object representing the featured pedigree.
#' @export
featured_pedigree <- function(studbook) {
  series   <- build_ped_series(studbook = studbook)
  pedigree <- extract_pedigree(pedigree_series = series)
  return(pedigree)
}

#' Generate Internal IDs for Pedigree Members
#'
#' This function creates a data frame containing internal numeric IDs and corresponding labels for the members of a pedigree.
#'
#' @param pedigree A pedigree object.
#' @return A data frame with columns \code{id_subject} and \code{label_subject}.
#' @export
#' @importFrom dplyr mutate across select
#' @importFrom pedtools internalID
#' @importFrom purrr map
#' @importFrom rlang set_names
#' @importFrom tibble enframe
#' @importFrom tidyselect everything
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

#' Extract Subnuclear Structures from a Pedigree
#'
#' This function extracts subnuclei (nuclear family units) from a pedigree and computes their levels and generation information.
#'
#' @param pedigree A pedigree object.
#' @return A data frame with subnuclei information including parents, children, and level data.
#' @export
#' @importFrom dplyr mutate select arrange relocate left_join join_by
#' @importFrom pedtools subnucs
#' @importFrom purrr map_depth
#' @importFrom tibble enframe
#' @importFrom tidyr unnest_longer hoist
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

#' Compute Generation and Level Information for a Pedigree
#'
#' This function computes and returns the generation numbers and corresponding levels for each individual
#' in a pedigree.
#'
#' @param pedigree A pedigree object.
#' @return A data frame with subject IDs, labels, levels, and generation numbers.
#' @export
#' @importFrom dplyr mutate select arrange left_join across
#' @importFrom pedtools generations
#' @importFrom tibble enframe
#' @importFrom tidyselect everything
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

#' Create Pedigree Metadata for Visualization
#'
#' This function combines pedigree level data with studbook information to produce metadata
#' required for pedigree visualization.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame with metadata including IDs, labels, groups, and tooltips.
#' @export
#' @importFrom dplyr mutate select right_join left_join across case_when if_else join_by filter
#' @importFrom stringr str_glue
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

#' Summarize Subnuclear Units in a Pedigree
#'
#' This function groups the subnuclei (nuclear family units) and counts the number
#' of offspring within each.
#'
#' @param pedigree A pedigree object.
#' @return A data frame summarizing each subnucleus with the number of children.
#' @export
#' @importFrom dplyr group_by summarize ungroup n
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

#' Create Edges for Subnuclear Connections
#'
#' This function creates an edge data frame linking subnuclei nodes in the pedigree,
#' and then merges these with child connection data.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of edges for subnuclei connections.
#' @export
#' @importFrom dplyr select left_join join_by mutate
#' @importFrom tidyr pivot_wider
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

#' Create Edges Connecting Parents to Children
#'
#' This function builds an edge data frame linking parents to children in the pedigree,
#' with associated connector colors.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of edges linking parents with their children.
#' @export
#' @importFrom dplyr filter select left_join join_by mutate
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

#' Create Edges for Maternal Connections
#'
#' This function extracts the edge data that connects mothers in the pedigree.
#'
#' @param pedigree A pedigree object.
#' @return A data frame of edges representing maternal connections.
#' @export
#' @importFrom dplyr filter select mutate
#' @importFrom purrr pluck
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

#' Create Edges for Paternal Connections
#'
#' This function extracts the edge data that connects fathers in the pedigree.
#'
#' @param pedigree A pedigree object.
#' @return A data frame of edges representing paternal connections.
#' @export
#' @importFrom dplyr filter select mutate
#' @importFrom purrr pluck
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

#' Combine Pedigree Edge Data
#'
#' This function combines the edges for children, subnuclei, maternal, and paternal connections
#' into a single edge data frame for pedigree visualization.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of combined edges.
#' @export
#' @importFrom dplyr bind_rows arrange distinct mutate relocate row_number
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

#' Create Node Data for Pedigree Subjects
#'
#' This function builds a node data frame for the pedigree subjects based on metadata,
#' which can be used for interactive plotting.
#'
#' @param studbook A data frame with studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of nodes for pedigree subjects.
#' @export
#' @importFrom dplyr mutate select
#' @importFrom stringr str_glue
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

#' Create Node Data for Pedigree Subnuclei
#'
#' This function constructs a node data frame for subnuclei, which serve as connectors
#' for nuclear family units in the pedigree visualization.
#'
#' @param pedigree A pedigree object.
#' @return A data frame of nodes for subnuclei.
#' @export
#' @importFrom dplyr pull mutate consecutive_id arrange if_else select
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect starts_with
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

