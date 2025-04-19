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
    enframe("label", "id_ped") %>%
    mutate(across(everything(), ~as.numeric(.))) %>%
    select(id_ped,
           label)
  return(internal_ids)
}

#' Compute Generation Information for a Pedigree
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
ped_generations <- function(pedigree) {
  as.list(generations(pedigree, what = "indiv")) %>%
    enframe("label", "generation") %>%
    mutate(across(everything(), ~as.numeric(.))) %>%
    arrange(generation) %>%
    left_join(ped_ids(pedigree = pedigree)) %>%
    select(id_ped,
           label,
           generation) %>%
    arrange(generation, id_ped)
}

#' Extract Subnuclear Structures from a Pedigree
#'
#' This function extracts subnuclei (nuclear family units) from a pedigree and computes their levels and generation information.
#'
#' @param pedigree A pedigree object.
#' @return A data frame with subnuclei information including parents, children, and level data.
#' @export
#' @importFrom dplyr mutate case_match n
#' @importFrom pedtools peelingOrder
#' @importFrom purrr map_depth keep_at modify_in assign_in
#' @importFrom rlang set_names
#' @importFrom tibble enframe
#' @importFrom tidyr unnest hoist
ped_subnucs <- function(pedigree) {
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


#' Create Pedigree Node Metadata for Visualization
#'
#' This function combines pedigree level data with studbook information to produce metadata
#' required for pedigree visualization.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame with metadata including IDs, labels, groups, and tooltips.
#' @export
#' @importFrom dplyr mutate select rename left_join case_when if_else join_by filter dense_rank arrange
#' @importFrom forcats fct_inorder fct_relabel
#' @importFrom tidyselect starts_with
#' @importFrom stringr str_glue str_starts str_ends
ped_metadata <- function(studbook, pedigree) {
  colors     <- set_colors()
  cols.light <- lighten_palette(colors, "CC")
  nodes <-  ped_subnucs(pedigree = pedigree) %>%
    left_join(ped_generations(pedigree = pedigree), by = "id_ped") %>%
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
      Year_birth,
      Institution_birth,
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
      starts_with("tip_"),
      exclude
    ) %>%
    mutate(exclude = case_match(
      exclude,
      "deceased"     ~"Excluded - Deceased",
      "n"            ~"Currently Included",
      "hypothetical" ~"Excluded - Hypothetical ID",
      "age"          ~"Excluded - Old Age",
      "behavior"     ~"Excluded - Behavioral Reasons"
    )) %>%
    filter(!is.na(id_ped)) %>%
    arrange(fam_year, id_stud) %>%
    mutate(famid = as.character(famid)) %>%
    mutate(subnuc_id = fct_inorder(famid)) %>%
    mutate(subnuc_id = fct_relabel(subnuc_id, ~as.character(seq_along(.)))) %>%
    mutate(famid = as.integer(subnuc_id), .keep = "unused") %>%
    arrange(famid, id_ped) %>%
    mutate(color_connector = last(color_connector), .by = famid)
  return(nodes)
}

#' Create Nodes for Subnuclear Connections
#'
#' This function creates a template node data frame linking subnuclei nodes in the pedigree.
#' (variables can be selected or removed later depending on which visualization tool you use)
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of nodes for subnuclei connections.
#' @export
#' @importFrom dplyr filter slice_tail mutate select bind_rows arrange group_by ungroup
#' @importFrom tidyr fill
nodes_connectors <- function(studbook, pedigree) {
  individuals <- ped_metadata(studbook = studbook, pedigree = pedigree)
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
           color       = color_connector,
           fillcolor   = "#5b5b5bFF",
           frame.color = "#5b5b5bFF",
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
      color_connector,
      shape,
      frame.color,
      size,
      label.cex,
      title
    )

  connectors <- individuals %>%
    filter(type == "mom") %>%
    mutate(type        = "parents",
           group       = "hub",
           value       = 1,
           level       = level + 0.5,
           label       = NA,
           id_count    = 1,
           id_stud     = NA,
           id_ped      = NA,
           size        = 1,
           shape       = "circle",
           color       = color_connector,
           fillcolor   = "#5b5b5bFF",
           frame.color = "#5b5b5bFF",
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
      color_connector,
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

#' Create a complete node data frame that includes connector nodes and original nodes representing individuals
#'
#' This function creates a template node data frame linking all nodes in the network visualization.
#' (variables can be selected or removed later depending on which visualization tool you use)
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of nodes for different visualization tools.
#' @export
#' @importFrom dplyr select bind_rows mutate arrange distinct row_number relocate
ped_nodes <- function(studbook, pedigree) {
  ped_metadata(studbook = studbook, pedigree = pedigree) %>%
    select(
      famid,
      link,
      id_count,
      id_ped,
      id_stud,
      label,
      level,
      generation,
      Year_birth,
      Institution_birth,
      group,
      type,
      value,
      color,
      color_connector,
      shape,
      frame.color,
      size,
      label.cex,
      title,
      exclude
    ) %>%
    bind_rows(nodes_connectors(studbook = studbook, pedigree = pedigree)) %>%
    mutate(type = factor(type, levels = c("mom", "dad", "parents", "children", "kid"), ordered = TRUE)) %>%
    arrange(famid, type, id_ped) %>%
    distinct() %>%
    mutate(id_node = row_number()) %>%
    relocate(id_node)
}

#' Create Edges connecting an individual across subnuclei/transfers
#'
#' This function creates an edge data frame linking any individual across multiple node representations
#' (Some individuals are a part of multiple subnuclear units based on transfers over time)
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of edges for subnuclei connections.
#' @export
#' @importFrom dplyr filter select left_join if_else arrange distinct mutate
#' @importFrom stringr str_ends str_replace_all
edges_links <- function(studbook, pedigree) {
  nodes <- ped_nodes(studbook = studbook,
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
      smooth     = TRUE,
      dashes     = TRUE,
      shadow     = FALSE,
      arrows     = "to",
      value      = 10
    )
  return(edges)
}


#' Create Edges Connecting Parents to Children
#'
#' This function creates an edge data frame linking parent connector nodes to offspring connector nodes in the pedigree
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of edges for subnuclei connections.
#' @export
#' @importFrom dplyr filter arrange select distinct mutate
#' @importFrom tidyr pivot_wider
edges_hubs <- function(studbook, pedigree) {
  ped_nodes(studbook = studbook,
            pedigree = pedigree) %>%
    group_by(famid) %>%
    fill(color_connector) %>%
    ungroup() %>%
    filter(group == "hub") %>%
    arrange(famid, generation) %>%
    select(famid,
           color_connector,
           id_node,
           type
    ) %>%
    pivot_wider(
      names_from = "type",
      values_from = "id_node"
    ) %>%
    select(from  = parents,
           to    = children,
           famid,
           color = color_connector)  %>%
    distinct() %>%
    mutate(
      from       = as.integer(from),
      to         = as.integer(to),
      arrow.size = 0,
      width      = 2.5,
      lty        = 1,
      curved     = FALSE,
      smooth     = FALSE,
      dashes     = FALSE,
      shadow     = TRUE,
      value      = 2
    ) %>%
    arrange(from, to)
}

#' Create Edges Connecting Moms to Parent Connector Node
#'
#' This function creates an edge data frame linking mom nodes to connector nodes in the pedigree
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of edges for mom connections.
#' @export
#' @importFrom dplyr filter arrange select distinct mutate
#' @importFrom tidyr pivot_wider
edges_moms <- function(studbook, pedigree) {
  colors     <- set_colors()
  cols.light <- lighten_palette(colors, "E6")
  edges <- ped_nodes(studbook = studbook,
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
      width      = 2.5,
      color      = cols.light[["f"]],
      lty        = 1,
      curved     = FALSE,
      smooth     = FALSE,
      dashes     = FALSE,
      shadow     = TRUE,
      value      = 1
    ) %>%
    arrange(from, to)
  return(edges)
}

#' Create Edges Connecting Dads to Parent Connector Node
#'
#' This function creates an edge data frame linking dad nodes to connector nodes in the pedigree
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of edges for mom connections.
#' @export
#' @importFrom dplyr filter arrange select distinct mutate
#' @importFrom tidyr pivot_wider
edges_dads <- function(studbook, pedigree) {
  colors     <- set_colors()
  cols.light <- lighten_palette(colors, "E6")
  edges <- ped_nodes(studbook = studbook,
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
      width      = 2.5,
      color      = cols.light[["m"]],
      lty        = 1,
      curved     = FALSE,
      smooth     = FALSE,
      dashes     = FALSE,
      shadow     = TRUE,
      value      = 1
    ) %>%
    arrange(from, to)
  return(edges)
}

#' Create Edges Connecting terminal child nodes in pedigree
#'
#' This function creates an edge data frame linking offspring connector nodes to each child node
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of edges for mom connections.
#' @export
#' @importFrom dplyr filter arrange select distinct mutate
#' @importFrom tidyr pivot_wider
edges_kids <- function(studbook, pedigree) {
  ped_nodes(studbook = studbook,
            pedigree = pedigree) %>%
    group_by(famid) %>%
    fill(color_connector) %>%
    ungroup() %>%
    filter(type %in% c("children", "kid")) %>%
    arrange(famid, type) %>%
    select(famid,
           color_connector,
           id_node,
           type
    ) %>%
    pivot_wider(
      names_from  = "type",
      values_from = "id_node",
      values_fn   = list
    ) %>%
    select(from  = children,
           to    = kid,
           famid,
           color = color_connector)  %>%
    distinct() %>%
    unnest(to) %>%
    mutate(
      from       = as.integer(from),
      to         = as.integer(to),
      arrow.size = 0,
      width      = 2.5,
      lty        = 1,
      curved     = FALSE,
      smooth     = FALSE,
      dashes     = FALSE,
      shadow     = TRUE,
      value      = 3
    ) %>%
    arrange(from, to)
}


#' Create Edges to connect all nodes for multiple visualization tools
#'
#' This function builds an edge data frame linking all nodes with visualization variables that can be selected
#' or deselected based on the tool in use.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of edges linking parents with their children.
#' @export
#' @importFrom dplyr bind_rows arrange mutate select
ped_edges <- function(studbook, pedigree) {
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
      smooth,
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
