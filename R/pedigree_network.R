# pedigree_network.R
# Interactive pedigree visualizations using visNetwork

#' Assign generation levels to individuals in a pedigree
#'
#' @param pedigree A pedigree object from `pedtools::ped()`
#' @param studbook A studbook tibble
#' @return A tibble of individual IDs and generation levels
#' @export
#'
#' @importFrom dplyr across
#' @importFrom dplyr distinct
#' @importFrom tibble enframe
#' @importFrom dplyr if_else
#' @importFrom dplyr mutate
#' @importFrom pedtools founders
#' @importFrom pedtools leaves
#' @importFrom rlang set_names
pedigree_levels <- function(pedigree, studbook) {
  studbook <- studbook %>% mutate(across(c(Sire, Dam), as.character))
  levels <- gen_numbers(pedigree) %>%
    as.list() %>%
    set_names(pedigree$ID) %>%
    enframe(name = "id", value = "level") %>%
    mutate(
      level = as.integer(level),
      level = if_else(id %in% founders(pedigree), level, level + 2),
      level = if_else(id %in% leaves(pedigree), level + 1, level)
    ) %>% distinct()
  return(levels)
}

#' Extract unique mating pairs from a pedigree
#'
#' @param pedigree A pedigree object
#' @param studbook A studbook tibble
#' @return A tibble of unique mating pairs with location info
#' @export
#'
#' @importFrom dplyr across
#' @importFrom dplyr distinct
#' @importFrom tibble enframe
#' @importFrom dplyr left_join
#' @importFrom dplyr join_by
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr transmute
#' @importFrom pedtools nonfounders
#' @importFrom pedtools parents
#' @importFrom purrr map
#' @importFrom tidyr unnest_wider
pedigree_pairs <- function(pedigree, studbook) {
  studbook <- studbook %>% mutate(across(c(Sire, Dam), as.character))
  pairs    <- map(as.list(nonfounders(pedigree)), \(x) as.list(parents(pedigree, id = x))) %>%
    enframe(name = NULL) %>%
    unnest_wider(value, names_sep = "_") %>%
    distinct() %>%
    mutate(pid = paste0("0", as.character(row_number()))) %>%
    left_join(studbook, by = join_by(value_1 == Sire, value_2 == Dam)) %>%
    select(pid, fid = value_1, mid = value_2, color = LocBirth_color, label = LocBirth) %>%
    distinct() %>%
    group_by(pid) %>%
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>%
    ungroup()
  return(pairs)
}

#' Generate edge list for pedigree visNetwork plot
#'
#' @param pedigree A pedigree object
#' @param studbook A studbook tibble
#' @return A tibble of edges with visual attributes
#' @export
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom tibble enframe
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr transmute
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom rlang set_names
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr unnest_longer
pedigree_edges <- function(pedigree, studbook) {
  pairs <- pedigree_pairs(pedigree, studbook)
  sire  <- pull(pairs, fid, pid)
  dam   <- pull(pairs, mid, pid)
  pairs.edf <- pairs  %>%
    pivot_longer(cols = c("fid", "mid"), values_to = "from", names_to = NULL) %>%
    select(from, to = pid, color) %>%
    mutate(dashes = TRUE,
           shadow = FALSE,
           width  = 0.5,
           arrows = "to")
  combined         <- c(sire, dam)
  combined_grouped <- split(combined, names(combined)) %>% map(unlist)
  offspring.edf    <- combined_grouped %>%
    imap(function(x, idx) {
      list(idx, as.list(commonDescendants(pedigree, ids = x, maxGen = 2)))
    }) %>%
    enframe(name = NULL, value = "id") %>%
    unnest_wider(id, names_sep = "_") %>%
    unnest_longer(id_2, values_to = "id") %>%
    select(from = id_1, to = id) %>%
    arrange(from, to) %>%
    distinct() %>%
    left_join(select(pairs, from = pid, color), by = join_by(from)) %>%
    mutate(dashes = FALSE,
           shadow = TRUE,
           width  = 0.7,
           arrows = NULL)

  edges <- bind_rows(pairs.edf, offspring.edf)
  return(edges)
}

#' Generate node list for pedigree visNetwork plot
#'
#' @param pedigree A pedigree object
#' @param palette A color palette in list format
#' @param studbook A studbook tibble
#' @return A tibble of nodes with visNetwork styling attributes
#' @export
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr transmute
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom tidyr pivot_longer
pedigree_nodes <- function(pedigree, palette, studbook) {
  levels <- pedigree_levels(pedigree, studbook)
  pairs  <- pedigree_pairs(pedigree, studbook)
  pairs_ndf <- pairs %>%
    pivot_longer(cols = c("fid", "mid"), values_to = "mateID", names_to = NULL) %>%
    left_join(levels, by = join_by(mateID == id)) %>%
    group_by(pid, color, label) %>%
    mutate(level = sum(max(level), 1)) %>% ungroup() %>%
    group_by(pid) %>%
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>%
    ungroup() %>%
    mutate(
      group = "pair",
      shape = "icon",
      icon  = map(color, \(x) list(code = "\uf068", color = x, face = "'Font Awesome 5 Free'", weight = 700, size = 35)),
      font  = map(color, \(x) list(size = 18, color = x, strokeColor = "#FFFFFF", strokeWidth = 3))
    ) %>%
    select(id = pid, level, group, shape, icon, label, font) %>%
    distinct()
  nodes <- as.data.frame(pedigree) %>%
    mutate(id    = as.character(id),
           group = as.character(sex)) %>%
    select(id, group) %>%
    group_by(id) %>%
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>%
    ungroup() %>%
    left_join(levels, by = join_by(id)) %>%
    group_by(id) %>%
    summarize(across(everything(), ~ dplyr::first(.x)), .groups = "drop") %>%
    ungroup() %>%
    mutate(color = case_when(
      group == "0" ~ palette[["u"]],
      group == "1" ~ palette[["m"]],
      group == "2" ~ palette[["f"]]),
      code = case_when(
        group == "0" ~ "\uf04b",
        group == "1" ~ "\uf0c8",
        group == "2" ~ "\uf111"),
      shape = "icon",
      label = id
    ) %>%
    mutate(icon = map2(color, code, \(x, y) list(code = y,
                                                 color = x,
                                                 face = "'Font Awesome 5 Free'",
                                                 weight = 700,
                                                 size = 30)),
           font = map(color, \(x) list(size = 12,
                                       color = x,
                                       strokeColor = "#FFFFFF",
                                       strokeWidth = 3))
    ) %>%
    select(id, level, group, shape, icon, label, font) %>%
    bind_rows(pairs_ndf) %>%
    distinct(id, .keep_all = TRUE) %>%
    ungroup()
  return(nodes)
}

#' Build visNetwork visualization of a pedigree
#'
#' @param pedigree A pedigree object
#' @param studbook A studbook tibble
#' @param palette A color palette in list format
#' @return A visNetwork plot object
#' @export
#'
#' @importFrom dplyr case_when
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom visNetwork addFontAwesome
#' @importFrom visNetwork visInteraction
#' @importFrom visNetwork visLegend
#' @importFrom visNetwork visNetwork
#' @importFrom visNetwork visNodes
ped_network <- function(pedigree, palette, studbook) {
  edges  <- pedigree_edges(pedigree, studbook) %>% distinct()
  nodes  <- pedigree_nodes(pedigree, palette, studbook) %>% distinct()
  legend <- nodes %>%
    select(group, shape, icon) %>%
    distinct() %>%
    mutate(icon = if_else(group %in% c("0", "1", "2"), icon, map(icon, \(x) list_assign(x, color = "#444444")))) %>%
    mutate(label = case_when(
      group == "0" ~ "Sex Undet",
      group == "1" ~ "Male",
      group == "2" ~ "Female",
      .default = "Pair colored/labeled by Location"
    )) %>% distinct()
  network <-  visNetwork(
             nodes,
             edges) %>%
    addFontAwesome(version = "5.13.0") %>%
    visNodes(shadow = TRUE,
             fixed  = list(x = FALSE, y = FALSE)
             ) %>%
    visEdges(smooth = FALSE) %>%
    visPhysics(stabilization = FALSE) %>%
    visLegend(addNodes  = legend,
              ncol      = 1,
              useGroups = FALSE)
  return(network)
}

#' Subset pedigree network to visualize match lineage
#'
#' @param pedigree.living A pedigree object for the living population
#' @param palette A color palette in list format
#' @param pair A character vector of two individual IDs (male, female)
#' @param studbook A tibble containing metadata for the full population
#'
#' @return A visNetwork object showing related individuals and lineage paths
#' @export
#'
#' @importFrom dplyr anti_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom purrr list_assign
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr setdiff
#' @importFrom dplyr union
#' @importFrom glue glue
#' @importFrom pedtools unrelated
#' @importFrom purrr map2
#' @importFrom verbalisr verbalise
#' @importFrom visNetwork addFontAwesome
#' @importFrom visNetwork visInteraction
#' @importFrom visNetwork visNetwork
#' @importFrom visNetwork visNodes
subset_network_btp <- function(pedigree.living, palette, pair, studbook) {
  male   <- pair[1]
  female <- pair[2]

  edges <- pedigree_edges(pedigree.living, studbook) %>% distinct()
  nodes <- pedigree_nodes(pedigree.living, palette, studbook) %>% distinct()
  result <- verbalise(pedigree.living, ids = pair)
  related.pair <- setdiff(
    pedigree.living$ID,
    union(unrelated(pedigree.living, male), unrelated(pedigree.living, female))
  )

  # Highlight paths connecting proposed pair
  connections <- edges %>%
    filter(to %in% c(pair, result[[1]]$v1, result[[1]]$v2)) %>%
    pull(from) %>%
    unique()

  paths <- edges %>%
    filter(to %in% union(related.pair, pair) | from %in% union(related.pair, pair)) %>%
    filter(to %in% c(pair, result[[1]]$v1, result[[1]]$v2, connections)) %>%
    mutate(color = palette[["emph"]], shadow = TRUE, width = 3)

  pair.edges <- edges %>%
    filter(to %in% union(related.pair, pair) | from %in% union(related.pair, pair)) %>%
    anti_join(paths, by = c("from", "to")) %>%
    bind_rows(paths) %>%
    distinct()

  node.ids <- unique(c(pair.edges$from, pair.edges$to))

  # Styling logic
  style_icons <- function(icon, group) {
    if (group %in% c("match.m", "match.f")) {
      list_assign(icon, size = 55)
    } else if (group == "match.anc") {
      list_assign(icon, size = 45)
    } else {
      icon
    }
  }

  style_font <- function(font, group) {
    if (group %in% c("match.m", "match.f", "match.anc")) {
      list_assign(font, size = 18, background = palette[["emph"]])
    } else if (group %in% c("match.rel.m", "match.rel.f")) {
      list_assign(font, size = 14)
    } else {
      font
    }
  }

  style_color <- function(group) {
    if (group %in% c("match.m", "match.f", "match.anc")) {
      list(background = palette[["emph"]], border = "#000000")
    } else {
      NULL
    }
  }

  pair.nodes <- nodes %>%
    filter(id %in% node.ids) %>%
    mutate(
      label = case_when(
        id == male   ~ "Male Partner",
        id == female ~ "Female Partner",
        id %in% result[[1]]$anc ~ "Shared Ancestor",
        TRUE ~ label
      ),
      group = case_when(
        id == male   ~ "match.m",
        id == female ~ "match.f",
        id %in% result[[1]]$anc ~ "match.anc",
        id %in% result[[1]]$v1  ~ "match.rel.m",
        id %in% result[[1]]$v2  ~ "match.rel.f",
        TRUE ~ group
      ),
      icon  = map2(icon, group, style_icons),
      font  = map2(font, group, style_font),
      color = map(group, style_color),
      borderWidth = case_when(
        group %in% c("match.m", "match.f")     ~ 3,
        group %in% c("match.rel.m", "match.rel.f") ~ 1.5,
        group == "match.anc"                     ~ 2,
        TRUE                                      ~ 1
      )
    ) %>%
    distinct()

  subtitle <- glue::glue(
    "{male} & {female} are {result[[1]]$rel}<br>Path Connecting Pair: {result[[1]]$path}"
  )

 network <-  visNetwork(pair.nodes,
                        pair.edges,
             main = "Relatives of Proposed Match", submain = subtitle) %>%
    addFontAwesome(version = "5.13.0") %>%
    visNodes(shadow = TRUE, fixed = list(x = FALSE, y = FALSE)) %>%
    visEdges(smooth = FALSE) %>%
    visPhysics(stabilization = FALSE)
 return(network)
}

