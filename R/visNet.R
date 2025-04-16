# visNet.R

#' Create Node Data for visNetwork Pedigree Graphs
#'
#' This function builds a node data frame for rendering a pedigree graph using visNetwork,
#' combining subject and subnucleus nodes.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of nodes formatted for visNetwork.
#' @export
#' @importFrom dplyr select bind_rows arrange
ped_visNodes <- function(studbook, pedigree) {
  subjects <- nodes_subjects(studbook = studbook,
                             pedigree = pedigree) %>%
    select(id,
           label,
           group,
           level = generation,
           title
    )
  subnucs  <- nodes_subnucs(pedigree = pedigree) %>%
    select(id,
           group,
           level = generation,
           value = n_kids)
  nodes <- bind_rows(subjects, subnucs) %>%
    arrange(id)
  return(nodes)
}

#' Create Edge Data for visNetwork Pedigree Graphs
#'
#' This function constructs an edge data frame for a pedigree graph formatted for visNetwork.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of edges formatted for visNetwork.
#' @export
#' @importFrom dplyr select mutate if_else
#' @importFrom magrittr %>%
ped_visEdges <- function(studbook, pedigree) {
  colors <- set_colors()
  edges <- ped_edges(studbook = studbook,
                     pedigree = pedigree) %>%
    mutate(color = if_else(rel %in% c("children", "subnucs"),
                           colors[["u"]],
                           color)) %>%
    select(from,
           to,
           color,
           rel)
  return(edges)
}

#' Set Custom Group Appearance for a Specific Node Group in a visNetwork Graph
#'
#' This function assigns a custom image to a specific node group in a visNetwork graph.
#'
#' @param graph A visNetwork object.
#' @param groupname A character string representing the group name.
#' @return The visNetwork object with the specified group settings applied.
#' @export
#' @importFrom visNetwork visGroups
pedNode_visGroup <- function(graph, groupname) {
  path <- "https://rich-molecular-health-lab.github.io/zoopop/inst/icons/"
  file <- paste0(path, groupname, ".png")
  if (groupname %in% c("connector", "offspring", "parents")) {
    size <- 5
    } else { size <- 40 }
  visGroups(graph     = graph,
            groupname = groupname,
            shape     = "image",
            image     = file,
            size      = size)
}

#' Apply Custom Group Settings to a visNetwork Pedigree Graph
#'
#' This function applies multiple predefined group settings for various pedigree groups to a visNetwork object.
#'
#' @param graph A visNetwork object.
#' @return A modified visNetwork object with custom group appearances.
#' @export
ped_visGroups <- function(graph) {
  pedNode_visGroup(graph = graph,   groupname = "female_deceased"    ) %>%
    pedNode_visGroup(               groupname = "male_deceased"      ) %>%
    pedNode_visGroup(               groupname = "female_included"    ) %>%
    pedNode_visGroup(               groupname = "male_included"      ) %>%
    pedNode_visGroup(               groupname = "female_excluded"    ) %>%
    pedNode_visGroup(               groupname = "male_excluded"      ) %>%
    pedNode_visGroup(               groupname = "female_hypothetical") %>%
    pedNode_visGroup(               groupname = "male_hypothetical"  ) %>%
    pedNode_visGroup(               groupname = "undetermined"       ) %>%
    pedNode_visGroup(               groupname = "offspring"          ) %>%
    pedNode_visGroup(               groupname = "parents"            ) %>%
    pedNode_visGroup(               groupname = "hub"                ) %>%
    pedNode_visGroup(               groupname = "connector"          )

}

#' Define Icon Settings for Pedigree Groups
#'
#' This function returns a list of FontAwesome icon settings for various pedigree groups.
#'
#' @return A list of icon settings.
#' @export
ped_groupIcons <- function() {
  colors <- set_colors()
  big    <- 75
  med    <- 60
  sml    <- 5

  big.f    <- big + 10
  med.f    <- med + 10
  sml.f    <- sml + 10

  icons <- list(
    undetermined        = list(code  = "f219",
                               size  = big,
                               color = colors[["u"]]),
    female_included     = list(code  = "f111",
                               size  = big.f,
                               color = colors[["f"]]),
    female_excluded     = list(code  = "f056",
                               size  = big.f,
                               color = colors[["f"]]),
    female_deceased     = list(code  = "f023",
                               size  = med.f,
                               color = colors[["f"]]),
    female_hypothetical = list(code  = "f47e",
                               size  = med.f,
                               color = colors[["f"]]),
      male_included     = list(code  = "f0c8",
                               size  = big,
                               color = colors[["m"]]),
      male_excluded     = list(code  = "f146",
                               size  = big,
                               color = colors[["m"]]),
      male_deceased     = list(code  = "f2d3",
                               size  = med,
                               color = colors[["m"]]),
      male_hypothetical = list(code  = "f0fd",
                               size  = med,
                               color = colors[["m"]]),
            offspring = list(code  = "f22d",
                             size  = sml,
                             color = colors[["u"]]),
            parents   = list(code  = "f22d",
                             size  = sml,
                             color = colors[["emp"]]),
    connector   = list(code  = "f22d",
                       size  = sml,
                       color = colors[["emp"]]),
    hub   = list(code  = "f22d",
                       size  = sml,
                       color = colors[["emp"]])
  )
}

#' Apply Custom Icon Settings to a visNetwork Pedigree Graph
#'
#' This function applies custom FontAwesome icon settings for various pedigree groups to a visNetwork object.
#'
#' @param graph A visNetwork object.
#' @return A modified visNetwork object with custom icon groups applied.
#' @export
#' @importFrom fontawesome fa
#' @importFrom visNetwork visGroups addFontAwesome
ped_visIcons <- function(graph) {
  icons  <- ped_groupIcons()
  dimmed <- 0.8
    visGroups(graph     = graph,
              groupname = "female_included",
              shape     = "icon",
              icon      = icons[["female_included"]],
              shadow    = TRUE) %>%
    visGroups(groupname = "female_excluded",
              shape     = "icon",
              icon      = icons[["female_excluded"]],
              opacity   = dimmed) %>%
    visGroups(groupname = "female_deceased",
              shape     = "icon",
              icon      = icons[["female_deceased"]],
              opacity   = dimmed) %>%
      visGroups(groupname = "female_hypothetical",
                shape     = "icon",
                icon      = icons[["female_hypothetical"]],
                opacity   = dimmed) %>%
      visGroups(groupname = "male_included",
                shape     = "icon",
                icon      = icons[["male_included"]],
                shadow    = TRUE) %>%
      visGroups(groupname = "male_excluded",
                shape     = "icon",
                icon      = icons[["male_excluded"]],
                opacity   = dimmed) %>%
      visGroups(groupname = "male_deceased",
                shape     = "icon",
                icon      = icons[["male_deceased"]],
                opacity   = dimmed) %>%
      visGroups(groupname = "male_hypothetical",
                shape     = "icon",
                icon      = icons[["male_hypothetical"]],
                opacity   = dimmed) %>%
      visGroups(groupname = "offspring",
                shape     = "icon",
                icon      = icons[["offspring"]]) %>%
      visGroups(groupname = "parents",
                shape     = "icon",
                icon      = icons[["parents"]]) %>%
      visGroups(groupname = "undetermined",
                shape     = "icon",
                icon      = icons[["undetermined"]]) %>%
    addFontAwesome()
}

#' Build an Interactive visNetwork Pedigree Graph
#'
#' This function creates an interactive pedigree graph using visNetwork by combining nodes,
#' edges, and custom icon settings.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A visNetwork object representing the pedigree graph.
#' @export
#' @importFrom visNetwork visNetwork visEdges visInteraction
#' @importFrom dplyr mutate
ped_visNet <- function(studbook, pedigree) {
  nodes <- ped_visNodes(
    studbook = studbook,
    pedigree = pedigree
    ) %>%
    mutate(n = max(id))
  edges <- ped_visEdges(
    studbook = studbook,
    pedigree = pedigree
    )
  graph <- visNetwork(nodes  = nodes,
                      edges  = edges,
                      width  = "100%") %>%
    visEdges(width = 1.5) %>%
    ped_visIcons() %>%
    visInteraction(tooltipDelay = 10,
                   tooltipStyle = "visibility:hidden")
  return(graph)
}

#' Create a Hierarchical visNetwork Pedigree Tree
#'
#' This function applies a hierarchical layout to a visNetwork pedigree graph.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A visNetwork object with a hierarchical layout applied.
#' @export
#' @importFrom visNetwork visHierarchicalLayout visPhysics
ped_visTree <- function(studbook, pedigree) {
  ped_visNet(studbook = studbook,
             pedigree = pedigree) %>%
  visHierarchicalLayout(
    levelSeparation = 400,
    nodeSpacing     = 150,
    sortMethod      = "directed"
  ) %>%
    visPhysics(enabled = FALSE)
}
