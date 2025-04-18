# visNet.R


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
  colors     <- set_colors()
  cols.light <- lighten_palette(colors, "40")
  path <- "https://rich-molecular-health-lab.github.io/zoopop/inst/icons/"
  file <- paste0(path, groupname, ".png")
  if (groupname %in% c("hub", "connector", "offspring", "parents")) {
    size <- 10
  } else { size <- 30 }
  if (str_starts(groupname, "female")) {
    background <- cols.light[["f"]]
  } else if (str_starts(groupname, "male")) {
    background <- cols.light[["m"]]
  } else if (str_starts(groupname, "undetermined")) {
    background <- cols.light[["u"]]
  } else {
    background <- "white"
  }

  visGroups(graph     = graph,
            groupname = groupname,
            shape     = "image",
            image     = file,
            size      = size,
            shadow    = TRUE,
            font      = list(
              size       = 16,
              background = background
            ),
            borderWidthSelected = 3,
            color     = list(
              highlight = list(
                background = background,
                border     = colors[["emp"]]
              )
            )
            )
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

#' Apply Custom Visualization Options to a visNetwork Pedigree Graph
#'
#' This function applies multiple predefined options settings for various pedigree groups to a visNetwork object.
#'
#' @param graph A visNetwork object.
#' @return A modified visNetwork object with custom options.
#' @export
#' @importFrom visNetwork visOptions
#' @importFrom dplyr select arrange
visPed_options <- function(graph) {
  graph <-  visOptions(
               graph            = graph,
               highlightNearest = list(enabled   = TRUE,
                                       degree    = 4,
                                       hover     = TRUE),
               selectedBy       = list(variable  = "exclude",
                                       multiple  = TRUE,
                                       main      = "Select by breeding inclusion")
    )
  return(graph)
}

#' Apply Custom Interaction Options to a visNetwork Pedigree Graph
#'
#' This function applies multiple predefined interaction settings for various pedigree groups to a visNetwork object.
#'
#' @param graph A visNetwork object.
#' @return A modified visNetwork object with custom options.
#' @export
#' @importFrom visNetwork visInteraction
visPed_interaction <- function(graph) {
    visInteraction(graph                = graph,
                   tooltipDelay         = 900,
                   zoomSpeed            = 0.8,
                   tooltipStyle         = "visibility:hidden",
                   hoverConnectedEdges  = TRUE,
                   multiselect          = TRUE,
                   navigationButtons    = TRUE,
                   selectable           = TRUE,
                   selectConnectedEdges = TRUE,
                   hideEdgesOnDrag      = TRUE)
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
#' @importFrom visNetwork visNetwork
#' @importFrom dplyr select
visPed <- function(studbook, pedigree) {
  nodes <- ped_nodes(studbook = studbook, pedigree = pedigree) %>%
    select(
      id = id_node,
      label,
      level,
      group,
      title,
      famid,
      exclude
    ) %>%
    mutate(n = max(id))
  edges <- ped_edges(studbook = studbook, pedigree = pedigree) %>%
    select(from,
           to,
           title = label,
           length,
           width,
           dashes,
           color,
           shadow,
           arrows,
           smooth
    )
  graph <- visNetwork(nodes = nodes, edges = edges) %>%
    ped_visGroups()
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
visPed_tree <- function(studbook, pedigree) {
  graph <- visPed(studbook = studbook, pedigree = pedigree)
  tree  <-  visPed_options(graph = graph) %>%
    visPed_interaction() %>%
    visPhysics(enabled = FALSE)  %>%
    visHierarchicalLayout(
      nodeSpacing     = 200,
      levelSeparation = 500,
      sortMethod      = "directed",
      shakeTowards    = "roots"
    )
  return(tree)
}
