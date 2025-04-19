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
              size       = 18,
              background = background
            ),
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
visPed_options <- function(graph, variable = NULL) {
  if (is.null(variable)) {
    variable <- "breeding"
  }
  if (variable == "breeding") {
    main <- "Select by breeding inclusion"
  } else if (variable == "Year_birth") {
    main <- "Select by Birth Year"
  } else if (variable == "Institution_birth") {
    main <- "Select by Birth Location"
  }
  graph_out <-  visOptions(
               graph            = graph,
               highlightNearest = list(enabled   = TRUE,
                                       degree    = 4,
                                       hover     = FALSE),
               selectedBy       = list(variable  = variable,
                                       multiple  = TRUE,
                                       main      = main),
               collapse         = list(enabled   = TRUE)
    )
  return(graph_out)
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
  graph_out <-   visInteraction(
                   graph                = graph,
                   keyboard             = TRUE,
                   zoomSpeed            = 0.8,
                   tooltipStyle         = "position: fixed;visibility:hidden;padding: 5px;font-family: verdana; font-size:14px;font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;*-webkit-border-radius: 3px;border-radius: 3px; border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);max-width:400px;word-break: break-all",
                   hoverConnectedEdges  = TRUE,
                   multiselect          = TRUE,
                   navigationButtons    = TRUE,
                   selectable           = TRUE,
                   selectConnectedEdges = TRUE,
                   hideEdgesOnDrag      = TRUE)
  return(graph_out)
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
      breeding = exclude,
      Year_birth,
      Institution_birth
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
           arrows
    )
  graph <- visNetwork(nodes  = nodes,
                      edges  = edges,
                      height = "800px") %>%
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
visPed_tree <- function(studbook, pedigree, variable = NULL) {
  if (is.null(variable)) {
    variable <- "breeding"
  }
  graph <- visPed(studbook = studbook, pedigree = pedigree)
  tree  <-  visPed_options(graph = graph, variable = variable) %>%
    visPhysics(enabled = FALSE) %>%
    visPed_interaction() %>%
    visHierarchicalLayout(
      nodeSpacing     = 200,
      levelSeparation = 300,
      sortMethod      = "directed",
      shakeTowards    = "roots"
    )
  return(tree)
}
