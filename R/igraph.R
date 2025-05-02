# igraph.R

#' Build an igraph Object from Pedigree Data
#'
#' This function creates an igraph object from pedigree edge and vertex data.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return An igraph object representing the pedigree.
#' @noRd
#' @importFrom igraph graph_from_data_frame
#' @importFrom dplyr select
ped_igraph <- function(studbook, pedigree) {
  vertices <- ped_nodes(studbook = studbook, pedigree = pedigree) %>%
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
  edges <- ped_edges(studbook = studbook, pedigree = pedigree) %>%
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

#' Compute Coordinates for Pedigree Nodes Using a Sugiyama Layout
#'
#' This function calculates (x, y) coordinates for nodes in the pedigree igraph
#' using a Sugiyama layout.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame containing vertex IDs and corresponding \code{x} and \code{y} coordinates.
#' @noRd
#' @importFrom igraph layout_with_sugiyama V
#' @importFrom tibble tibble
ped_coords <- function(studbook, pedigree) {
  graph  <- ped_igraph(studbook = studbook,
                       pedigree = pedigree)
  coords <- layout_with_sugiyama(graph)
  xy   <- coords$layout
  vids <- V(graph)
  layout_df <- tibble(
    vertex_id = as.numeric(vids),
    x         = xy[, 1],
    y         = xy[, 2]
  )
  return(layout_df)
}

#' Plot a Pedigree igraph Object
#'
#' This function plots an igraph pedigree using a Sugiyama layout.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A base R plot of the pedigree igraph.
#' @export
#' @importFrom igraph layout_with_sugiyama
ped_plot_igraph <- function(studbook, pedigree) {
  graph  <- ped_igraph(studbook = studbook,
                       pedigree = pedigree)
  layout <- layout_with_sugiyama(graph)
  plot(graph, layout = layout)
}
