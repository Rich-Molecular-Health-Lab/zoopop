# igraph.R

#' Create Vertices Data for an igraph Pedigree Graph
#'
#' This function builds a vertices data frame for constructing an igraph object from pedigree data,
#' applying custom colors, shapes, and sizes.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of vertices with plotting attributes.
#' @export
#' @importFrom dplyr mutate case_match case_when select bind_rows arrange
#' @importFrom stringr str_starts str_ends
ped_verts <- function(studbook, pedigree) {
  colors   <- set_colors()
  subjects <- nodes_subjects(studbook = studbook,
                             pedigree = pedigree) %>%
    mutate(
      color = case_match(
        group,
        "male_included"       ~colors[["m"]],
        "female_included"     ~colors[["f"]],
        "male_deceased"       ~colors[["sire"]],
        "female_deceased"     ~colors[["dam"]],
        "male_excluded"       ~colors[["m"]],
        "female_excluded"     ~colors[["f"]],
        "male_hypothetical"   ~colors[["sire"]],
        "female_hypothetical" ~colors[["dam"]],
        "undetermined"        ~colors[["u"]]
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
      label.cex = 0.25
    ) %>%
    select(
      id,
      label,
      label.cex,
      color,
      frame.color,
      shape,
      size,
      generation
    )
  subnucs <- nodes_subnucs(pedigree = pedigree) %>%
    select(id,
           generation) %>%
    mutate(shape = "circle",
           color = "gray",
           size  = 1)

  vertices <- bind_rows(subjects, subnucs) %>%
    arrange(id)
  return(vertices)
}

#' Create Edge Data for an igraph Pedigree Graph
#'
#' This function constructs an edge data frame for building an igraph object using pedigree data.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return A data frame of edges with attributes for igraph rendering.
#' @export
#' @importFrom dplyr select mutate
edges_igraph <- function(studbook, pedigree) {
  ped_edges(studbook = studbook,
            pedigree = pedigree) %>%
    select(from, to, color, rel) %>%
    mutate(arrow.size = .1,
           curved     = 0,
           width      = 1)
}

#' Build an igraph Object from Pedigree Data
#'
#' This function creates an igraph object from pedigree edge and vertex data.
#'
#' @param studbook A data frame containing studbook metadata.
#' @param pedigree A pedigree object.
#' @return An igraph object representing the pedigree.
#' @export
#' @importFrom igraph graph_from_data_frame
ped_igraph <- function(studbook, pedigree) {
  edges <- edges_igraph(studbook = studbook,
                       pedigree = pedigree)
  vertices <- ped_verts(studbook = studbook,
                       pedigree = pedigree)
  graph <- graph_from_data_frame(edges, vertices = vertices)
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
#' @export
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
