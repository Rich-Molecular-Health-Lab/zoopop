# ped_base_plot.R

#' Plot a Pedigree with Custom Formatting
#'
#' This function plots a single pedigree object using custom formatting options,
#' highlighting deceased individuals and applying a custom color palette.
#'
#' @param pedigree A pedigree object created with \code{pedtools::ped()}.
#' @param studbook A data frame containing studbook metadata.
#' @param name Optional. A title for the plot. Defaults to the pedigree family ID.
#' @param palette Optional. A named list mapping individuals to colors; defaults to \code{set_colors()}.
#' @param labs_spec Logical. If \code{TRUE}, special labels defined in \code{name_spec} are emphasized.
#' @param ... Additional arguments passed to \code{pedtools::plot()}.
#' @return A \code{plotTag} object containing the pedigree plot.
#' @export
#' @importFrom dplyr filter setdiff filter select
#' @importFrom htmltools plotTag
#' @importFrom pedtools famid
#' @importFrom rlang set_names
#' @importFrom tibble deframe
plot_pedigree <- function(pedigree, studbook, name = NULL, palette = NULL, labs_spec = FALSE, ...) {
  ped_ids <- c(pedigree[["ID"]])
  if (is.null(palette)) {
    palette <- set_colors()
  }
  if (is.null(name)) {
    name <- pedtools::famid(pedigree)
  }
  if (labs_spec == TRUE) {
    labs <- studbook_short(studbook) %>% filter(!is.na(name_spec)) %>%
      select(name_spec, ID) %>%
      deframe()
    starred  <- labs
    lwd_spec <- labs %>% set_names(., 0.7)
    lwd_def  <- setdiff(ped_ids, labs) %>% set_names(., 0.3)
    lwd      <- as.list(c(lwd_spec, lwd_def))
  } else {
    labs    <- NULL
    starred <- NULL
    lwd     <- 0.3
  }
  ped.palette  <- set_ped_fills(palette, studbook)
  deceased_ids <- deceased(studbook)
  plotTag(
    expr = plot(
      pedigree,
      title    = name,
      cex      = 0.4,
      deceased = deceased_ids,
      labs     = labs,
      starred  = starred,
      fill     = ped.palette,
      lwd      = lwd,
      col      = "black",
      pty      = "m",
      ...
    ),
    alt    = "pedigree-plot",
    width  = 900,
    height = 900
  )
}

#' Plot a Series of Pedigree Objects
#'
#' This function generates a plot for each pedigree in a series using \code{plot_pedigree()}.
#'
#' @param pedigree_series A list of pedigree objects.
#' @param studbook A data frame containing studbook metadata.
#' @param palette Optional. A named color palette; defaults to \code{set_colors()}.
#' @return A list of pedigree plots.
#' @export
#' @importFrom purrr imap
plot_ped_series <- function(pedigree_series, studbook, palette = NULL, labs_spec = FALSE) {
  if (is.null(palette)) {
    palette <- set_colors()
  }
  imap(pedigree_series, \(x, idx) plot_pedigree(pedigree = x,
                                                studbook = studbook,
                                                name     = idx,
                                                palette  = palette,
                                                labs_spec= labs_spec)
  )
}

#' Save a Pedigree Plot to HTML
#'
#' This function saves a pedigree plot as an HTML file in the local directory \code{zoopop_plots}.
#'
#' @param pedigree A pedigree object.
#' @param studbook A data frame containing studbook metadata.
#' @param name Optional. The title for the plot; defaults to the pedigree family ID.
#' @param palette Optional. A named list mapping individuals to colors.
#' @param labs_spec Logical. If \code{TRUE}, emphasizes individuals with special labels.
#' @return None. The function saves an HTML file.
#' @export
#' @importFrom htmltools save_html
#' @importFrom pedtools famid
pedsave <- function(pedigree, studbook, name = NULL, palette = NULL, labs_spec = FALSE) {
  if (is.null(palette)) {palette <- set_colors()}
  if (is.null(name)) {name <- pedtools::famid(pedigree)}
  ped.palette  <- set_ped_fills(palette, studbook)
  deceased_ids <- deceased(studbook)
  out_dir <- file.path("zoopop_plots")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }
  plot <- plot_pedigree(pedigree, studbook, name = name, palette = palette, labs_spec = labs_spec)
  htmltools::save_html(plot, paste0(out_dir, "/ped_plot_", name, ".html"))
}
