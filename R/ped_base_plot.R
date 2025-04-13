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
