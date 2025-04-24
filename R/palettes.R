# palettes.R
# Utility functions for managing color palettes and symbols across plots and pedigrees

#' Generate symbol mapping for plotly plots
#'
#' If you do not wish to assign custom marker symbols, this will apply default symbols to your named list that are referenced in other functions in this package.
#'
#' @param f    A single symbol to map to females
#' @param m    A single symbol to map to males
#' @param u    A single symbol to map to undetermined sex
#' @param t    A single symbol to map to totals
#' @return A named list of symbols to reference in plotly functions and plots
#' @export
#'
set_markers <- function(f = "circle", m = "square", u = "diamond", t = "cross") {
  symbols <- list(f = f, m = m, u = u, t = t)
  return(symbols)
}

#' Generate color mapping for reactable tables
#'
#' If you do not wish to assign custom color values, this will apply default colors to your named list that are referenced in other functions in this package.
#'
#' @param seq  A named list of color values to use for sequential mapping
#' @param div  A named list of color values to use for divergent mapping
#' @param rnd  A named list of color values to use for randomized, qualitative mapping
#' @param emp  A single color to map to emphasized values
#' @param f    A single color to map to females
#' @param m    A single color to map to males
#' @param u    A single color to map to undetermined sex
#' @param sire A single color to map to sires
#' @param dam  A single color to map to dams
#' @return A named list of color palettes to reference in reactable functions and plots
#' @export
#'
#'@importFrom paletteer paletteer_d
set_colors <- function(seq  = NULL,
                       div  = NULL,
                       rnd  = NULL,
                       emp  = "#B24422FF",
                       f    = "#C44D76FF",
                       m    = "#4457A5FF",
                       u    = "#7CAF5CFF",
                       t    = "#59386CFF") {
  if (is.null(seq)) {
   seq.pal  <- function() { paletteer_d("rcartocolor::Sunset") }
  } else {
    seq.pal <- function() { as.list(seq) }
    }
  if (is.null(div)) {
    div.pal <- function() { paletteer_d("rcartocolor::Temps") }
  } else {
    div.pal <- function() { as.list(div) }
    }
  if (is.null(rnd)) {
    rnd.pal <- function() { paletteer_d("khroma::stratigraphy") }
  } else {
    rnd.pal <- function() { as.list(rnd) }
    }
  palette <- list(seq  = seq.pal(),
                  div  = div.pal(),
                  rnd  = rnd.pal(),
                  emp  = emp ,
                  f    = f   ,
                  m    = m   ,
                  u    = u   ,
                  t    = t   ,
                  sire = gsub("FF", "33", m),
                  dam  = gsub("FF", "33", f) )
  return(palette)
}

#'
#' Generate fill color mapping for pedigree plots
#'
#' @param studbook A data frame of studbook data produced by \code{read_studbook}.
#' @param palette A named list of color values
#' @return A named list mapping individuals to colors by sex and status
#' @export
#'
#' @importFrom dplyr intersect setdiff union
#' @importFrom purrr list_assign keep_at
set_ped_fills <- function(palette = NULL, studbook) {
  if (is.null(palette)) { palette <- set_colors() }
  pal.light <- lighten_palette(palette, "70")
  female       <- living(  studbook, "females"     )
  male         <- living(  studbook, "males"       )
  undet        <- living(  studbook, "undetermined")
  female.d     <- deceased(studbook, "females"     )
  male.d       <- deceased(studbook, "males"       )
  undet.d      <- deceased(studbook, "undetermined")
  fills        <- list(female, male, undet, female.d, male.d, undet.d) %>%
    set_names(list(
                palette[["f"]],
                palette[["m"]],
                palette[["u"]],
                pal.light[["f"]],
                pal.light[["m"]],
                pal.light[["u"]]))
  return(fills)
}

#' Convert a named list palette into named vector format
#'
#' @param palette A named list of color values
#' @return A named vector suitable for plotly or reactable inputs
#' @export
#'
#' @importFrom purrr keep_at
#' @importFrom stats setNames
set_plotly_pal <- function(palette) {
  col.pal <- keep_at(palette, c("f", "m", "t")) %>% unlist()
  col.pal <- setNames(col.pal, c("F", "M", "Total"))
  return(col.pal)
}

#' Lighten color palette by replacing hex alpha value
#'
#' @param palette A named list or vector of colors
#' @param hex A two-character hex string (e.g., "33")
#' @return Palette with adjusted transparency
#' @export
#'
#' @importFrom purrr map_depth
#'
lighten_palette <- function(palette, hex) {
  if (is.list(palette)) {
    new <- map_depth(palette, 1, ~ gsub("FF", hex, .x))
  } else {
    new <- gsub("FF", hex, palette)
  }
  return(new)
}

#' Lighten a named vector of plotly colors
#'
#' @param palette A named vector of hex colors
#' @param hex A two-character hex code (e.g., "26" or "33")
#' @return A modified vector of colors with lighter alpha values
#' @export
#'
#' @importFrom stats setNames
lighten_plotly_pal <- function(palette, hex = "33") {
  if (is.list(palette)) {
    palette <- unlist(palette)
  }
  # Replace only the trailing alpha component
  palette <- gsub("([A-Fa-f0-9]{6})FF$", paste0("\\1", hex), palette)
  stats::setNames(palette, names(palette))
}
