# palettes.R
# Utility functions for managing color palettes across plots and pedigrees

#' Generate fill color mapping for pedigree plots
#'
#' @param pedigree A pedigree object created by `pedtools::ped()`
#' @param studbook A tibble of studbook data
#' @param palette A named list of color values
#' @return A named list mapping individuals to colors by sex and status
#' @export
#'
#' @importFrom dplyr intersect
#' @importFrom dplyr setdiff
#' @importFrom dplyr union
#' @importFrom pedtools females
#' @importFrom pedtools males
#' @importFrom purrr list_assign
#' @importFrom purrr keep_at
set_ped_fills <- function(pedigree, palette, studbook) {
  female <- intersect(
    living(studbook),
    females(pedigree)
    )
  male   <- intersect(
    living(studbook),
    males(pedigree)
    )
  undet  <- setdiff(
    living(studbook),
    union(female, male)
    )
  female.d <- intersect(
    deceased(studbook),
    females(pedigree)
    )
  male.d   <- intersect(
    deceased(studbook),
    males(pedigree)
    )
  undet.d  <- setdiff(
    deceased(studbook),
    union(female.d, male.d)
    )

  fills        <- list(female,
                       male,
                       undet)
  names(fills) <- keep_at(palette,
                          c("f", "m", "u")
                          )
  ped.fills <- list(
    "#D5328870" = female.d,
    "#3F459B70" = male.d,
    "#21B14B70" = undet.d
  )

  fills <- list_assign(fills, !!!ped.fills)
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
  col.pal <- keep_at(palette, c("f", "m", "u")) %>% unlist()
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
