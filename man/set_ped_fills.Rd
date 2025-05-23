% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/palettes.R
\name{set_ped_fills}
\alias{set_ped_fills}
\title{Generate fill color mapping for pedigree plots}
\usage{
set_ped_fills(palette = NULL, studbook)
}
\arguments{
\item{palette}{A named list of color values}

\item{studbook}{A data frame of studbook data produced by \code{read_studbook}.}
}
\value{
A named list mapping individuals to colors by sex and status
}
\description{
Generate fill color mapping for pedigree plots
}
