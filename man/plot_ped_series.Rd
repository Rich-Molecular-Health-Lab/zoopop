% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ped_base_plot.R
\name{plot_ped_series}
\alias{plot_ped_series}
\title{Plot a Series of Pedigree Objects}
\usage{
plot_ped_series(pedigree_series, studbook, palette = NULL, labs_spec = FALSE)
}
\arguments{
\item{pedigree_series}{A list of pedigree objects.}

\item{studbook}{A data frame containing studbook metadata.}

\item{palette}{Optional. A named color palette; defaults to \code{set_colors()}.}
}
\value{
A list of pedigree plots.
}
\description{
This function generates a plot for each pedigree in a series using \code{plot_pedigree()}.
}
