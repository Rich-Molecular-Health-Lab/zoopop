% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lifetab.R
\name{vitals}
\alias{vitals}
\title{Caclulate basic vital stats to generate population projections and life tables}
\usage{
vitals(studbook, span = 5)
}
\arguments{
\item{span}{integer representing the width of each birth-cohort in years}

\item{stubook}{tibble organized and formatted originally using \code{read_studbook}}
}
\value{
Tibble with one row per birth cohort and a tibble-column called \code{data} which contains
vital stats to compute life tabls and projections, where \code{x} = age class in years since birth,
\code{Sx} = census count, \code{Dx} = deaths as raw count, \code{births} = births as raw count,
\code{bx} = per-capita birthrate (\code{births/Sx})
}
\description{
Caclulate basic vital stats to generate population projections and life tables
}
