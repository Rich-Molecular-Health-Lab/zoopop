% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{living}
\alias{living}
\title{Gather IDs of all living individuals}
\usage{
living(studbook, include = "all")
}
\arguments{
\item{studbook}{A data frame of studbook data produced by \code{read_studbook}.}

\item{include}{A string specifying whether to return \code{"all"} living individuals, only \code{"females"}, or only \code{"males"}.}
}
\value{
A vector containing a subset of individual IDs
}
\description{
Gather IDs of all living individuals
}
