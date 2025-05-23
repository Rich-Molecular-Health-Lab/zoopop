% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{demog_intercept}
\alias{demog_intercept}
\title{Calculate a y-intercept value to use for non-age-specific demographic variables}
\usage{
demog_intercept(studbook, cohort_params = NULL, variable, log_trans = FALSE)
}
\arguments{
\item{studbook}{Studbook tibble}

\item{cohort_params}{A named list of the parameter values to use for cohorts (\code{Year_min}, \code{Year_max}, \code{span}, \code{age_max}) (optional)}

\item{variable}{Name of the column containing an age-specific variable to map onto the y-axis
age-specific options for variable: \code{Births}, \code{Deaths}, \code{Nx}, \code{Qx}, \code{Lx}, \code{Lx1}, \code{Px}, \code{ex}, \code{Tx}, \code{Mx}, \code{Fx}
options for lifetime variable: \code{N0}, \code{N1}, \code{R0}, \code{T}, \code{MLE}, \code{Repro_first}, \code{Repro_last}, \code{age_max}, \code{lambda}, \code{r}}

\item{log_trans}{Logical indicating whether to log-transform the y-axis variable}
}
\value{
A list of parameter values to use in plotly plots of demographic variables
}
\description{
Calculate a y-intercept value to use for non-age-specific demographic variables
}
\keyword{internal}
