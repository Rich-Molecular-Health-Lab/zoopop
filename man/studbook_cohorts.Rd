% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/life_tables.R
\name{studbook_cohorts}
\alias{studbook_cohorts}
\title{Label studbook rows by Birth Cohorts}
\usage{
studbook_cohorts(
  studbook,
  Year_min = NULL,
  Year_max = NULL,
  span = 10,
  age_max = NULL
)
}
\arguments{
\item{studbook}{A data frame of studbook data produced by \code{read_studbook}.}

\item{Year_min}{Start year of cohorts (optional)}

\item{Year_max}{End year (optional)}

\item{span}{Years per cohort (optional)}

\item{age_max}{Max age to include (optional)}
}
\value{
A joined and restructured tibble
}
\description{
Label studbook rows by Birth Cohorts
}
