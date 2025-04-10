% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pedigrees.R
\name{ped_graph}
\alias{ped_graph}
\title{Create a pedigree graph using \code{DiagrammeR}}
\usage{
ped_graph(nodes, edges)
}
\arguments{
\item{nodes}{A node dataframe created using \code{ped_ndf}}

\item{edges}{An edge dataframe created using \code{ped_edf}}
}
\value{
A formatted graphvis object for rendering in \code{DiagrammeR}
}
\description{
Create a pedigree graph using \code{DiagrammeR}
}
