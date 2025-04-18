% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pedigrees.R
\name{ped_edges}
\alias{ped_edges}
\title{Create Edges to connect all nodes for multiple visualization tools}
\usage{
ped_edges(studbook, pedigree)
}
\arguments{
\item{studbook}{A data frame containing studbook metadata.}

\item{pedigree}{A pedigree object.}
}
\value{
A data frame of edges linking parents with their children.
}
\description{
This function builds an edge data frame linking all nodes with visualization variables that can be selected
or deselected based on the tool in use.
}
