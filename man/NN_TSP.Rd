% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/greedy.R
\name{NN_TSP}
\alias{NN_TSP}
\title{Nearest Neighbour TSP}
\usage{
NN_TSP(data, type = "one")
}
\arguments{
\item{data}{matrix of positions for cities in (x,y)}

\item{type}{"one" or "all" to start from one city only or to repeat the algorithm for all possible starting cities}
}
\value{
a vector of indices ("best" order of the cities to visit)
}
\description{
Nearest Neighbour algorithm for TSP
}
