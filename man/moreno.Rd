\name{moreno}
\alias{moreno}
\title{Sociometric Indexes}
\usage{
moreno(x, ...)
}
\arguments{
  \item{x}{either a file path to CSV file or a square
  logical matrix}

  \item{...}{additional parameters for
  \code{\link{import.csv}}}
}
\description{
Main package function that calculates sociometric indexes
(both individual and group ones). For now it only supports
data imports from CSV format (see \code{\link{import.csv}}
for details).
}
\details{
Upon successful calculation, a list with custom class
(\code{moreno}) is returned with following items: \itemize{
\item \code{data} - holds logical square matrix with user
votes \item \code{counts} - numeric matrix with positive
and negative vote counts \item \code{individual} - numeric
matrix with individual sociometric indexes, namely
\emph{acception index} (\code{Ia}), \emph{rejection index}
(\code{Ir}) and \emph{social status index} (\code{Iss})
\item \code{group} - list of 2 numeric values: \emph{group
cohesion index} (\code{cohesion}) and \emph{group tension
index} (\code{tension}). Each value has \code{indices}
attribute with matrix indices where mutual
acception/rejection has occured. }
}

