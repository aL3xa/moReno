\name{sociogram}
\alias{sociogram}
\title{Sociogram}
\usage{
sociogram(m, group = NULL, group.colors = NULL, arrow.colors = NULL)
}
\arguments{
  \item{m}{a \code{moreno}-classed object}

  \item{group}{additional vector for group affiliation
  (defaults to \code{NULL})}

  \item{group.colors}{character vector for group colors
  (defaults to \code{NULL})}

  \item{arrow.colors}{character vector for arrow colors
  (defaults to \code{NULL})}
}
\description{
Plots sociogram for given \code{moreno}-classed object.
Uses \code{igraph} library and \emph{Fruchterman-Reingold}
layout to plot sociogram.
}

