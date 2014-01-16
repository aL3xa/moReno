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
index} (\code{tension}) \item \code{categories} - list of
9, each element containing data, either a character vector
with user ids, or a matrix with id pairs of users that fall
into specific sociometric category. Following sociometric
categories are available: \enumerate{ \item \code{loners} -
users who have no votes, nor give votes to others \item
\code{unchosen} - have no votes, but give votes to others
\item \code{rejected} - have only negative votes \item
\code{abstinents} - refuse to give vote to others, but do
get voted \item \code{popular} - have 5 or more positive
votes, and no negative votes \item \code{unpopular} - have
4 or more negative votes, but no positive votes \item
\code{mutual.positive} - pairs of users who give positive
votes mutually \item \code{mutual.negative} - pairs of
users who give negative votes mutually \item
\code{unrequited} - pairs of users who give negative votes
mutually } }
}

