\name{moreno.import.csv}
\alias{moreno.import.csv}
\title{Import CSV with Sociometric Data}
\usage{
moreno.import.csv(file, upvote.sign = "+", downvote.sign = "-",
  na.strings = c("", "NA"), id.column.index = 1L, ...)
}
\arguments{
  \item{file}{CSV file path}

  \item{upvote.sign}{character string that represents
  upvote (defaults to \code{+})}

  \item{downvote.sign}{character string that represents
  downvote (defaults to \code{-})}

  \item{na.strings}{strings that will be converted to
  \code{NA} (see respective entry for
  \code{\link{read.csv}} for details)}

  \item{id.column.index}{index of ID column (defaults to
  \code{1L})}

  \item{...}{additional params for \code{\link{read.csv}}}
}
\description{
Imports CSV file with votes from sociometric questionnaire
and converts it to logical matrix where upvotes are
recorded as \code{TRUE} and downvotes with \code{FALSE}.
First
}

