\name{export.csv}
\alias{export.csv}
\title{Export \code{moreno} Object to CSV File}
\usage{
export.csv(m, file, upvote.sign = "+", downvote.sign = "-", na = "",
  use.ids = TRUE, counts = TRUE, individual = TRUE, ...)
}
\arguments{
  \item{m}{\code{moreno}-class object to include}

  \item{file}{CSV file path}

  \item{upvote.sign}{string for upvotes (defaults to
  \code{+})}

  \item{downvote.sign}{string for downvotes (defaults to
  \code{-})}

  \item{na}{string for missing values (defaults to empty
  string)}

  \item{use.ids}{should user IDs be included (defaults to
  \code{TRUE})}

  \item{counts}{include vote counts (defaults to
  \code{TRUE})}

  \item{individual}{include individual sociometric indexes
  (defaults to \code{TRUE})}

  \item{...}{additional parameters for
  \code{\link{write.csv}}}
}
\description{
What title said.
}

