\name{length.or.null}
\alias{length.or.null}
\title{Length or NULL}
\usage{
\method{length}{or.null}(x)
}
\arguments{
  \item{x}{an object to check}
}
\description{
Return object if its length is non-zero, otherwise return
\code{NULL}. This helper is convenient (?) when you want to
avoid zero-length vectors in expressions.
}

