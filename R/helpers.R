##' Length or NULL
##'
##' Return object if its length is non-zero, otherwise return \code{NULL}. This helper is convenient (?) when you want to avoid zero-length vectors in expressions.
##' @param x an object to check
length.or.null <- function(x) {
    if (length(x) == 0) {
        NULL
    } else {
        x
    }
}
