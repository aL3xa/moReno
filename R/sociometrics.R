## sociometry.R

##' Sociometric Indexes
##'
##' Main package function that calculates sociometric indexes (both individual and group ones). For now it only supports data imports from CSV format (see \code{\link{import.csv}} for details).
##'
##' Upon successful calculation, a list with custom class (\code{moreno}) is returned with following items:
##' \itemize{
##'     \item \code{data} - holds logical square matrix with user votes
##'     \item \code{counts} - numeric matrix with positive and negative vote counts
##'     \item \code{individual} - numeric matrix with individual sociometric indexes, namely \emph{acception index} (\code{Ia}), \emph{rejection index} (\code{Ir}) and \emph{social status index} (\code{Iss})
##'     \item \code{group} - list of 2 numeric values: \emph{group cohesion index} (\code{cohesion}) and \emph{group tension index} (\code{tension}). Each value has \code{indices} attribute with matrix indices where mutual acception/rejection has occured.
##' }
##' @param x either a file path to CSV file or a square logical matrix
##' @param ... additional parameters for \code{\link{import.csv}}
##' @export
moreno <- function(x, ...) {
    ## x can be either a matrix or a character string
    if (is.character(x) && length(x) == 1) {
        d <- switch(ext <- tolower(tools::file_ext(x)),
                    csv = import.csv(x, ...),
                    stop('invalid file format "' + ext + '"')
                    )
        ## or a logical square matrix
    } else if (is.logical(x) && is.matrix(x) && nrow(x) == ncol(x)) {
        d <- x
    } else {
        stop('invalid data format')
    }

    ## check main diagonal
    if (!all(is.na(diag(d)))) {
        stop('data matrix main diagonal has non-missing values')
    }

    ## votes (positive/negative)
    pos <- colSums(d, na.rm = TRUE)
    neg <- colSums(d == FALSE, na.rm = TRUE)
    n <- nrow(d)
    nn <- n - 1                         #corrected sample size

    ## calculate group indexes
    prefs.max <- n * (n - 1) / 2
    mutual.pos <- mutual.neg <- logical(n)
    for (i in 1:n) {
        mutual.pos[i] <- isTRUE(which(d[i, ]) %in% which(d[, i]))
        mutual.neg[i] <- isTRUE(which(d[i, ] == FALSE) %in% which(d[, i] == FALSE))
    }
    cohesion <- (sum(mutual.pos) / 2) / prefs.max #cohesion index
    tension <- (sum(mutual.neg) / 2) / prefs.max  #tension index
    attr(cohesion, 'indices') <- which(mutual.pos)
    attr(tension, 'indices') <- which(mutual.neg)

    ## return final
    res <- list(
        ## vote matrix
        data = d,
        ## vote counts
        counts = rbind(
            '+' = pos,                  #positive votes count
            '-' = neg                   #negative votes count
            ),
        ## individual indexes
        individual = rbind(
            Ia = pos / nn,              #acceptance index
            Ir = neg / nn,              #rejection index
            Iss = (pos - neg) / nn      #social status index
            ),
        ## group indexes
        group = list(
            cohesion = cohesion,        #cohesion index
            tension = tension           #tension index
            )
        )

    class(res) <- 'moreno'
    res
}
