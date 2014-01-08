## sociometry.R

##' Acceptance Index
##'
##' Number of positive votes corrected by sample length.
##' @param x user record (logical vector) to calculate acceptance index from
##' @export
accept.index <- function(x) {
    sum(x == TRUE, na.rm = TRUE) / (length(x) - 1)
}

##' Rejection Index
##'
##' Number of negative votes corrected by sample length.
##' @param x user record (logical vector) to calculate rejection index from
##' @export
reject.index <- function(x) {
    sum(x == FALSE, na.rm = TRUE) / (length(x) - 1)
}

##' Social Status Index
##'
##' Calculates social status index, i.e. difference between positive and negative votes count corrected by sample length.
##' @param x user record (logical vector) to calculate social status index from
##' @export
status.index <- function(x) {
    (sum(x == TRUE, na.rm = TRUE) - sum(x == FALSE, na.rm = TRUE)) / (length(x) - 1)
}

##' Individual Sociometric Indexes
##'
##' Calculates acceptance and rejection indexes, as well as social status index.
##' @param d data to calculate indexes from
individual.indexes <- function(d) {
    res
}

group.indexes <- function(d, ...) {

}

##' Sociometrics
##'
##' .. content for \details{} ..
##' @param x
##' @param ...
##' @export
moreno <- function(x, ...) {
    ## x can be either a matrix or a character string
    if (is.character(x) && length(x) == 1) {
        d <- switch(ext <- tolower(file_ext(x)),
                    csv = moreno.import.csv(x),
                    stop('invalid file format "' + ext + '"')
                    )
        ## or a logical square matrix
    } else if (is.logical(x) && is.matrix(x) && nrow(x) == ncol(x)) {
        d <- x
    } else {
        stop('invalid data format')
    }

    ## votes (positive/negative)
    pos <- colSums(d, na.rm = TRUE)
    neg <- colSums(d == FALSE, na.rm = TRUE)
    n <- nrow(d)
    nn <- n - 1                         #corrected sample size

    ## calculate group indexes
    prefs.max <- (n * (n - 1)) / 2
    mutual.pos <- mutual.neg <- logical(n)
    for (i in 1:n) {
        mutual.pos[i] <- isTRUE(which(m[i, ]) %in% which(m[, i]))
        mutual.neg[i] <- isTRUE(which(m[i, ] == FALSE) %in% which(m[, i] == FALSE))
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
