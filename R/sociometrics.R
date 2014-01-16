## sociometry.R

##' Mutual Choices
##'
##' Get pairs of mutual choices.
##' @param d data matrix
##' @param ind mutual indices
get.mutual.pairs <- function(d, ind) {
    m <- d[ind, ind]
    nr <- nrow(m)
    if (nr == 0) {
        NULL
    } else if (nr == 2) {
        matrix(rownames(m), nrow = 1)
    } else {
        m1 <- which(m, arr.ind = TRUE)
        m2 <- t(apply(m1, 1, sort))
        m3 <- m2[order(m2[, 1]), ]
        matrix(rownames(m3), nrow = nrow(m3) / 2, byrow = TRUE)
    }
}

##' Sociometric Indexes
##'
##' Main package function that calculates sociometric indexes (both individual and group ones). For now it only supports data imports from CSV format (see \code{\link{import.csv}} for details).
##'
##' Upon successful calculation, a list with custom class (\code{moreno}) is returned with following items:
##' \itemize{
##'     \item \code{data} - holds logical square matrix with user votes
##'     \item \code{counts} - numeric matrix with positive and negative vote counts
##'     \item \code{individual} - numeric matrix with individual sociometric indexes, namely \emph{acception index} (\code{Ia}), \emph{rejection index} (\code{Ir}) and \emph{social status index} (\code{Iss})
##'     \item \code{group} - list of 2 numeric values: \emph{group cohesion index} (\code{cohesion}) and \emph{group tension index} (\code{tension})
##'     \item \code{categories} - list of 9, each element containing data, either a character vector with user ids, or a matrix with id pairs of users that fall into specific sociometric category. Following sociometric categories are available:
##'     \enumerate{
##'         \item \code{loners} - users who have no votes, nor give votes to others
##'         \item \code{unchosen} - have no votes, but give votes to others
##'         \item \code{rejected} - have only negative votes
##'         \item \code{abstinents} - refuse to give vote to others, but do get voted
##'         \item \code{popular} - have 5 or more positive votes, and no negative votes
##'         \item \code{unpopular} - have 4 or more negative votes, but no positive votes
##'         \item \code{mutual.positive} - pairs of users who give positive votes mutually
##'         \item \code{mutual.negative} - pairs of users who give negative votes mutually
##'         \item \code{unrequited} - pairs of users who give negative votes mutually
##'     }
##' }
##' @param x either a file path to CSV file or a square logical matrix
##' @param ... additional parameters for \code{\link{import.csv}}
##' @export
moreno <- function(x, ...) {
    ## x can be either a matrix or a character string
    if (is.character(x) && length(x) == 1) {
        ## TODO: replace with regexp to ditch dependency
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

    ## check data validity: only one + and/or -, or none, per row
    ok <- apply(d, 1, function(x) {
        isTRUE(length(which(x)) == 1 && length(which(!x)) == 1 || all(is.na(x)))
    })
    if (!all(ok)) {
        stop('found invalid rows: ', paste0(names(which(!ok)), collapse = ", "))
    }

    ids <- row.names(d)                 #this should work after import

    ## votes (positive/negative)
    pos <- colSums(d, na.rm = TRUE)
    neg <- colSums(d == FALSE, na.rm = TRUE)
    nas <- apply(d, 1, function(x) all(is.na(x)))
    n <- nrow(d)
    nn <- n - 1                         #corrected sample size

    ## calculate group indexes
    prefs.max <- n * (n - 1) / 2
    mutual.pos <- mutual.neg <- logical(n)
    u <- NULL
    unreq <- numeric()

    for (i in 1:n) {
        u <- xor(d[i, ], d[, i])
        if (isTRUE(any(u))) {
            unreq <- rbind(unreq, cbind(ids[i], ids[which(u)]))
        }
        mutual.pos[i] <- isTRUE(which(d[i, ]) %in% which(d[, i]))
        mutual.neg[i] <- isTRUE(which(d[i, ] == FALSE) %in% which(d[, i] == FALSE))
    }
    if (length(unreq)) {
        unrequited <- unique(t(apply(unreq, 1, sort)))
    } else {
        unrequited <- NULL
    }
    cohesion <- (sum(mutual.pos) / 2) / prefs.max #cohesion index
    tension <- (sum(mutual.neg) / 2) / prefs.max  #tension index
    max.ct <- n / prefs.max                       #maximum cohesion/tension

    ## sociometric categories
    categories <- list(
        ## loners - 0 votes and NAs for the others
        loners = length.or.null(ids[pos == 0 & neg == 0 & nas]),
        ## unchosen - 0 votes
        unchosen = length.or.null(ids[pos == 0 & neg == 0]),
        ## rejected - only negative votes
        rejected = length.or.null(ids[pos == 0 & neg > 0]),
        ## abstinents - 1+ votes and NAs for others
        abstinents = length.or.null(ids[(pos > 0 | neg > 0) & nas]),
        ## popular - 5+ positive votes, no negative votes
        popular = length.or.null(ids[pos >= 5 & neg == 0]),
        ## unpopular - 4+ negative votes, no positive votes
        unpopular = length.or.null(ids[neg >= 4 & pos == 0]),
        ## mutual attraction - mutual positive votes
        mutual.positive = get.mutual.pairs(d, mutual.pos),
        ## mutual rejection - mutual negative votes
        mutual.negative = get.mutual.pairs(d, mutual.neg),
        ## unrequited - positive <-> negative vote
        unrequited = unrequited
        )

    ## return final
    res <- list(
        ## vote matrix
        data = d,
        ## vote counts
        counts = structure(list(
            `+` = pos,                  #positive votes count
            `-` = neg                   #negative votes count
            ),
            .Names = c('+', '-'),
            row.names = ids,
            class = 'data.frame'
            ),
        ## individual indexes
        individual = data.frame(
            Ia = pos / nn,              #acceptance index
            Ir = neg / nn,              #rejection index
            Iss = (pos - neg) / nn,     #social status index
            row.names = ids
            ),
        ## group indexes
        group = list(
            cohesion = c(
                index = cohesion,       #cohesion index
                max = max.ct,           #maximum cohesion
                relative = cohesion / max.ct #relative cohesion index (0-1)
                ),
            tension = c(
                index = tension,        #tension index
                max = max.ct,           #maximum cohesion
                relative = tension / max.ct #relative tension index (0-1)
                )
            ),
        categories = categories
        )

    class(res) <- 'moreno'
    res
}
