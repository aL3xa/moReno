## data-import-export.R

## create skeleton JSON, CSV
## IDEA: create interactive mode for data insertion

## moreno.json.skeleton <- function(file, person.ids = NULL, person.names = NULL, criterias = NULL, person.name.placeholder = "person_", ...) {

##     null.person.ids <- is.null(person.ids)
##     null.person.names <- is.null(person.names)

##     ## no person names
##     if (null.person.names) {
##         ## you must provide person ids
##         if (null.person.ids) {
##             stop('you must provide person ID or names')
##         } else {
##             ## fill in person names with bogus names
##         }
##     } else {
##         ## no person ids
##         if (null.person.ids) {
##             ## fill in with bogus person ids
##             person.ids <- 1:(length(person.names))
##         }
##         if (length(person.ids) != length(person.names)) {
##             stop('person ID and names vector must have the same length')
##         }
##     }

##     if (is.null(criterias)) {
##         ## only one criteria
##         criterias <- ""
##     }

##     res <- list()
##     length(res) <- length(person.names)
##     votes <- list()

##     for (i in seq_along(person.names)) {
##         res[i] <- list(
##             id = person.ids[i],
##             name = person.names[i]
##             )
##     }

##     ## invisibly return R list

## }

##' Import CSV with Sociometric Data
##'
##' Imports CSV file with votes from sociometric questionnaire and converts it to logical matrix where upvotes are recorded as \code{TRUE} and downvotes with \code{FALSE}.
##' @param file CSV file path
##' @param upvote.sign character string that represents upvote (defaults to \code{+})
##' @param downvote.sign character string that represents downvote (defaults to \code{-})
##' @param na.strings strings that will be converted to \code{NA} (defaults to \code{c('', 'NA')})
##' @param id.column.index index of ID column (defaults to \code{1L})
##' @param ... additional params for \code{\link{read.csv}}
import.csv <- function(file, upvote.sign = '+', downvote.sign = '-', na.strings = c('', 'NA'), id.column.index = 1L, ...) {
    ## TODO: match.call() to avoid duplicate formal argument matching (na.strings) - set to c('', 'NA') if not provided
    d <- read.csv(file, stringsAsFactors = FALSE, na.strings = na.strings, ...)
    if (has.column.index <- !is.null(id.column.index)) {
        ids <- d[, id.column.index]
        d <- d[, -id.column.index]
    }
    if (ncol(d) != nrow(d)) {
        stop('data matrix should be a square matrix')
    }
    cn <- colnames(d)
    m <- as.matrix(d)
    m[m == upvote.sign] <- TRUE
    m[m == downvote.sign] <- FALSE
    m <- as.logical(m)
    dim(m) <- dim(d)
    if (has.column.index) {
        rownames(m) <- ids
    }
    colnames(m) <- cn
    m
}


##' Export \code{moreno} Object to CSV File
##'
##' What title said.
##' @param m \code{moreno}-class object to include
##' @param file CSV file path
##' @param upvote.sign string for upvotes (defaults to \code{+})
##' @param downvote.sign string for downvotes (defaults to \code{-})
##' @param na string for missing values (defaults to empty string)
##' @param use.ids should user IDs be included (defaults to \code{TRUE})
##' @param counts include vote counts (defaults to \code{TRUE})
##' @param individual include individual sociometric indexes (defaults to \code{TRUE})
##' @param ... additional parameters for \code{\link{write.csv}}
##' @export
export.csv <- function(m, file, upvote.sign = '+', downvote.sign = '-', na = '', use.ids = TRUE, counts = TRUE, individual = TRUE, ...) {
    stopifnot(inherits(m, 'moreno'))
    res <- m$data
    res[is.na(res)] <- ''
    res[res == 'TRUE'] <- upvote.sign
    res[res == 'FALSE'] <- downvote.sign
    if (!use.ids) {
        rownames(res) <- 1:nrow(res)
    }
    if (counts) {
        res <- rbind(res, t(m$counts))
    }
    if (individual) {
        res <- rbind(res, t(m$individual))
    }
    write.csv(res, file, na = na, ...)
}
