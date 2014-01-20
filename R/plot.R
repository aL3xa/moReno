## plot.R

##' Sociogram
##'
##' Plots sociogram for given \code{moreno}-classed object. Uses \code{igraph} library and \emph{Fruchterman-Reingold} layout to plot sociogram.
##' @param m a \code{moreno}-classed object
##' @param group additional vector for group affiliation (defaults to \code{NULL})
##' @param group.colors character vector for group colors (defaults to \code{NULL})
##' @param arrow.colors character vector for arrow colors (defaults to \code{NULL})
##' @param user.focus logical or numeric vector with indices for users whose vertex shape will be changed on the sociogram according to the value provided in \code{user.focus.shape} argument (defaults to \code{NULL})
##' @param user.focus.shape see \code{user.focus} (defaults to \code{csquare})
##' @import igraph
##' @export
sociogram <- function(m, group = NULL, group.colors = NULL, arrow.colors = NULL, user.focus = NULL, user.focus.shape = 'csquare') {
    stopifnot(inherits(m, 'moreno'))
    ## prepare data matrix
    d0 <- as.data.frame(t(apply(m$data, 1, function(x){
        y <- which(x)
        n <- which(!x)
        y <- if (length(y)) y else NA
        n <- if (length(n)) n else NA
        c(y, n)
    })))
    colnames(d0) <- choice.columns <- c('vote.1', 'vote.2')
    ## add id
    d0$id <- 1:nrow(d0)
    if (group.exists <- !is.null(group)) {
        d0$group <- group
    }
    d <- reshape(d0, direction = 'long', varying = choice.columns)
    ## get rid of rownames
    rownames(d) <- NULL
    edges <- as.matrix(d[, c('id', 'vote')])
    g <- graph.data.frame(edges, directed = TRUE)
    V(g)$label <- V(g)$name

    ## color arrows based on vote
    ac <- if (is.null(arrow.colors)) c('blue', 'red') else arrow.colors
    E(g)$color <- as.character(factor(d$time, labels = ac))

    if (group.exists) {
        ## color vertex based on group
        V(g)$group <- group
        V(g)$color <- as.character(factor(group, labels = group.colors))
    }

    ## TODO: nodesize by votes

    ## change plot vertex for specific users
    if (!is.null(user.focus)) {
        V(g)$shape <- 'circle'
        V(g)[user.focus]$shape <- user.focus.shape
    }

    ## avoid node overlap (fruchterman.reingold ?)
    par(mar = rep(0, 4))
    plot(g, layout = layout.fruchterman.reingold(g))
    invisible(g)
}
