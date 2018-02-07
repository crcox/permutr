decision <- function(x, ...) {
    UseMethod("decision", x)
}

decision.permbs <- function(x, critical_p) {
    if (critical_p < 0.5) {
        nn <- floor(x$bootstrap_perms * critical_p)
        d <- x$xLessThan > nn
    } else {
        nn <- ceiling(x$bootstrap_perms * critical_p)
        d <- (x$bootstrap_perms-x$xLessThan) > nn
    }
    return(d)
}