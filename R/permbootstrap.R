permbootstrap <- function(x,P,n=10000) {
    require('dplyr')
    require('reshape2')

    nsub <- dim(P)[2]
    nperm <- dim(P)[1]
    colshift <- seq(0,nsub-1) * nperm
    cc <- 0
    mm <- numeric(n)
    if (length(x) > 1) {
        for (i in 1:n) {
            ix <- sample(nperm,nsub,replace = T) + colshift
            mm[i] <- mean(P[ix] - x)
            cc <- cc + (mm[i] > 0)
        }
        mm <- mm + mean(x)
    } else {
        for (i in 1:n) {
            ix <- sample(nperm,nsub,replace = T) + colshift
            mm[i] <- mean(P[ix])
            cc <- cc + (x < mm[i])
        }
    }
    r <- list(xLessThan=cc, x = x, subjects = nsub, base_perms = nperm, bootstrap_perms = n, pMeans=mm)
    class(r) <- "permbs"
    return(r)
}
plot.permbs <- function(x, critical_p, ...) {
    m <- mean(x$pMean)
    d <- abs(m - mean(x$x))
    xlim = c(
        min(min(x$pMean),m-d),
        max(max(x$pMean),m+d)
    )
    v <- mean(x$x)
    hist(x$pMeans,
         main="Histogram of permutation means",
         xlab = "Permutation Mean over Subjects",
         xlim = xlim
    )
    abline(v=v, col='red')
    if (!missing(critical_p)) {
        tt <- quantile(x$pMeans, critical_p)
        abline(v=tt, col='blue')
    }
}
summary.permbs <- function(x, critical_p, ...) {
    cat(sprintf('Number of subjects: %d  Number of base permutations: %d\n', x$subjects, x$base_perms))
    cat(sprintf('Number of bootstrapped group means (sampling from base permutations): %d\n', x$boostrap_perms))
    if (length(x$x) > 1) {
        cat(sprintf('Mean over subjects in true data: %.4f\n', mean(x$x)))
        cat(sprintf('Mean (paired) difference between bootstrapped means and true mean: %.4f\n', mean(x$pMean)))
    } else {
        cat(sprintf('Mean over subjects in true data: %.4f\n', x$x))
        cat(sprintf('Mean (samplewise) difference between bootstrapped means and true mean: %.4f\n', mean(x$pMean-x$x)))
    }
    cat(sprintf('True mean is smaller than %d bootstrapped group means.\n', x$xLessThan))
    if (!missing(critical_p)) {
        if (critical_p < 0.5)
            nn <- floor(x$bootstrap_perms * critical_p)
        else
            nn <- ceiling(x$bootstrap_perms * critical_p)
        cat(sprintf('Critical p=%g corresponds to decision criterion "Smaller than at least %d".\n', critical_p, nn))
        if (decision(x, critical_p))
            cat('True mean exceeds this decision crition. It is unlikely to be sampled from the random distribution.\n')
        else
            cat('True mean does not exceed this decision crition. It is likely to be sampled from the random distribution.\n')
    }
}
