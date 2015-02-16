#' @title Plot MassBalance object
#'
#' @description stuff
#'
#' @details stuff
#' @param stuff
#'
#' @return stuff
#'
#' @examples
#'          stuff <- 1
#' @export
plot.MassBalance <- function(x, ...){

    bob <- as.logical(lapply(x, function(x) is.numeric(x)))
    xx <- x[bob]

    oldpar <- par(no.readonly = TRUE)
    par(ask = TRUE)

    for (i in 1:length(xx)){
        if (class(xx[[i]]) == "numeric"){
            xlab <- c(expression(paste("m"^"3"*"d"^"-1")), expression(paste("mgl"^"-1")))
            xlab <- rep(xlab, times = 3)
            main <- names(xx)[i]
            mean.x <- mean(xx[[i]])
            hist(xx[[i]], main = main, xlab = xlab[i])
            abline(v = mean.x, col = "red", lty = 1, lwd = 2)
            if (i == 1 | i == 2){  # draw conf int lines for ds dists
                interval <- 1.96 * (sd(xx[[i]]) / sqrt(length(xx[[i]])))
                lower = mean.x - interval; upper = mean.x + interval
                abline(v = upper, col = "dark blue", lty = 2)
                abline(v = lower, col = "dark blue", lty = 2)
            }
        }
    }
    on.exit(par(oldpar), add = TRUE)
}