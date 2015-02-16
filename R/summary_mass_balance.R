#' @title Summary for MassBalance object
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

summary.MassBalance <- function(x, ...) {

    if (!inherits(x, "MassBalance")){
        stop("wrong summary method selected - check objct class")
    }

    bob <- as.logical(lapply(x, function(x) is.data.frame(x)))
    ll <- x[bob]
    ll
}