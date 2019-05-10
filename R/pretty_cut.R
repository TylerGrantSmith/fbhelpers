#' Convert Numeric to Nice Factor
#'
#' Creates an ordered factor from a numeric vector input with "nice" endpoints.
#'
#' @param x a numeric vector which is to be converted to a factor by cutting
#' @param buckets approximate number of intervals into which x is to be cut
#' @param inf.left logical indicating if the leftmost interval should expand to include -Inf
#' @param inf.right logical indicating if the rightmost interval should expand to include Inf
#'
#' @return A \code{\link{factor}} is returned.
#' @export

pretty_cut <- function(x,
                       buckets = 20,
                       inf.left = T,
                       inf.right = T) {
  if(!is.numeric(x)) {
    warning("x must be numeric.")
  }

  if(length(unique(x)) <= buckets) {
    xvec <- x
    buckets <- length(unique(x))
  }
  else {
    xmin <- quantile(x, .01, na.rm = T) %>% unname
    xmax <- quantile(x, .99, na.rm = T) %>% unname
    xvec <- c(xmin,xmax)
  }

  breaks <- pretty(xvec,buckets)
  breaks_pad <- c(-Inf, breaks, Inf)

  right_end <- x %>% purrr::map(~`<=`(.,breaks_pad)) %>% purrr::map_int(list(which.max,1),.default=NA)
  left_end <- right_end - 1

  breaks <- scales::comma(breaks)

  interval_levels <- paste0("(", breaks[-length(breaks)],",",breaks[-1],"]")

  interval_levels <- c(paste0("(",-Inf,",", breaks[1],"]"),
                       interval_levels,
                       paste0("(",breaks[length(breaks)],",", Inf,")"))

  factor(left_end, levels = 1:length(interval_levels), labels = interval_levels)
}
