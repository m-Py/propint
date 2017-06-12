
#'
#' Confidence interval for a single proportion
#'
#' This function returns the confidence interval for a single proportion using
#' »Wilson's score« (1927) as recommended in Newcombe (Method 3, 1998).
#'
#' @param ci The confidence level - pass as a natural number (e.g. 95 for the
#'     95\% confidence interval) 
#' @param r Frequency of events
#' @param n Total trial frequency
#'
#' @return A \code{list} containing the confidence interval boudaries.
#'   \item{p}{The proportion r/n}
#'   \item{l}{The lower bound of the confidence interval of p=r/n}
#'   \item{u}{The upper bound of the confidence interval of p=r/n}
#' 
#' @references
#'
#'   Newcombe, R. G. (1998). Two‐sided confidence intervals for the single
#'       proportion: comparison of seven methods. Statistics in medicine, 17(8),
#'       857-872.
#'
#'   Wilson, E. B. (1927). Probable inference, the law of succession, and statistical
#'       inference. Journal of the American Statistical Association, 22(158), 209-212.
#'
#' @examples
#' # Examples from Newcombe (1998):
#' ci.one.prop(ci=95, r=81, n=263)
#' ci.one.prop(ci=95, r=15, n=148)
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
#' 

ci.one.prop <- function(ci, r, n) {
    alpha <- (100 - ci)/100
    z     <- qnorm((1-alpha/2))

    p <- r/n
    lower <- (2*n*p + z^2 - (z*sqrt(z^2 + 4*n*p*(1-p)) )) / (2*(n+z^2))
    upper <- (2*n*p + z^2 + (z*sqrt(z^2 + 4*n*p*(1-p)) )) / (2*(n+z^2))
    
    return(list(p=p, l=lower, u=upper))
}
