
#'
#' Confidence interval for the difference of two dependent proportions
#'
#' This function returns the confidence interval for the difference of
#' two dependent proportions. It can be used for statistical
#' inference, i.e. we assume that two proportions are significantly
#' different from each other if the confidence interval does not
#' contain 0. See Newcombe (Method 10; 1998).
#' 
#'
#' @param ci The confidence level - pass as a natural number (e.g. 95
#'     for the 95\% confidence interval).
#' @param e Number of cases for which both events are 1.
#' @param f Number of cases for which the first event is 1, and the
#'     second event is 0.
#' @param g Number of cases for which the first event is 0, and the
#'     second event is 1.
#' @param h Number of cases for which both events are 0.
#'
#' @details The sum of the frequencies e + f + g + h must be equal to
#'     the total n in the sample. The parameter of interest is
#'     d=(f-g)/n, which is the difference between the two dependent
#'     proportions.
#' 
#' @return A \code{list} containing the confidence interval boudaries
#'   \item{d}{The difference between the two dependent proportions (d = (f-g)/n)}
#'   \item{l}{The lower bound of the confidence interval}
#'   \item{u}{The upper bound of the confidence interval}
#'
#' @references
#'
#'   Newcombe, R. G. (1998). Improved confidence intervals for the
#'       difference between binomial proportions based on paired
#'       data. Statistics in medicine, 17(22), 2635-2650.
#'
#'   Newcombe, R. G. (2001). Estimating the difference between
#'       differences: measurement of additive scale interaction for
#'       proportions. Statistics in medicine, 20(19), 2885-2893.
#'
#' @examples
#' # Examples from Newcombe (1998):
#' ci.two.dep.props(ci=95, e=36, f=12, g=2, h=0)
#' ci.two.dep.props(ci=95, e=18, f=12, g=2, h=18)
#' ci.two.dep.props(ci=95, e=53, f=0, g=0, h=1)
#' ci.two.dep.props(ci=95, e=0, f=30, g=0, h=0)
#'
#' # Examples from Newcombe (2001):
#' ci.two.dep.props(ci=95, 4, 1, 2, 25)
#' ci.two.dep.props(ci=95, 2, 12, 3, 17)
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
#' 

ci.two.dep.props <- function(ci, e, f, g, h) {
    
    ## compute n and parameter estimate (I call it d)
    n <- e + f + g + h
    d <- (f-g)/n

    ## Compute confidence interval (see section 3.4 in Newcombe, 2001)
    
    if ( ((e+f) * (g+h) * (e+g) * (f+h)) == 0) {
        phi <- 0
    } else {
        if (e*h > f*g) num <- max(c(e*h-f*g-(n/2), 0))
        else num <- (e*h - f*g)
        den <- sqrt((e+f) * (g+h) * (e+g) * (f+h))
        phi <- num / den
    }
    
    l2 <- ci.one.prop(ci, (e+f), n)$l
    u2 <- ci.one.prop(ci, (e+f), n)$u
    dl2 <- (e+f)/n - l2
    du2 <- u2 - (e+f)/n

    l3 <- ci.one.prop(ci, (e+g), n)$l
    u3 <- ci.one.prop(ci, (e+g), n)$u
    dl3 <- (e+g)/n - l3
    du3 <- u3 - (e+g)/n
    

    # upper and lower bound for the confidence interval
    delta   <- sqrt(dl2^2 - 2*phi*dl2*du3 + du3^2)
    epsilon <- sqrt(du2^2 - 2*phi*du2*dl3 + dl3^2)
    
    return(list(d=d, l=d-delta, u=d+epsilon))
}
