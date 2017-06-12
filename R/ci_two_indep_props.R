

#'
#' Confidence interval for the difference of two independent
#' proportions
#'
#' This function returns the confidence interval for the difference of
#' two independent proportions. It can be used for statistical
#' inference, i.e. we assume that two proportions are significantly
#' different from each other if the confidence interval does not cover
#' 0. See Newcombe (Method 10, 1998).
#' 
#'
#' @param ci What confidence interval - give as a natural number
#'     (e.g. 95 for the 95\% confidence interval).  Only pass this
#'     argument when passing absolute frequencies (a, m , b, n); do
#'     not combine with proportions and confidence interval boundaries
#'     (in this case the confidence interval is implied in the
#'     boundaries from the passed intervals).
#' @param a Frequency of events in proportion A
#' @param m Total frequency in proportion A
#' @param b Frequency of events in proportion B
#' @param n Total frequency in proportion B
#' @param p1 Proportion 1 - a value between 0 and 1. Pass only if
#'     absolute frequencies (a, m , b, n) are not given.
#' @param l1 The lower bound of the confidence interval of p1.
#' @param u1 The upper bound of the confidence interval of p1.
#' @param p2 Proportion 2 - a value between 0 and 1. Pass only if
#'     absolute frequencies (a, m , b, n) are not given.
#' @param l2 The lower bound of the confidence interval of p2.
#' @param u2 The upper bound of the confidence interval of p2.
#'
#' @return A \code{list} containing the confidence interval boundaries
#'   \item{d}{The difference between the proportions (a/m) - (b/n)}
#'   \item{l}{The lower bound of the confidence interval}
#'   \item{u}{The upper bound of the confidence interval}
#'
#' @references
#'
#'   Newcombe, R. G. (1998). Interval estimation for the difference
#'       between independent proportions: comparison of eleven
#'       methods. Statistics in medicine, 17(8), 873-890.
#' 
#' @examples
#' # Examples from Newcombe (1998):
#' ci.two.indep.props(ci=95, a=56, m=70, b=48, n=80)
#' ci.two.indep.props(ci=95, a=9,  m=10, b=3,  n=10)
#'
#' # Alternative usage:
#' p1 <- ci.one.prop(95, 56, 70)
#' p2 <- ci.one.prop(95, 48, 80)
#' ci.two.indep.props(p1=p1$p, p2=p2$p, l1=p1$l, l2=p2$l, u1=p1$u, u2=p2$u)
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
#' 

ci.two.indep.props <- function(
    ci = NULL ,
    a = NULL , 
    m = NULL ,
    b = NULL ,
    n = NULL ,
    p1 = NULL ,
    l1 = NULL ,
    u1 = NULL ,
    p2 = NULL ,
    l2 = NULL ,
    u2 = NULL
    ) {

    # Validate user input
    input_frequency <- c(a, m, b, n, ci)
    input_proportion <- c(p1, l1, u1, p2, l2, u2)
    proportions_were_passed <- is.null(input_frequency)
    frequencies_were_passed <- is.null(input_proportion)

    if (proportions_were_passed == frequencies_were_passed) {
        stop("False input: Pass EITHER frequencies OR proportions and CIs")
    } else if (proportions_were_passed == FALSE & length(input_frequency) != 5) {
        stop("False input: at least one required frequency was not passed")
    } else if (frequencies_were_passed == FALSE & length(input_proportion) != 6) {
        stop("False input: at least one required proportion or CI was not passed")
    }

    # Compute CI for differences in proportions

    # User input case (a): frequencies given
    if (frequencies_were_passed) {
        p1 <- a/m
        p2 <- b/n
        # confidence intervals for both proportions using ci.one.prop
        l1 <- ci.one.prop(ci, a, m)$l
        u1 <- ci.one.prop(ci, a, m)$u
        l2 <- ci.one.prop(ci, b, n)$l
        u2 <- ci.one.prop(ci, b, n)$u
    } 
    # User input case (b): confidence intervalls were passed as user input;
    # thus, they need not be computed via ci.one.prop()

    d  <- p1 - p2 # difference between proportions
    delta   <- sqrt( (p1 - l1)^2 + (u2 - p2)^2 ) # see Newcombe 1998, 2001
    epsilon <- sqrt( (u1 - p1)^2 + (p2 - l2)^2 )

    return(list(d=d, l=d-delta, u=d+epsilon))
}
