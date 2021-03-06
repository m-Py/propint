
#'
#' Confidence interval for the interaction of independent proportions
#'
#' This function tests for an interaction effect in independent
#' proportions: it tests the whether the difference between two
#' proportions is larger than the difference between two different
#' proportions using a confidence interval (i.e., the parameter of
#' interest is d = (p1 - p2) - (p3 - p4)). This corresponds closely to a
#' 2x2 ANOVA when outcomes are dichotomous rather than continuous. See
#' Newcombe (2001).
#' 
#' @param ci The confidence level - pass as a natural number (e.g. 95
#'     for the 95\% confidence interval). Only pass this argument when
#'     passing absolute frequencies (a, m , b, n); do not combine with
#'     proportions and confidence interval boundaries (in this case the
#'     confidence interval is implied in the boundaries from the passed
#'     intervals).
#' @param a1 Frequency of events in proportion A1
#' @param m1 Total frequency in proportion A1
#' @param b1 Frequency of events in proportion B1
#' @param n1 Total frequency in proportion B1
#' @param a2 Frequency of events in proportion A2
#' @param m2 Total frequency in proportion A2
#' @param b2 Frequency of events in proportion B2
#' @param n2 Total frequency in proportion B2
#' @param p1 Proportion 1 - a value between 0 and 1. Pass only if
#'     absolute frequencies (a, m , b, n) are not given.
#' @param l1 The lower bound of the confidence interval of p1.
#' @param u1 The upper bound of the confidence interval of p1.
#' @param p2 Proportion 2 - a value between 0 and 1. Pass only if
#'     absolute frequencies (a, m , b, n) are not given.
#' @param l2 The lower bound of the confidence interval of p2.
#' @param u2 The upper bound of the confidence interval of p2.
#' @param p3 Proportion 3 - a value between 0 and 1. Pass only if
#'     absolute frequencies (a, m , b, n) are not given.
#' @param l3 The lower bound of the confidence interval of p3.
#' @param u3 The upper bound of the confidence interval of p3.
#' @param p4 Proportion 4 - a value between 0 and 1. Pass only if
#'     absolute frequencies (a, m , b, n) are not given.
#' @param l4 The lower bound of the confidence interval of p4.
#' @param u4 The upper bound of the confidence interval of p4.
#' 
#' @return A \code{list} containing the confidence interval boudaries.
#'   \item{d}{The difference between differences (d = (a1/m1 - b1/n1) - (a2/m2 - b2/n2) = (p1 - p2) - (p3 - p4)}
#'   \item{l}{The lower bound of the confidence interval}
#'   \item{u}{The upper bound of the confidence interval}
#'
#' @references
#' 
#'   Newcombe, R. G. (2001). Estimating the difference between
#'       differences: measurement of additive scale interaction for
#'       proportions. Statistics in medicine, 20(19), 2885-2893.
#'
#' @examples
#'
#' # Example from From Newcombe (2001):
#'
#' ci.indep.interaction(ci=95, a1=17, m1=65, b1=17, n1=75, a2=18, m2=72,
#' b2=16, n2=65)
#' 
#' # Alternative usage:
#' p1 <- ci.one.prop(95, 17, 65)
#' p2 <- ci.one.prop(95, 17, 75)
#' p3 <- ci.one.prop(95, 18, 72)
#' p4 <- ci.one.prop(95, 16, 65) # it is possible to use other CI boundaries
#' ci.indep.interaction(p1=p1$p, p2=p2$p, p3=p3$p, p4=p4$p, l1=p1$l, l2=p2$l, l3=p3$l, l4=p4$l,
#'                      u1=p1$u, u2=p2$u, u3=p3$u, u4=p4$u)
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
#' 

ci.indep.interaction <- function(
    ci = NULL ,
    a1 = NULL ,
    m1 = NULL ,
    b1 = NULL ,
    n1 = NULL ,
    a2 = NULL ,
    m2 = NULL ,
    b2 = NULL ,
    n2 = NULL ,
    p1 = NULL ,
    l1 = NULL ,
    u1 = NULL ,
    p2 = NULL ,
    l2 = NULL ,
    u2 = NULL ,
    p3 = NULL ,
    l3 = NULL ,
    u3 = NULL ,
    p4 = NULL ,
    l4 = NULL ,
    u4 = NULL
    ) {

    # Validate user input
    input_frequency  <- c(ci, a1, m1, b1, n1, a2, m2, b2, n2)
    input_proportion <- c(p1, l1, u1, p2, l2, u2, p3, l3, u3, p4, l4, u4)
    proportions_were_passed   <- is.null(input_frequency)
    frequencies_were_passed   <- is.null(input_proportion)

    if (proportions_were_passed == frequencies_were_passed) {
        stop("False input: Pass EITHER frequencies OR proportions and CIs")
    } else if (proportions_were_passed == FALSE & length(input_frequency) != 9) {
        stop("False input: at least one required frequency was not passed")
    } else if (frequencies_were_passed == FALSE & length(input_proportion) != 12) {
        stop("False input: at least one required proportion or CI was not passed")
    }

    # Proceed to compute interaction CI

    # Step 1: Determine CIs for differences 
    # Input case (a): Frequencies (a1, ..., n2) are given 
    if (frequencies_were_passed) {
        diff1 <- ci.two.indep.props(ci=ci, a=a1, m=m1, b=b1, n=n1)
        diff2 <- ci.two.indep.props(ci=ci, a=a2, m=m2, b=b2, n=n2)
    }
    # Input case (b): 4 Proportions and respective CIs are given
    else if (proportions_were_passed) {
        diff1 <- ci.two.indep.props(p1=p1, l1=l1, u1=u1, p2=p2, l2=l2, u2=u2)
        diff2 <- ci.two.indep.props(p1=p3, l1=l3, u1=u3, p2=p4, l2=l4, u2=u4)
    }

    return(ci.two.indep.props(p1 = diff1$d, l1 = diff1$l, u1 = diff1$u, 
                              p2 = diff2$d, l2 = diff2$l, u2 = diff2$u))

} 
