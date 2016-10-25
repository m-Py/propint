
#'
#' Confidence interval for interaction of proportions
#'
#' This function tests for an interaction effect in proportions: it tests the
#' difference between the differences of two proportions (i.e. (p1 - p2) and (p3 -
#' p4)) using a confidence intervall. This corresponds closely to a 2x2 ANOVA when
#' outcomes are dichotomous rather than continuous. See Newcombe (2001).
#' 
#' @param ci What confidence interval - give as a natural number (e.g. 95 for the
#'     95\% confidence interval). Only pass this argument when passing absolute frequencies
#'     (a, m , b, n); do not combine with proportions and confidence intervall boundaries.
#' @param a1 Frequency of events in proportion A1
#' @param m1 Total frequency in proportion A1
#' @param b1 Frequency of events in proportion B1
#' @param n1 Total frequency in proportion B1
#' @param a2 Frequency of events in proportion A2
#' @param m2 Total frequency in proportion A2
#' @param b2 Frequency of events in proportion B2
#' @param n2 Total frequency in proportion B2
#' @param p1 Proportion 1; pass only if absolute frequencies (a, m , b, n) are not
#'     given
#' @param l1 The lower bound of the confidence interval of p1.
#' @param u1 The lower bound of the confidence interval of p1.
#' @param p2 Proportion 2; pass only if absolute frequencies (a, m , b, n) are not
#'     given
#' @param l2 The lower bound of the confidence interval of p2.
#' @param u2 The lower bound of the confidence interval of p2.
#' @param p3 Proportion 3; pass only if absolute frequencies (a, m , b, n) are not
#'     given
#' @param l3 The lower bound of the confidence interval of p3.
#' @param u3 The lower bound of the confidence interval of p3.
#' @param p4 Proportion 4; pass only if absolute frequencies (a, m , b, n) are not
#'     given
#' @param l4 The lower bound of the confidence interval of p4.
#' @param u4 The lower bound of the confidence interval of p4.
#' 
#' @return A \code{list} containing the confidence interval boudaries.
#'   \item{d}{The difference between differences ((a1/m1 - b1/n1) - (a2/m2 - b2/n2) = (p1 - p2) - (p3 - p4)}
#'   \item{l}{The lower bound of the confidence interval d}
#'   \item{u}{The upper bound of the confidence interval d}
#'
#' @references
#' 
#'   Newcombe, R. G. (2001). Estimating the difference between differences: measurement
#'       of additive scale interaction for proportions. Statistics in medicine, 20(19),
#'       2885-2893.
#'
#' @examples
#'
#' # Example from From Newcombe (2001):
#'
#' ci.indep.interaction(ci=95, a1=17, m1=65, b1=17, n1=75, a2=18, m2=72, b2=16, n2=65)
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
    input1 <- c(ci, a1, m1, b1, n1, a2, m2, b2, n2)
    input2 <- c(p1, l1, u1, p2, l2, u2, p3, l3, u3, p4, l4, u4)
    bool1 <- is.null(input1)
    bool2 <- is.null(input2)

    if (bool1 == bool2) {
        stop("False input: Pass EITHER frequencies OR proportions and CIs")
    } else if (bool1 == FALSE & length(input1) != 9) {
        stop("False input: at least one required frequency was not passed")
    } else if (bool2 == FALSE & length(input2) != 12) {
        stop("False input: at least one required proportion or CI was not passed")
    }

    # proceed to compute interaction CI

    # Step 1: Determine CIs for differences 
    # Input case (a): Frequencies (a1, ..., n2) are given 
    if (bool2) {
        diff1 <- ci.two.indep.props(ci=ci, a=a1, m=m1, b=b1, n=n1)
        diff2 <- ci.two.indep.props(ci=ci, a=a2, m=m2, b=b2, n=n2)
    }
    # Input case (b): 4 Proportions and respective CIs are given
    else if (bool1) {
        diff1 <- ci.two.indep.props(p1=p1, l1=l1, u1=u1, p2=p2, l2=l2, u2=u2)
        diff2 <- ci.two.indep.props(p1=p3, l1=l3, u1=u3, p2=p4, l2=l4, u2=u4)
    }
    
    l1 <- diff1$l
    u1 <- diff1$u
    d1 <- diff1$d

    l2 <- diff2$l
    u2 <- diff2$u
    d2 <- diff2$d
    
    # Step 2: Determine CIs for difference of differences
    delta   <- sqrt( (d1 - l1)^2 + (u2 - d2)^2 ) # see Newcombe 2001
    epsilon <- sqrt( (u1 - d1)^2 + (d2 - l2)^2 )
    d <- d1 - d2
    
    return(list(d=d, l=d-delta, u=d+epsilon))
} 
