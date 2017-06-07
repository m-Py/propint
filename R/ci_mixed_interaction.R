
#'
#' Confidence interval for the interaction of pairs of dependent proportions
#'
#' This function returns a confidence interval for the interaction of two 
#' pairs of dependent proportions. This for example corresponds to a repeated
#' measures design where two independent groups yield dichotomous responses on 
#' two different occasions.
#' 
#' @param ci What confidence interval - give as a natural number (e.g. 95 for the
#'     95\% confidence interval).
#' @param e1 Group 1: Number of cases for which both events are 1.
#' @param f1 Group 1: Number of cases for which the first event is 1, and the second event is 0.
#' @param g1 Group 1: Number of cases for which the first event is 0, and the second event is 1.
#' @param h1 Group 1: Number of cases for which both events are 0.
#' @param e2 Group 2: Number of cases for which both events are 1.
#' @param f2 Group 2: Number of cases for which the first event is 1, and the second event is 0.
#' @param g2 Group 2: Number of cases for which the first event is 0, and the second event is 1.
#' @param h2 Group 2: Number of cases for which both events are 0.
#' 
#' @return A \code{list} containing the confidence interval boudaries.
#'   \item{d}{The difference between differences of the two paired proportions}
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
#' # Example from From Newcombe (Section 4.2; 2001):
#' ci.mixed.interaction(95, 294, 36, 59, 57, 209, 19, 32, 50)
#' # Example from From Newcombe (Section 4.3; 2001):
#' ci.mixed.interaction(95, 2, 12, 3, 17, 4, 1, 2, 25)
#' # Example from From Newcombe (Section 4.4; 2001):
#' ci.mixed.interaction(95, 11, 6, 1, 3, 9, 4, 5, 1)
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
#' 

ci.mixed.interaction <- function(
    ci ,
    e1 ,
    f1 ,
    g1 ,
    h1 ,
    e2 ,
    f2 ,
    g2 ,
    h2) {

    # Step 1: Determine CIs for the two pairs of differences 
    diff1 <- ci.two.dep.props(ci=ci, e=e1, f=f1, g=g1, h=h1)
    diff2 <- ci.two.dep.props(ci=ci, e=e2, f=f2, g=g2, h=h2)

    return(ci.two.indep.props(p1 = diff1$d, l1 = diff1$l, u1 = diff1$u, 
                              p2 = diff2$d, l2 = diff2$l, u2 = diff2$u))
} 
