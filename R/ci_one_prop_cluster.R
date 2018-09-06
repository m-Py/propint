#'
#' Confidence interval for a single proportion estimated from a
#' clustered sample
#'
#' Computes the confidence interval for a proportion arising from
#' clustered data. That is, individual responses are nested within a
#' cluster; for example disease prevalence may be estimated on the basis
#' of responses given from individuals in different communities. The
#' clustering of responses is taken into account in the estimation of
#' the standard error of the proportion using the method described in
#' Bennet et al. (1991).
#'
#' @param ci The confidence level - pass as a natural number (e.g. 95
#'     for the 95\% confidence interval)
#' @param successes A vector describing the number of "successes" (i.e.,
#'     response = 1) in each cluster.
#' @param N A vector describing the total number of responses in each
#'     cluster.
#'
#' @return A \code{list} containing the confidence interval boudaries.
#'   \item{p}{The estimated proportion of "success" across all clusters}
#'   \item{l}{The lower bound of the confidence interval of p}
#'   \item{u}{The upper bound of the confidence interval of p}
#'   \item{se.cluster}{The estimated standard error of the proportion}
#'   \item{design.effect}{The design effect}
#'   \item{intraclass.cor}{The intraclass correlation}
#'
#' @details When the intraclass correlation is estimated to be lower
#'     than 0, the value is set to 0 as intraclass correlations of less
#'     than zero are generally implausible (Donner & Klar, 1993)
#' 
#' @references
#'
#' Bennett, S., Woods, T., Liyanage, W. M., Smith, D. L. (1991). A
#'     simplified general method for cluster-sample surveys of health in
#'     developing countries. World Health Stat Quarterly, 44(3), 98-106.
#'
#' Donner, A., & Klar, N. (1993). Confidence interval construction for
#'   effect measures arising from cluster randomization trials. Journal
#'   of clinical epidemiology, 46(2), 123-131.
#' 
#' @examples
#'
#' # Example from Bennet et al. (1991)
#' ci.one.prop.cluster(95, successes = c(2, 5, 3, 3, 1, 0), N = c(2, 7, 4, 6, 4, 3))
#' 
#' 
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
#' 

ci.one.prop.cluster <- function(ci, successes, N) {
    
    if (length(successes) != length(N)) {
        stop("'successes' and 'N' indicate different number of clusters") 
    }

    c <- length(successes)
    p <- sum(successes) / sum(N)

    # compute confidence interval
    se.cluster <- se.cluster(successes, N, c, p)
    z <- get.z.score(ci)
    lower <- p - z*se.cluster
    upper <- p + z*se.cluster

    # compute design effect and intraclass correlation as additional
    # information
    se.simple      <- se.simple(p, sum(N)) # se from unclustered data
    design.effect  <- se.cluster^2 / se.simple^2
    intraclass.cor <- intraclass.from.design(design.effect, N)
    
    return(list(p=p, l=lower, u=upper, se.cluster = se.cluster,
                design.effect = design.effect,
                intraclass.cor = intraclass.cor))
}

se.cluster <- function(y, x, c, p) {
    se <- (c / sum(x)) * sqrt( (sum(y^2) - 2*p * sum(x*y) + p^2 *
                                sum(x^2)) / (c*(c-1)))
    return(se)
}

se.simple <- function(p, N) {
    se <- sqrt((p * (1-p) / N))
    return(se)
}

intraclass.from.design <- function(design.effect, N) {
    # b = average number of individuals per cluster
    b <- sum(N) / length(N)
    roh <- (design.effect - 1) / (b - 1)
    if (roh < 0) roh <- 0 # recommended since values < 0 are implausible
    return(roh)
}
