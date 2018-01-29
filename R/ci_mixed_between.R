
#' Compute the a confidence interval for the difference between two
#' proportions based on clustered data
#'
#' This function computes the same confidence interval as
#' `ci.mixed.between` but uses different input parameters.
#'
#' @param ci The confidence level - pass as a natural number (e.g. 95
#'     for the 95\% confidence interval).
#' @param successes1 A numeric vector indicating the number of
#'     "successes" per cluster in condition 1
#' @param successes2 A numeric vector indicating the number of
#'     "successes" per cluster in condition 2
#' @param N1 A numeric vector indicating the number of observations per
#'     cluster in condition 1
#' @param N2 A numeric vector indicating the number of observations per
#'     cluster in condition 2
#' 
#' @references
#' 
#' Donner, A., & Klar, N. (1993). Confidence interval construction for
#'   effect measures arising from cluster randomization
#'   trials. Journal of clinical epidemiology, 46(2), 123-131.
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
#'  

ci.two.clustered.props <- function(ci, successes1, successes2, N1, N2) {

    vector1 <- lapply(successes1, function(x) rep(1, x))
    vector2 <- lapply(successes2, function(x) rep(1, x))
    trials1 <- lapply(N1, function(x) rep(0, x))
    trials2 <- lapply(N2, function(x) rep(0, x))

    trials1 <- add_lists(vector1, trials1)
    trials2 <- add_lists(vector2, trials2)

    return(ci.two.clustered.props.internal(ci, trials1, trials2))
}

#' Confidence interval for the "main effect" of a between-subjects
#' factor in mixed 2 x 2 design with a dichotomous outcome
#'
#' This function computes a confidence interval for the main effect of
#' a grouping variable across two levels of a within subject factor
#' when the measure of interest is dichotomous. This for example
#' corresponds to the effectiveness of a treatment on individuals
#' across two different points in time. The confidence interval that
#' is produced is corrected for the within-subjects dependence of the
#' two measures by applying a correction suggested by Donner and Klar
#' (section 2.1; 1993) to the standard error of the difference of
#' proportions.
#' 
#' @param ci The confidence level - pass as a natural number (e.g. 95
#'     for the 95\% confidence interval).
#' @param group1.measure1 A vector of "successes" (= 1) and "fails" (=
#'     0) for the first measure in group 1
#' @param group1.measure2 A vector of "successes" (= 1) and "fails" (=
#'     0) for the second measure in group 1
#' @param group2.measure1 A vector of "successes" (= 1) and "fails" (=
#'     0) for the first measure in group 2
#' @param group2.measure2 A vector of "successes" (= 1) and "fails" (=
#'     0) for the second measure in group 2
#'
#' @references
#' 
#' Donner, A., & Klar, N. (1993). Confidence interval construction for
#'   effect measures arising from cluster randomization
#'   trials. Journal of clinical epidemiology, 46(2), 123-131.
#'
#' @author Martin Papenberg \email{martin.papenberg@@hhu.de}
#'
#' @export
#'  

ci.mixed.between <- function(ci, group1.measure1, group1.measure2, 
                             group2.measure1, group2.measure2) {
    
    if (length(group1.measure1) != length(group1.measure2)) {
        stop("do not include missing data")
    } 
    if (length(group2.measure1) != length(group2.measure2)) {
        stop("do not include missing data")
    }

    cluster.lists <- create.cluster.lists(group1.measure1,
                                          group1.measure2,
                                          group2.measure1,
                                          group2.measure2)
    
    return(ci.two.clustered.props.internal(ci, cluster.lists[[1]], cluster.lists[[2]]))
}

## This function actually does the work. The parameter cluster.list is a
## list of vectors where each vector represents a cluster. Each vector
## is a sequence of 1s and 0s representing "successes" and the opposite
## respectively. The functions `ci.mixed.between` and
## `ci.two.clustered.props` only create these lists and then call this
## internal function.
ci.two.clustered.props.internal <- function(ci, cluster.list1, cluster.list2) {

    # total "n" by condition (total units across all clusters)
    N1 <- length(unlist(cluster.list1))
    N2 <- length(unlist(cluster.list2))

    P1 <- mean(unlist(cluster.list1))
    P2 <- mean(unlist(cluster.list2))

    D = P1 - P2

    Q1 <- 1 - P1
    Q2 <- 1 - P2

    intraclass.cor <- intraclass(cluster.list1, cluster.list2)

    C1 <- correction.factor(cluster.list1, intraclass.cor)
    C2 <- correction.factor(cluster.list2, intraclass.cor)
    
    SE <- sqrt( ((C1 * P1 * Q1) / N1) + ((C2 * P2 * Q2) / N2) )

    z <- get.z.score(ci)
    
    return(list(p1=P1, p2=P2, d=D, l=D-z*SE, u=D+z*SE,
                intraclass.cor=intraclass.cor))
}


## computations of intermediate results such as intraclass correlations
## and mean squares:

intraclass <- function(cluster.list1, cluster.list2) {
    MSC <- mean.sq.clusters(cluster.list1, cluster.list2)
    MSE <- mean.sq.error(cluster.list1, cluster.list2)
    if (MSC < MSE) return(0)
    Na  <- correction.intraclass(cluster.list1, cluster.list2)
    return( (MSC - MSE) / ( MSC + (Na - 1) * MSE) )
}

correction.factor <- function(cluster.list, intraclass.cor) {
    sizes <- cluster.sizes(cluster.list)
    summand <- correction.summand(sizes)
    return(1 + (summand - 1) * intraclass.cor)
}

correction.summand <- function(cluster.sizes) {
    return(sum(cluster.sizes^2)/sum(cluster.sizes))
}

cluster.sizes <- function(cluster.list) {
    return(sapply(cluster.list, length))
}

mean.sq.clusters <- function(cluster.list1, cluster.list2) {
    return(mean.squared(cluster.list1, cluster.list2, "cluster"))
}

mean.sq.error <- function(cluster.list1, cluster.list2) {
    return(mean.squared(cluster.list1, cluster.list2, "Residuals"))
}

correction.intraclass <- function(cluster.list1, cluster.list2) {
    N1 <- length(unlist(cluster.list1))
    N2 <- length(unlist(cluster.list2))
    N <- N1 + N2
    sizes1 <- cluster.sizes(cluster.list1)
    sizes2 <- cluster.sizes(cluster.list2)
    summand <- correction.summand(sizes1) + correction.summand(sizes2)
    return( (N - summand) / ((length(cluster.list1)-1) + (length(cluster.list1)-1)) )
}

#' @importFrom stats anova
mean.squared <- function(cluster.list1, cluster.list2, id) {
    models <- aov.cluster(cluster.list1, cluster.list2)
    m1     <- anova(models[[1]])
    m2     <- anova(models[[2]])
    SC1    <- m1[id, "Sum Sq"]
    SC2    <- m2[id, "Sum Sq"]
    DF1    <- m1[id, "Df"]
    DF2    <- m2[id, "Df"]
    return( (SC1 + SC2)/(DF1 + DF2) )
}

#' @importFrom stats aov
aov.cluster <- function(cluster.list1, cluster.list2) {
    frame1 <- list.to.data.frame(cluster.list1)
    frame2 <- list.to.data.frame(cluster.list2)
    model1 <- aov(outcome ~ cluster, data=frame1)
    model2 <- aov(outcome ~ cluster, data=frame2)
    return(list(model1, model2))
}

#############################################
## Some functions for data transformations ##

create.cluster.lists <- function(group1.measure1, group1.measure2,
                                 group2.measure1, group2.measure2) {

    group1 <- cbind(group1.measure1, group1.measure2)
    group2 <- cbind(group2.measure1, group2.measure2)

    list1 <- list()
    list2 <- list()
    
    for (i in 1:nrow(group1)) {
        list1[[i]] <- group1[i,]
    }
    for (i in 1:nrow(group2)) {
        list2[[i]] <- group2[i,]
    }

    return(list(list1, list2))
}

list.to.data.frame <- function(list) {
    clusters <- length(list)
    sizes <- cluster.sizes(list)
    cluster.vector <- vector()
    for (i in 1:clusters) {
        cluster.vector <- c(cluster.vector, rep(i, sizes[i]))
    }
    return(data.frame(cluster=as.factor(cluster.vector),
                      outcome = unlist(list)))
}

## elements in list2 must have more elements (or as many) than vectors
## in list1
add_lists <- function(list1, list2) {
    outcome <- list()
    for (i in 1:length(list1)) {
        outcome[[i]] <- list2[[i]]
        ## in case no positive existed:
        if (length(list1[[i]]) > 0)
            outcome[[i]][1:length(list1[[i]])] <- list1[[i]]
    }
    return(outcome)
}
