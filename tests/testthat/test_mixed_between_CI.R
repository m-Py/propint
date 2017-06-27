
library("propint") 

context("Main effect group variable in mixed design")
test_that("standard.error produces the results reported in Donner1993", {
    
    # create data structure from Donner & Klar 1993
    # 1 = dead, 0 = alive
    donnerdata <- list()
    donnerdata$eg <- list()
    donnerdata$eg[[1]] <- c(rep(1, 3), rep(0, 98))
    donnerdata$eg[[2]] <- c(rep(1, 3), rep(0, 50))
    donnerdata$eg[[3]] <- c(rep(1, 2), rep(0, 211))
    donnerdata$eg[[4]] <- c(rep(1, 8), rep(0, 287))
    donnerdata$eg[[5]] <- c(rep(1, 1), rep(0, 163))
    donnerdata$eg[[6]] <- c(rep(1, 1), rep(0, 223))
    donnerdata$eg[[7]] <- c(rep(1, 7), rep(0, 311)) 
    donnerdata$eg[[8]] <- c(rep(1, 3), rep(0, 169)) 
    donnerdata$eg[[9]] <- c(rep(1, 4), rep(0, 285)) 
    donnerdata$eg[[10]] <- c(rep(1, 2), rep(0, 89))
    donnerdata$eg[[11]] <- c(rep(1, 2), rep(0, 126))
    donnerdata$eg[[12]] <- c(rep(1, 1), rep(0, 16)) 
    donnerdata$eg[[13]] <- c(rep(1, 7), rep(0, 97)) 
    donnerdata$eg[[14]] <- c(rep(1, 6), rep(0, 88)) 
    donnerdata$eg[[15]] <- c(rep(1, 6), rep(0, 277))
    donnerdata$eg[[16]] <- c(rep(1, 8), rep(0, 207))
    donnerdata$eg[[17]] <- c(rep(1, 4), rep(0, 233))
    
    donnerdata$cg <- list()
    
    donnerdata$cg[[1]] <- c(rep(1, 2), rep(0, 108))
    donnerdata$cg[[2]] <- c(rep(1, 3), rep(0, 25))
    donnerdata$cg[[3]] <- c(rep(1, 0), rep(0, 80))
    donnerdata$cg[[4]] <- c(rep(1, 3), rep(0, 237))
    donnerdata$cg[[5]] <- c(rep(1, 4), rep(0, 122))
    donnerdata$cg[[6]] <- c(rep(1, 12), rep(0, 376))
    donnerdata$cg[[7]] <- c(rep(1, 8), rep(0, 156)) 
    donnerdata$cg[[8]] <- c(rep(1, 9), rep(0, 187)) 
    donnerdata$cg[[9]] <- c(rep(1, 6), rep(0, 208)) 
    donnerdata$cg[[10]] <- c(rep(1, 2), rep(0, 50))
    donnerdata$cg[[11]] <- c(rep(1, 5), rep(0, 167))
    donnerdata$cg[[12]] <- c(rep(1, 3), rep(0, 48)) 
    donnerdata$cg[[13]] <- c(rep(1, 3), rep(0, 115)) 
    donnerdata$cg[[14]] <- c(rep(1, 4), rep(0, 115)) 
    donnerdata$cg[[15]] <- c(rep(1, 11), rep(0, 226))
    donnerdata$cg[[16]] <- c(rep(1,10 ), rep(0, 241))
    donnerdata$cg[[17]] <- c(rep(1, 7), rep(0, 221))

    intraclass.correlation <- intraclass(donnerdata$eg, donnerdata$cg)
    expect_true((intraclass.correlation - 0.0027) < 1e-4)

    C1 <- correction.factor(donnerdata$eg, intraclass.correlation) 
    expect_true((C1 - 1.60) < 1e-2)

    C2 <- correction.factor(donnerdata$cg, intraclass.correlation) 
    expect_true((C2 - 1.58) < 1e-2)
    
})
