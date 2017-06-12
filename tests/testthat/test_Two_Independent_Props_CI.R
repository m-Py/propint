library("propint") 

context("Two independent proportions CI")

test_that("ci.two.indep.props produces the results reported in Newcombe1998", {

test_1 <- ci.two.indep.props(ci=95, a=56, m=70, b=48, n=80)
    expect_equal(test_1$d, (56/70)-(48/80))
    expect_true(abs(test_1$l - 0.0524) < 1e-4)
    expect_true(abs(test_1$u - 0.3339) < 1e-4)
    
    test_2 <- ci.two.indep.props(ci=95, a=9,  m=10, b=3,  n=10)
    expect_equal(test_2$d, (9/10)-(3/10))
    expect_true(abs(test_2$l - 0.1705) < 1e-4)
    expect_true(abs(test_2$u - 0.8090) < 1e-4)  
})
