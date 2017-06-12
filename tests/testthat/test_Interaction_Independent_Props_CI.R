library("propint") 

context("Interaction for independent proportions")

test_that("ci.indep.interaction produces the results reported in Newcombe2001", {
    # section 4.1 in Newcombe 2001
    test_1 <- ci.indep.interaction(ci=95, a1=17, m1=65, b1=17, n1=75, 
                                          a2=18, m2=72, b2=16, n2=65)
    expect_equal(test_1$d, (17/65 - 17/75) - (18/72 - 16/65))
    expect_true(abs(test_1$l - (-0.1687)) < 1e-4)
    expect_true(abs(test_1$u - 0.2343) < 1e-4)
})
