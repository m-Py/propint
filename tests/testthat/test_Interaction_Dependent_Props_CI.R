library("propint") 

context("Interaction for dependent proportions")

test_that("ci.mixed.interaction produces the results reported in Newcombe2001", {
    # section 4.2 in Newcombe 2001
    test_1 <- ci.mixed.interaction(ci=95, 294, 36, 59, 57, 209, 19, 32, 50)
    expect_equal(test_1$d, (36-59)/(294 + 36 + 59 + 57) - 
                           (19-32)/(209 + 19 + 32 + 50))
    expect_true(abs(test_1$l - (-0.0721)) < 1e-4)
    expect_true(abs(test_1$u - 0.0529) < 1e-4)
    
    # section 4.3 in Newcombe 2001
    test_2 <- ci.mixed.interaction(ci=95, 2, 12, 3, 17, 4, 1, 2, 25)
    expect_equal(test_2$d, (12-3)/(2 + 12 + 3 + 17) - 
                           (1-2)/(4 + 1 + 2 + 25))
    expect_true(abs(test_2$l - 0.0430) < 1e-4)
    expect_true(abs(test_2$u - 0.5269) < 1e-4)
    
    # section 4.4 in Newcombe 2001
    test_3 <- ci.mixed.interaction(ci=95, 11, 6, 1, 3, 9, 4, 5, 1)
    expect_equal(test_3$d, (6-1)/(11 + 6 + 1 + 3) - 
                           (4-5)/(9 + 4 + 5 + 1))
    expect_true(abs(test_3$l - (-0.0973)) < 1e-4)
    expect_true(abs(test_3$u - 0.6475) < 1e-4)
})
