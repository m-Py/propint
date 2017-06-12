library("propint") 

context("Two dependent proportions CI")

test_that("ci.two.dep.props produces the results reported in Newcombe1998", {

    test_1 <- ci.two.dep.props(ci=95, e=36, f=12, g=2, h=0)
    expect_equal(test_1$d, (12-2)/(36+12+2+0))
    expect_true(abs(test_1$l - 0.0569) < 1e-4)
    expect_true(abs(test_1$u - 0.3404) < 1e-4) 

    test_2 <- ci.two.dep.props(ci=95, e=18, f=12, g=2, h=18)
    expect_equal(test_2$d, (12-2)/(18+12+2+18))
    expect_true(abs(test_2$l - 0.0562) < 1e-4)
    expect_true(abs(test_2$u - 0.3290) < 1e-4)
    
    test_3 <- ci.two.dep.props(ci=95, e=53, f=0, g=0, h=1)
    expect_equal(test_3$d, (0-0)/(53+0+0+1))
    expect_true(abs(test_3$l - (-0.0729)) < 1e-4)
    expect_true(abs(test_3$u - 0.0729) < 1e-4)
    
    test_4 <- ci.two.dep.props(ci=95, e=0, f=30, g=0, h=0)
    expect_equal(test_4$d, (30-0)/(0+30+0+0))
    expect_true(abs(test_4$l - 0.8395) < 1e-4)
    expect_true(abs(test_4$u - 1.0) < 1e-4)
})
