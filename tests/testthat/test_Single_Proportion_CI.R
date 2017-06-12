library("propint") 

context("Single proportion CI")

test_that("ci.one.prop produces the results reported in Newcombe1998", {
  # First result
  expect_equal(ci.one.prop(ci=95, r=81, n=263)$p, 81/263)
  # need some tolerance for decimal rounding
  expect_true(abs(ci.one.prop(ci=95, r=81, n=263)$l - 0.2553) < 1e-4)
  expect_true(abs(ci.one.prop(ci=95, r=81, n=263)$u - 0.3662) < 1e-4)
  
  # Second result
  expect_equal(ci.one.prop(ci=95, r=15, n=148)$p, 15/148)
  # need some tolerance for decimal rounding
  expect_true(abs(ci.one.prop(ci=95, r=15, n=148)$l - 0.0624) < 1e-4)
  expect_true(abs(ci.one.prop(ci=95, r=15, n=148)$u - 0.1605) < 1e-4)  
})
