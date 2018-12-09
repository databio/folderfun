# test_listff.R
# Tests of name resolution search prioritization.
# Author: Vince Reuter
# Email: vpr9v@virginia.edu

context("test_listff")

test_that("Function/value matrix is empty when no variables are set", {
  expect_true(is.null(listff()))
})
