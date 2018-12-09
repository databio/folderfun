# test_listff.R
# Tests of name resolution search prioritization.
# Author: Vince Reuter
# Email: vpr9v@virginia.edu

context("test_listff")

test_that("Function/value matrix is empty when no variables are set", {
  funVarMat = listff()
  expect_true(inherits(funVarMat, "matrix"))
  expect_equal(nrow(funVarMat), 1)
  expect_equal(ncol(funVarMat), 1)
  expect_true(is.na(funVarMat[1, 1]))
})
