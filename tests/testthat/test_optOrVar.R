# test_optOrVar.R
# Tests of name resolution search prioritization.
# Author: Vince Reuter
# Email: vpr9v@virginia.edu

context("optOrVar")

test_that("NULL result when neither option nor env. var. is set", {
  for (n in sapply(1:10, getRandVarName)) {
    stopifnot(is.null(getOption(n)) && identical("", Sys.getenv(n)))  
    expect_true(is.null(optOrVar(!!n)))
  }
})

test_that("Option is recovered", {
  for (n in sapply(1:10, getRandVarName)) {
    stopifnot(is.null(getOption(n)) && identical("", Sys.getenv(n)))  
    value = "dummy_test_value"
    opts = list(value)
    names(opts) = n
    options(opts)
    if (is.null(getOption(n))) stop("Failed to set option: ", n)
    expect_equal(optOrVar(!!n), value)
    opts[[n]] = NULL
    options(opts)
  }
})

test_that("Environment variable is recovered", {
  for (n in sapply(1:10, getRandVarName)) {
    stopifnot(is.null(getOption(n)) && identical("", Sys.getenv(n)))  
    value = "dummy_test_value"
    args = list(value)
    names(args) = n
    do.call(what = Sys.setenv, args = args)
    if (.isEmpty(Sys.getenv(n))) stop("Failed to set env var: ", n)
    expect_equal(optOrVar(!!n), value)
    Sys.unsetenv(n)
  }
})

test_that("Option trumps environment variable", {

})

test_that("NULL result when option or env. var. is empty", {

})

test_that("Name for which to fetch value must be text", {
  for (n in list(NULL, c("PROCESSED", "RESOURCES"))) {
    expect_error(optOrVar(!!n))
  }
})
