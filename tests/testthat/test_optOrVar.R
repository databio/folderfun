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
    opts = list(NULL)
    names(opts) = n
    options(opts)
    stopifnot(is.null(getOption(n)))
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
  varnames = sapply(1:10, getRandVarName)
  optValues = sapply(1:10, getRandVarName)
  envVarValues = sapply(1:10, getRandVarName)
  for (i in 1:10) {
    n = varnames[i]
    stopifnot(is.null(getOption(n)) && identical("", Sys.getenv(n)))
    optVal = list(optValues[i])
    names(optVal) = n
    options(optVal)
    envArg = list(envVarValues[i])
    names(envArg) = n
    do.call(Sys.setenv, args = envArg)
    stopifnot(!is.null(getOption(n)) && !identical("", Sys.getenv(n)))
    expect_equal(optOrVar(!!n), !!optValues[i])
    Sys.unsetenv(n)
    optVal = list(NULL)
    names(optVal) = n
    options(optVal)
    stopifnot(is.null(getOption(n)) && identical("", Sys.getenv(n)))
  }
})

test_that("NULL result when option is empty", {
  varname = "dummy_test_opt"
  for (x in list("", logical(0), character(0), numeric(0), integer(0))) {
    stopifnot(is.null(getOption(varname)))
    optArg = list(x)
    names(optArg) = varname
    options(optArg)
    if (is.null(getOption(varname))) stop("Failed to set option: ", varname)
    message("CLASS: ", class(optOrVar(varname)))
    expect_true(is.null(optOrVar(varname)))
    optArg = list(NULL)
    names(optArg) = varname
    options(optArg)
    stopifnot(is.null(getOption(varname)))
  }
})

test_that("Name for which to fetch value must be text", {
  for (n in list(NULL, c("PROCESSED", "RESOURCES"))) {
    expect_error(optOrVar(!!n))
  }
})
