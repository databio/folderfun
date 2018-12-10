# test_setff.R
# Tests of binding value to name.
# Author: Vince Reuter
# Email: vpr9v@virginia.edu

context("setff")

test_that("there is no double slashes in the produced path",  {
  ns = c("NEWPROCVAR", "RAND_DATA_VAR", "ProcData", "RawData")
  xs = c("test/path", "//test/path", "/test/path//", "testpath//")
  if (length(ns) != length(xs)) stop(sprintf(
    "%d names and %d values as test inputs", length(ns), length(xs)))
  for (i in 1:length(ns)) {
    expect_true(.isEmpty(optOrVar(!!ns[i])))
    setff(ns[i], path = xs[i])
    expect_false(grepl(pattern="//", x=eval(get(paste0(.FFTAGFUNC, !!ns[i]))())))
    cleanFfSetting(ns[i])
  }
})

test_that("result of function created matches value provided",  {
	ns = c("NEWPROCVAR", "RAND_DATA_VAR", "ProcData", "RawData", "ROUT.DIR", "PROCESSED.PROJECT")
	xs = c("processed", "/home/data", "proc/data", "RawData", "output", "proc_proj")
	if (length(ns) != length(xs)) stop(sprintf(
		"%d names and %d values as test inputs", length(ns), length(xs)))
	for (i in 1:length(ns)) {
		expect_true(.isEmpty(optOrVar(!!ns[i])))
		setff(ns[i], path = xs[i])
		expect_equal(eval(get(paste0(.FFTAGFUNC, !!ns[i]))()), !!xs[i])
		cleanFfSetting(ns[i])
	}
})

test_that("setting explicit empty is prohibited", {
	vals = list(NULL, "", character(0), logical(0), numeric(0), integer(0))
	varnames = lapply(1:length(vals), getRandVarName)
	for (i in 1:length(vals)) {
		expect_error(setff(!!varnames[[i]], path = !!vals[[i]]))
	}
})

test_that("attempt to derive value from empty varname is prohibited", {
	vals = list(NULL, "", character(0), logical(0), numeric(0), integer(0))
	varnames = lapply(1:length(vals), getRandVarName)
	for (i in 1:length(vals)) {
		expect_error(setff(!!varnames[[i]], currVal = !!vals[[i]]))
	}
})

test_that("explicit value or variable is required", {
	varnames = sapply(1:10, getRandVarName)
	for (i in 1:length(varnames)) { expect_error(setff(!!varnames[i])) }
})

test_that("explicit value trumps variable, and using both emits warning", {
	fixed = "explicit"
	target = "DUMMY_TEST_VAR"
	varnames = sapply(1:10, getRandVarName)
	for (i in 1:length(varnames)) {
		expect_warning(setff(target, path = fixed, pathVar = varnames[i]))
		func = paste0(.FFTAGFUNC, "DUMMY_TEST_VAR")
		expect_equal(eval(get(func)()), fixed)
		cleanFfSetting(target)
	}
})

test_that("name to set can be interpreted as environment variable holding value", {
	varVal = "dummyTestValue"
	checkClean = function(vn) { if (!identical("", Sys.getenv(vn))) {
		stop(sprintf("Name %s is already set as env var: %s", vn, Sys.getenv(vn))) } }
	for (n in sapply(1:10, getRandVarName)) {
		checkClean(n)
		envArg = list(varVal)
		names(envArg) = n
		do.call(what = Sys.setenv, args = envArg)
		if (identical("", Sys.getenv(n))) stop("Failed to set env var: ", n)
		setff(n)
		expect_equal(get(paste0(.FFTAGFUNC, !!n))(), varVal)
		cleanFfSetting(n)
		Sys.unsetenv(n)
		checkClean(n)
	}
})

test_that("name to set can be interpreted as name of current option holding value", {
	varVal = "optionValueTest"
	checkClean = function(opt) { if (!is.null(getOption(opt))) {
		stop(sprintf("Option %s is already set", opt)) } }
	for (n in sapply(1:10, getRandVarName)) {
		checkClean(n)
		optArg = list(varVal)
		names(optArg) = n
		options(optArg)
		if (is.null(getOption(n))) stop("Failed to set option: ", n)
		setff(n)
		expect_equal(get(paste0(.FFTAGFUNC, !!n))(), varVal)
		cleanFfSetting(n)
		optArg = list(NULL)
		names(optArg) = n
		options(optArg)
		checkClean(n)
	}
})
