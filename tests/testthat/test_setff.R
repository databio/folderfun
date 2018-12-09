# test_setff.R
# Tests of binding value to name.
# Author: Vince Reuter
# Email: vpr9v@virginia.edu

context("setff")

test_that("result of function created matches value provided",  {
	ns = c("NEWPROCVAR", "RAND_DATA_VAR", "ProcData", "RawData", "ROUT.DIR", "PROCESSED.PROJECT")
	xs = c("processed", "/home/data", "proc/data", "RawData", "output", "proc_proj")
	if (length(ns) != length(xs)) stop(sprintf(
		"%d names and %d values as test inputs", length(ns), length(xs)))
	for (i in 1:length(ns)) {
		expect_true(.isEmpty(optOrVar(!!ns[i])))
		setff(ns[i], path = xs[i])
		expect_equal(eval(get(paste0(.PDIRFUNCTAG, !!ns[i]))()), !!xs[i])
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
		expect_warning(setff(target, path = fixed, currVar = varnames[i]))
		func = paste0(.PDIRFUNCTAG, "DUMMY_TEST_VAR")
		expect_equal(eval(get(func)()), fixed)
		cleanFfSetting(target)
	}
})
