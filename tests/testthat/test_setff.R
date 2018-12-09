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
	}
})

test_that("setting explicit empty is prohibited", {
	for (val in c(NULL, "", character(0), logical(0), numeric(0), integer(0))) {
		expect_error(setff(!!getRandVarName(), path = val))
	}
})

test_that("attempt to derive value from empty varname is prohibited", {
	for (val in c(NULL, "", character(0), logical(0), numeric(0), integer(0))) {
		expect_error(setff(!!getRandVarName(), currVal = val))
	}
})

test_that("explicit value or variable is required", {

})

test_that("explicit value is preferred to variable", {})

test_that("explicit value + variable produces warning", {})
