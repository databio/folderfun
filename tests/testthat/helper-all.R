# helper-all.R
# Ancillary functions for setff tests
# Author: Vince Reuter
# Email: vpr9v@virginia.edu

# Randomize lowercase alphabetic string of length in specified range.
getRandVarName = function(minLen = 10, maxLen = 20) {
  if (minLen < 1 || maxLen < 1) stop(sprintf(
    "Negative character count bound(s) for string randomization: %d, %d", 
    minLen, maxLen))
  if (maxLen < minLen) stop(sprintf(
    "Max chars < min chars for random string: %d < %d", maxLen, minLen))
  paste0(sample(letters, sample(minLen:maxLen)), collapse = "")
}

# Remove (set as NULL) the option this package uses to store a named variable,
# and remove the variable storing its fetcher function.
cleanFfSetting = function(n) {
  optArg = list(NULL)
  names(optArg) = paste0(.PDIROPTTAG, n)
  options(optArg)
  do.call(what = rm, args = list(paste0(.PDIRFUNCTAG, n), pos = globalenv()))
}
