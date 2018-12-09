# helper-setff.R
# Ancillary functions for setff tests
# Author: Vince Reuter
# Email: vpr9v@virginia.edu

getRandVarName = function(minLen = 10, maxLen = 20) {
  if (minLen < 1 || maxLen < 1) stop(sprintf(
    "Negative character count bound(s) for string randomization: %d, %d", 
    minLen, maxLen))
  if (maxLen < minLen) stop(sprintf(
    "Max chars < min chars for random string: %d < %d", maxLen, minLen))
  paste0(sample(letters, sample(minLen:maxLen)), collapse = "")
}
