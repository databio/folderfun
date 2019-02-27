#' Display of project environment variables
#'
#' \code{listff} displays a two-column matrix of function names 
#' with which to access option / environment variable values 
#' associated with this package, along with the current value 
#' associated with each function.
#'
#' @return Two-column matrix in which first column contains function 
#'    names and the second contains the value associated with each.
#' @export
listff = function() {
  optionNames = names(options())
  pdirOpts = grep(.FFTAGOPT, optionNames)
  if (length(pdirOpts) == 0) NULL else {
  	pdirOptNames = optionNames[pdirOpts]
	  pdirOptVals = options()[pdirOpts]
	  funcNames = paste0(.FFTAGFUNC, sub(.FFTAGOPT, "", pdirOptNames))
	  cbind(funcNames, pdirOptVals)
  }
}
