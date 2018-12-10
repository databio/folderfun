# PACKAGE DOCUMENTATION
#' Manages folder functions
#'
#' If you find yourself working on multiple different projects in R, you'll want
#' a series of folders pointing to raw data, processed data, plot results,
#' intermediate table outputs, etc. This package makes it easier to do that by
#' providing a quick and easy way to create and use functions for project-level
#' directories.
#'
#'
#' @docType package
#' @name folderfun
#' @author Nathan C. Sheffield
#'
#' @references \url{http://github.com/databio/folderfun}
NULL


# Global setting identifying options set by this package
.PDIROPTTAG = "FF_"
.PDIRFUNCTAG = "ff"


#' Create a folder function
#'
#' Creates a folder function for easy access to the directory, named 
#' by prepending a prefix specific to this package to the given \code{name}.
#' If neither an explicit \code{path} not a \code{pathVar} holding the value 
#' to set is provided, then the given \code{name} will be treated as an option 
#' or environment variable name, and those locations will be searched for the 
#' value to be returned when the function created here is called.
#' 
#' @param name An immutable key given to identify the current folder, and used as a
#'     string to create the new folder function
#' @param path An absolute path to a folder that will be prepend when the
#'     specified folder function is called
#' @param pathVar Name of the currently set variable whose value should 
#'    be bound to \code{name}. First \code{getOption} is used, and then 
#'    \code{Sys.getenv}.
#' @return A function named \code{ff<name>} that when executed without 
#'    arguments points to the \code{path} and appends the provided argument to it
#'    if any were provided.
#' @export
#' @seealso See \href{http://code.databio.org/folderfun/articles/intro.html}{this vignette} for more
#'  detailed explanation of the concept
#' @examples
#' setff("PROC", "/path/to/directory")
setff = function(name, path = NULL, pathVar = NULL) {
	if (.isEmpty(path)) {
    path = optOrVar(if (.isEmpty(pathVar)) name else pathVar)
  } else if (!.isEmpty(pathVar)) { warning("Explicit value provided; ignoring ", pathVar) }
	if (.isEmpty(path)) stop("Attempted to set empty value for ", name)
	l = list(path)
	varName = paste0(.PDIROPTTAG, name)
	names(l) = varName
	# Set the option
	options(l)
	funcName = paste0(.PDIRFUNCTAG, name)
	tempFunc = function(...) {
	  userPath = .sanitizeUserPath(...)
	  # First check if there's an R option with this name.
	  parentFolder = getOption(varName)
	  if (is.null(parentFolder)) {
	    stop("No parent folder found for variable ", name)
	  }
	  outputPath = if (.isEmpty(userPath)) parentFolder else file.path(parentFolder, userPath)
	  # prevent returing paths with double slashes
	  outputPath = gsub("//","/",outputPath)
	  return(outputPath)	
	}
	assign(funcName, tempFunc, envir=globalenv())
	message("Created folder function ", funcName, "(): ", tempFunc())
  return(tempFunc)
}


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
  pdirOpts = grep(.PDIROPTTAG, optionNames)
  if (length(pdirOpts) == 0) NULL else {
  	pdirOptNames = optionNames[pdirOpts]
	  pdirOptVals = options()[pdirOpts]
	  funcNames = paste0(.PDIRFUNCTAG, sub(.PDIROPTTAG, "", pdirOptNames))
	  cbind(funcNames, pdirOptVals)
  }
}


#' Retrieval of value associated with a name
#'
#' \code{optOrVar} retrieves that value assocaited with the 
#' name provided as an argument, prioritizing in its search 
#' by first interpreting the given argument as an \code{R}
#' option name, and then trying an interpretation as an 
#' environment variable if and only if the \code{R} option 
#' is not set or is an empty string or vector.
#'
#' @param name The name of the option or env var to fetch
#' @return The value associated with the given \code{name}, 
#'    returning \code{NULL} if and only if the \code{name} is 
#'    set neither as an option nor as environment variable.
#' @export
optOrVar = function(name) {
	opt = getOption(name)
	res = if (.nonempty(opt)) opt else Sys.getenv(name)
	if (.nonempty(res)) res else NULL
}


.isEmpty = function(x) is.null(x) || identical(x, "") || length(x) == 0
.nonempty = function(x) !.isEmpty(x)


# paste0() if given no values returns character(0); this doesn't play
# nicely with file.path, which returns bad value if any of the values are
# bad, instead of ignoring them. This function changes the default output to an
# empty string so it can be passed to file.path without problems.
.sanitizeUserPath = function(...) {
  userPath = paste0(...)
  if (identical(userPath, character(0))) {
    # for a blank function call; that's allowed, give parent dir.
    userPath = ""
  }
  return(userPath)
}
