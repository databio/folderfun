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
#' Creates a folder function for easy access to the directory
#' @param name An immutable key given to identify the current folder, and used as a
#'     string to create the new folder function
#' @param path An absolute path to a folder that will be prepend when the
#'     specified folder function is called
#' @param currVar Name of the currently set variable whose value should 
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
setff = function(name, path = NULL, currVar = NULL) {
	if (.isEmpty(path)) {
		if (.isEmpty(currVar)) stop("To set a variable, path or currVar must be provided.")
		path = optOrVar(currVar)
		if (.isEmpty(path)) stop(sprintf(
			"No value provided, and variable %s is empty", currVar))
	} else if (!.isEmpty(currVar)) { warning("Explicit value provided; ignoring ", currVar) }
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
	  outputPath = file.path(parentFolder, userPath)
	  # prevent returing paths with double slashes
	  outputPath = gsub("//","/",outputPath)
	  return(outputPath)	
	}
	assign(funcName, tempFunc, envir=globalenv())
	message("Created folder function ", funcName, "(): ", tempFunc())
}


#' Show project environment variables
#'
#' Displays the environment variables that are set and used by this package.
#'@export
listff = function() {
  optionNames = names(options())
  pdirOpts = grep(.PDIROPTTAG, optionNames)
  pdirOptNames = optionNames[pdirOpts]
  pdirOptVals = options()[pdirOpts]
  funcNames = paste0(.PDIRFUNCTAG, sub(.PDIROPTTAG, "", pdirOptNames))
  cbind(pdirOptVals, funcNames)
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
#' @param name
#' @return The value associated with the given \code {name}, 
#'    returning \code{NULL} if and only if the \code{name} is 
#'    set neither as an option nor as environment variable.
#' @export
optOrVar = function(name) {
	opt = getOption(name)
	res = if (.nonempty(name)) opt else Sys.getenv(name)
	if (.nonempty(res)) res else NULL
}


.isEmpty = function(x) is.null(x) || identical(x, "") || length(x) == 0
.nonempty = function(x) !.isEmpty(var)


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
