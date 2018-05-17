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

#' Creates a folder function for easy access to the directory
#' @param name An immutable key given to identify the current folder, and used as a
#'     string to create the new folder function
#' @param location     (Absolute) path to a folder that will be prepend when the
#'     specified folder function is called
#' @export
#' @examples
#' setff("PROC", "here")
setff = function(name, location) {
	l = list(location)
	varName = paste0(.PDIROPTTAG, name)
	names(l) = varName
	# Set the option
	options(l)
	message(name, ": ", getOption(varName))
	funcName = paste0(.PDIRFUNCTAG, name)

	tempFunc = function(...) {
		inff(name, ...)		
	}
	assign(funcName, tempFunc, envir=globalenv())
}

#' Prepends a parent folder to a given filename or directory.
#' 
#' Generic function to prepend an environment variable directory to your
#' relative filepath. This helps make your scripts portable because you can
#' point to all files with a relative path. This function uses a variable to
#' identify pre-specified root folders, and then it makes your relative path
#' into an absolute path by pre- pending the appropriate root folder. The
#' variables identify locations of root folders using a priority system; it look
#' first in R options, and then in environment variables.
#' @param ffName The name of a variable (either an R option or an environment
#'     variable) that contains the path to the root folder to prepend
#' @param ... One or more arguments that, when concatenated, specify a relative
#'     path to a file or folder within the parent folder represented by the
#'     folder function.
#' @export
#' @examples
#' inff("PROC", "data.txt")
inff = function(ffName, ...) {
	userPath = .sanitizeUserPath(...)
	varName = paste0(.PDIROPTTAG, ffName)
	# First check if there's an R option with this name.
	parentFolder = getOption(varName)
	# If not, try an environment variable
	if (is.null(parentFolder)) {
		parentFolder = Sys.getenv(ffName)
	}
	# Otherwise, bail out, what can we do?
	if (is.null(parentFolder)) {
		stop("No parent folder found for variable ", ffName)
	}
	outputPath = file.path(parentFolder, userPath)
	return(outputPath)
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

