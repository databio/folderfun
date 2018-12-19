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
.FFTAGOPT = "FF_"
.FFTAGFUNC = "ff"


#' Creator of function to reference project path.
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
#' @param postpend Value(s) with which to make subpath relative to main \code{path} 
#'    or value fetched from option or environment variable.
#' @return A function named \code{ff<name>} that when executed without 
#'    arguments points to the \code{path} and appends the provided argument to it
#'    if any were provided.
#' @export
#' @seealso See \href{http://code.databio.org/folderfun/articles/intro.html}{this vignette} for more
#'  detailed explanation of the concept
#' @examples
#' setff("PROC", "/path/to/directory")
setff = function(name, path = NULL, pathVar = NULL, postpend = NULL) {
	if (.isEmpty(path)) {
    path = if (.isEmpty(pathVar)) .lookup(name) else optOrEnvVar(pathVar)
    if (.isEmpty(path)) stop("Attempted to set empty value for ", name)
  } else if (!.isEmpty(pathVar)) { warning("Explicit value provided; ignoring ", pathVar) }
  if (.nonempty(postpend)) {
    if (is.character(postpend)) { postpend = list(postpend) }
    if (is.list(postpend)) { path = file.path(path, do.call(file.path, postpend)) }
    else { stop(sprintf("Invalid argument to postpend: %s (%s)", postpend, class(postpend))) }
  }
	l = list(path)
	varName = paste0(.FFTAGOPT, name)
	names(l) = varName
	# Set the option
	options(l)
	funcName = paste0(.FFTAGFUNC, name)
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
  invisible(tempFunc)
}


.isEmpty = function(x) is.null(x) || identical(x, "") || length(x) == 0
.nonempty = function(x) !.isEmpty(x)

# Implement priority lookup of a name's value. For each name variant, first 
# attempt lookup as option, then as environment variable. First try the 
# name exactly as given, then as uppercase, finally as lowercase.
.lookup = function(name) {
  for (n in c(name, toupper(name), tolower(name))) {
    res = optOrEnvVar(n)
    if (.nonempty(res)) return(res)
  }
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
