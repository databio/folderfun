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
#' @param location An absolute path to a folder that will be prepend when the
#'     specified folder function is called
#' @return A function named \code{ff<name>} that when executed without
#' arguments points to the \code{location} and appends the provided argument to it
#' if any were provided.
#' @export
#' @seealso See \href{http://code.databio.org/folderfun/articles/intro.html}{this vignette} for more
#'  detailed explanation of the concept
#' @examples
#' setff("PROC", "/path/to/directory")
setff = function(name, location) {
  l = list(location)
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
    outputPath = ifelse(userPath == "",
                        parentFolder ,
                        file.path(parentFolder, userPath))
    # prevent returing paths with double slashes
    outputPath = gsub("//", "/", outputPath)
    return(outputPath)
  }
  assign(funcName, tempFunc, envir = globalenv())
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
