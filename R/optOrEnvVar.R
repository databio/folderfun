#' Retrieval of value associated with a name
#'
#' \code{optOrEnvVar} retrieves that value assocaited with the 
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
optOrEnvVar = function(name) {
	opt = getOption(name)
	res = if (.nonempty(opt)) opt else Sys.getenv(name)
	if (.nonempty(res)) res else NULL
}
