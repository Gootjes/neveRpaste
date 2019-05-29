

#' @title Produce a bookmark
#'
#' @param x The object to be bookmarked
#' @param name If NULL (default), a name is deducted from `x`
#' @param sep The seperator to use to concatenate bookmark and `x`
#'
#' @export
bookmark <- function(x, name = NULL, sep = "") {

  if(missing(x)) stop("x is missing")

  #if(length(x) > 1) stop("x must be of length one")

  if(is.null(name)) {
    args <- as.list(match.call())
    name <- deparse(args$x)
  }

  name <- sanitize_name(name)

  i <- bookmark_counter$next_value()

  knitr::asis_output(wrap(x = x, name = name, id = i, sep = sep))

}

#' @title Produce bookmarks
#'
#' @param ... Named arguments that you want to be bookmarked. If no name exists, it will be given a noname name.
#'
#' @return The bookmarked values pasted together.
#'
#' @export
bookmarks <- function(...) {
  args <- list(...)
  bookmarks <- names(args)

  bms <- Map(f = function(name){

    output <- args[[name]]

    #if(length(output) > 1) stop("x must be of length one")

    i <- bookmark_counter$next_value()
    if(nchar(name) == 0) {
      n <- make_name(i)
    } else {
      n <- name
    }

    wrap(x = output, name = n, id = i)

  }, names(args))

  knitr::asis_output(paste(bms, collapse = ""))

}


