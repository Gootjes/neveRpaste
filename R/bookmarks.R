
sanitize_name <- function(x) {
  stringr::str_replace_all(string = x, pattern = "[^a-zA-Z0-9_]+", replacement = "_")
}

#' @importFrom knitr knit_print
wrap <- function(x, name, id, sep = "") {
  capture.output(x <- knit_print(x), file = "NUL")

  before <- paste0('`<w:bookmarkStart w:id="', id,'" w:name="', name,'" />`{=openxml}')
  after <- paste0('`<w:bookmarkEnd w:id="', id,'" />`{=openxml}')

  paste(before, x, after, sep = sep)
}

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

bookmark_counter <- (function() {

  .initial <- 0

  counter <- .initial

  .get <- function() {
    counter
  }

  .increment <- function() {
    counter <<- counter + 1
  }

  .next <- function() {
    .increment()
    .get()
  }

  .reset <- function() {
    counter <<- .initial
  }

  list(.get = .get, .increment = .increment, next_value = .next, .reset = .reset)
})()

make_name <- function(count) {
  paste0("unnamed-bookmark-", count, sep = "", collapse = "")
}

#' @export
neverpaste_knitr_bookmark <- function(before, after, options) {
  if(before) {
    i <- bookmark_counter$next_value()

    n <- options$label
    if(is.null(n)) {
      n <- make_name(i)
    }
    paste0('`<w:bookmarkStart w:id="', i,'" w:name="', n,'" />`{=openxml}')
  } else {
    i <- bookmark_counter$.get()
    paste0('`<w:bookmarkEnd w:id="', i,'" />`{=openxml}')
  }
}

#' @export
neverpaste_knitr_inline <- function(output, options) {
  i <- bookmark_counter$next_value()
  n <- make_name(i)

  before <- paste0('`<w:bookmarkStart w:id="', i,'" w:name="', n,'" />`{=openxml}')
  after <- paste0('`<w:bookmarkEnd w:id="', i,'" />`{=openxml}')

  paste0(before, output, after)
}

#' @export
neverpaste_knitr_chunk <- function(output, options) {

  i <- bookmark_counter$next_value()
  n <- options$label
  if(is.null(n)) {
    n <- make_name(i)
  }

  before <- paste0('`<w:bookmarkStart w:id="', i,'" w:name="', n,'" />`{=openxml}')
  after <- paste0('`<w:bookmarkEnd w:id="', i,'" />`{=openxml}')

  paste0(before, output, after)
}

#' @title Hook into knitr to automatically bookmark output
#'
#' @description Setup the hooks for the use of of this package in combination with `knitr`. Every inline chunk or regular chunk will be bookmarked.
#' In the case of regular chunks, the label of the chunk will be used.
#'
#' @export
setup_knitr_bookmark_hooks <- function() {

  bookmark_counter$.reset()

  knitr::knit_hooks$set(bookmark = neverpaste_knitr_bookmark)

  knitr::knit_hooks$set(inline = neverpaste_knitr_inline)

  knitr::knit_hooks$set(chunk = neverpaste_knitr_chunk)

}

#' @export
as_bookmark <- function(x, name, sep = "") {
  class(x) <- c(class(x), "neverpaste_bookmark")
  structure(x, neverpaste = list(name = name, sep = sep))
  #x
}

#' @export
knit_print.neverpaste_bookmark <- function(x, ...) {
  class(x) <- setdiff(class(x), "neverpaste_bookmark")
  bookmark(x = x, name = attributes(x)$neverpaste$name, sep = attributes(x)$neverpaste$sep)
}
