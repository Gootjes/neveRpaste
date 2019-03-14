
sanitize_name <- function(x) {
  stringr::str_replace_all(string = x, pattern = "[^a-zA-Z0-9_]+", replacement = "_")
}

wrap <- function(x, name, id) {
  before <- paste0('`<w:bookmarkStart w:id="', id,'" w:name="', name,'" />`{=openxml}')
  after <- paste0('`<w:bookmarkEnd w:id="', id,'" />`{=openxml}')

  paste(before, x, after, sep = "")
}

#' @title Produce a bookmark
#'
#' @param x The object to be bookmarked
#' @param name If NULL (default), a name is deducted from `x`
#'
#' @export
bookmark <- function(x, name = NULL) {

  if(is.null(name)) {
    args <- as.list(match.call())
    name <- names(args)$x
  }

  name <- sanitize_name(name)

  i <- bookmark_counter$next_value()

  knitr::asis_output(wrap(x = x, name = name, id = i))

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

neverpaste_knitr_inline <- function(output, options) {
  i <- bookmark_counter$next_value()
  n <- make_name(i)

  before <- paste0('`<w:bookmarkStart w:id="', i,'" w:name="', n,'" />`{=openxml}')
  after <- paste0('`<w:bookmarkEnd w:id="', i,'" />`{=openxml}')

  paste0(before, output, after)
}

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

