
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

bookmark_inline <- function(output, options) {
  if(before) {
    i <- bookmark_counter$next_value()
    n <- make_name(i)
    paste0('`<w:bookmarkStart w:id="', i,'" w:name="', n,'" />`{=openxml}')
  } else {
    i <- bookmark_counter$.get()
    paste0('`<w:bookmarkEnd w:id="', i,'" />`{=openxml}')
  }
}

bookmark <- function(output, options) {
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

inline <- function(output, options) {
  i <- bookmark_counter$next_value()
  n <- make_name(i)

  before <- paste0('`<w:bookmarkStart w:id="', i,'" w:name="', n,'" />`{=openxml}')
  after <- paste0('`<w:bookmarkEnd w:id="', i,'" />`{=openxml}')

  paste0(before, output, after)
}

chunk <- function(output, options) {

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

  knitr::knit_hooks$set(bookmark_inline = bookmark_inline)

  knitr::knit_hooks$set(bookmark = bookmark)

  knitr::knit_hooks$set(inline = inline)

  knitr::knit_hooks$set(chunk = chunk)

}
#
# bookmark_options <- list(
#   bookmark_id = -1
# )
#
# bookmark <- function(x, name = NULL) {
# #   UseMethod("bookmark", x)
# # }
# #
# # bookmark.character <- function() {
#   if(is.null(name)) {
#     name <- as.character(as.list(match.call())$x)
#   }
#
#   bookmark_options$bookmark_id <- bookmark_options$bookmark_id + 1
#
#   structure(list(x = x, id = bookmark_options$bookmark_id, name = name), class = "bookmark")
# }
#
# knit_print.bookmark <- function(x, ...) {
#
#   UseMethod("knit_print.bookmark", x$x)
#
# }
#
# knit_print.bookmark.default <- function(x, ...) {
#   r <- x
#   knitr::knit_print(knitr::asis_output(paste0("`<w:bookmarkStart w:id=\"",x$id,"\" w:name=\"",x$name,"\" />`{=openxml}",r,"`<w:bookmarkEnd w:id=\"",x$id,"\" />`{=openxml}")))
# }
#
# knit_print.bookmark.flextable <- function(x, ...) {
#
#   r <- knitr::knit_print(x$x)
#
#
#   knitr::knit_print(knitr::asis_output(paste0("`<w:bookmarkStart w:id=\"",x$id,"\" w:name=\"",x$name,"\" />`{=openxml}",r,"`<w:bookmarkEnd w:id=\"",x$id,"\" />`{=openxml}")))
# }
#
# print_bookmark <- function(x, name = NULL) {
#   if(is.null(name)) {
#     name <- as.character(as.list(match.call())$x)
#   }
#
#   bookmark_options$bookmark_id <- bookmark_options$bookmark_id + 1
#
#   knitr::knit_print(knitr::asis_output(paste0("`<w:bookmarkStart w:id=\"",bookmark_options$bookmark_id,"\" w:name=\"",name,"\" />`{=openxml}")))
#   knitr::knit_print(x)
#   knitr::knit_print(knitr::asis_output(paste0("`<w:bookmarkEnd w:id=\"",bookmark_options$bookmark_id,"\" />`{=openxml}")))
# }
