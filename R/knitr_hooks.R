
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
