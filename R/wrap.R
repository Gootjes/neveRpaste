

#' @importFrom knitr knit_print
wrap <- function(x, name, id, sep = "") {
  capture.output(x <- knit_print(x), file = "NUL")

  before <- paste0('`<w:bookmarkStart w:id="', id,'" w:name="', name,'" />`{=openxml}')
  after <- paste0('`<w:bookmarkEnd w:id="', id,'" />`{=openxml}')

  paste(before, x, after, sep = sep)
}
