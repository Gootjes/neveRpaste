
#' @export
knit_print.neverpaste_bookmark <- function(x, ...) {
  class(x) <- setdiff(class(x), "neverpaste_bookmark")
  bookmark(x = x, name = attributes(x)$neverpaste$name, sep = attributes(x)$neverpaste$sep)
}
