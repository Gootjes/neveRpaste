
#' @export
as_bookmark <- function(x, name, sep = "") {
  class(x) <- c(class(x), "neverpaste_bookmark")
  structure(x, neverpaste = list(name = name, sep = sep))
  #x
}
