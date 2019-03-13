#' Print a Document to the console.
#'
#'
#' @export
print.neverpasteDocument <- function(doc) {
  cat("<neverpasteDocument>\n")
}

#' Convert a Document to character.
#'
#'
#' @export
as.character.neverpasteDocument <- function(doc) {
  doc$build()
}
