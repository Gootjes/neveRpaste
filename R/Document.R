#' Produce a new document to work with. Use the \code{add} function, and when finished, use \code{save}.
#'
#' @return A neverpaste Document.
#'
#' @export
Document <- function() {

  this <- environment()
  this$bookmarks <- list()

  public <- local({

    add <- function(...) {
      args <- list(...)

      r <- constructBookmarks(args)

      inter <- intersect(names(r), names(this$bookmarks))

      if(length(inter) > 0) {
        stop(paste0("Duplicate bookmarks are not allowed: ", paste(inter, collapse = ", ")))
      }

      for(bookmark in names(r)) {
        text <- r[[bookmark]]
        this$bookmarks[[bookmark]] <- wrapInBookmark(bookmark, mdToHtml(text))
      }

      invisible()
    }

    remove <- function(...) {
      args <- list(...)

      r <- constructBookmarks(args)

      inter <- intersect(names(r), names(this$bookmarks))

      if(length(inter) > 0) {

      }

      for(bookmark in names(inter)) {
        this$bookmarks[[bookmark]] <- NULL
      }

      invisible()
    }

    build <- function() {
      unlist(this$bookmarks)
    }

    save <- function(path, format = "OpenOffice") {

      d <- NULL
      if(format == "OpenOffice") {
        d <- OpenOffice()
      }

      d$setContent(public$build())

      b <- d$build()
      Encoding(b) <- "UTF-8"

      stringi::stri_write_lines(str = b, fname = path, encoding = "UTF-8")

      invisible()
    }

    environment()
  })

  structure(public, class = "neverpasteDocument")

}
