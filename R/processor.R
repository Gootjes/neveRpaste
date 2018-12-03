
htmlToDocx <- function(html) {

}

htmlToOpenOffice <- function(html) {
  html
}

mdToHtml <- function(md) {

  md <- as.character(md)

  md <- markdown::markdownToHTML(text = md, fragment.only = T)

  md <- stringr::str_replace(string = md, pattern = "^<p>", replacement = "")
  md <- stringr::str_replace(string = md, pattern = "\\n$", replacement = "")
  md <- stringr::str_replace(string = md, pattern = "</p>$", replacement = "")

  squiggly_count <- stringr::str_count(md, "(?<!~)~(?!~)")
  if(squiggly_count > 0) {
    while(squiggly_count > 0) {

      md <- stringr::str_replace(string = md, pattern = "~", replacement = "<sub>")

      if(stringr::str_count(md, "~") == 0) {
        stop("cannot parse subscripts because ~ count is uneven", call. = F)
      }

      md <- stringr::str_replace(string = md, pattern = "~", replacement = "</sub>")

      squiggly_count <- stringr::str_count(md, "(?<!~)~(?!~)")
    }
  }

  md <- iconv(md, from = "ascii", "UTF-8")

  htmlToUTF8(md)
}

wrapInBookmark <- function(bookmark, html) {
  paste0("<p><a name = \"", bookmark, "\">", html, "</a></p>")
}

constructBookmarks <- function(obj) {

  b <- do.call(c, list(obj, recursive = T))

  b <- sapply(b, as.character)

  n <- names(b)

  n <- gsub(pattern = "[^a-zA-Z0-9_]+", replacement = "_", n)
  n <- gsub(pattern = "_$", replacement = "", n)

  names(b) <- n

  b

}

htmlToUTF8 <- function(x) {
  sapply(x, function(sx){
    matches <- stringr::str_match_all(sx, "<U[+]([A-Z0-9]+)>")[[1]]
    Reduce(f = function(nx, i) {
      hex <- as.hexmode(matches[i,2])
      from <- paste0("<U[+]",matches[i,2],">")
      stringr::str_replace_all(string = nx, pattern = from, replacement = intToUtf8(hex))
    }, x = 1:nrow(matches), init = sx)
  }, USE.NAMES = F)
}
