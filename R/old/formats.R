OpenOffice <- function() {


  .this <- environment()

  setContent <- function(md) {
    .this$content <- mdToHtml(md)
  }

  build <- function() {
    header_path <- system.file("openoffice/header.html", package = "neverpaste")
    footer_path <- system.file("openoffice/footer.html", package = "neverpaste")

    header <- stringi::stri_read_lines(header_path, encoding = "UTF-8")
    footer <- stringi::stri_read_lines(footer_path, encoding = "UTF-8")

    c(
      header,
      .this$content,
      footer
    )
  }

  environment()
}

MicrosoftWord <- function() {

  .this <- environment()

  setContent <- function(md) {
    html <- mdToHtml(md)

    html <- gsub(pattern = "<em>", replacement = "<i>", x = html)
    html <- gsub(pattern = "</em>", replacement = "</i>", x = html)

    html <- gsub(pattern = "<strong>", replacement = "<b>", x = html)
    html <- gsub(pattern = "</strong>", replacement = "</b>", x = html)

    html <- gsub(pattern = "<p>", replacement = "", x = html)
    html <- gsub(pattern = "</p>", replacement = "", x = html)

    .this$content <- html
  }

  build <- function() {
    header_path <- system.file("microsoftword/header.html", package = "neverpaste")
    footer_path <- system.file("microsoftword/footer.html", package = "neverpaste")

    header <- stringi::stri_read_lines(header_path)
    footer <- stringi::stri_read_lines(footer_path)

    c(
      header,
      .this$content,
      footer
    )
  }

  environment()

}

Excel <- function() {
  .this <- environment()



  environment()
}
