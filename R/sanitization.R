
sanitize_name <- function(x) {
  stringr::str_replace_all(string = x, pattern = "[^a-zA-Z0-9_]+", replacement = "_")
}


make_name <- function(count) {
  paste0("unnamed-bookmark-", count, sep = "", collapse = "")
}
