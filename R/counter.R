
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
