context("Testing flextable wrapping")

library(flextable)

testthat::test_that(desc = "Flextable wrapping", code = {
  testthat::expect_equal(
    object = wrap(flextable(data.frame(A=1:3, B=4:6, C = 7:9)), name = "table", id = 1),
    expected = readLines(con = "test_flextable_wrapping.dat", n = 1, warn = FALSE)
  )
})
