context("Testing sanitization of names")

testthat::test_that(desc = "Sanity", code = {
  testthat::expect_equal(
    object = sanitize_name("kse0- 2q -afkpa2k _1"),
    expected = "kse0_2q_afkpa2k__1"
  )
})
