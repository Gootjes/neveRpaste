context("Testing output of bookmark hooks")

test_that(desc = "Inline code", code = {
  expect_equal(
    object = neverpaste_knitr_inline("Inline code", list()),
    expected = '`<w:bookmarkStart w:id=\"1\" w:name=\"unnamed-bookmark-1\" />`{=openxml}Inline code`<w:bookmarkEnd w:id=\"1\" />`{=openxml}'
    )
})
