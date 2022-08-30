test_that("style_xxx works", {
  expect_error(
    style_xxx(7:10, width = 2, digits = 0),
    NA)

  expect_equal(
    style_xxx(7:10, width = 2, digits = 0),
    c("xx", "xx", "xx", "xx")
  )

  expect_equal(
    style_xxx(7:10, width = 5, digits = 2),
    c("xx.xx", "xx.xx", "xx.xx", "xx.xx")
  )


})
