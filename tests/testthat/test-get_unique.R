test_that("get_unique() returns correct output", {
  dat <- data.frame(
    x = c(1, 1, 2),
    y = c("B", "B", "A"),
    z = factor(x = c("cat", "cat", "dog"), levels = c("dog", "cat"))
  )
  expect_equal(get_unique(dat, x), c(1, 2))
  expect_equal(get_unique(dat, y), c("A", "B"))
  expect_equal(get_unique(dat, z), c("dog", "cat"))
})
