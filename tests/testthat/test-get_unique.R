test_that("get_unique() returns correct output", {
  dat <- tibble::tibble(
    w = c(NA, 1, "B"),
    x = c(1, 1, 2),
    y = c("B", "B", "A"),
    z = factor(x = c("cat", "cat", "dog"), levels = c("dog", "cat")),
    u = factor(x = c("cat", "cat", "dog"), levels = c("dog", "pig", "cat"))
  )
  expect_equal(get_unique(dat, w), c("1", "B"))
  expect_equal(get_unique(dat, w, drop_na = FALSE), c("1", "B", NA))
  expect_equal(get_unique(dat, x), c(1, 2))
  expect_equal(get_unique(dat, y), c("A", "B"))
  expect_equal(get_unique(dat, z), c("dog", "cat"))
  expect_equal(get_unique(dat, u), c("dog", "pig", "cat"))
  expect_equal(get_unique(dat, u, keep_fct_levels = FALSE), c("dog", "cat"))
})
