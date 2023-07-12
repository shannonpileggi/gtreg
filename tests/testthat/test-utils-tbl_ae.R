test_that("check_factor() fails when needed", {
  expect_equal(.check_factor("x"), factor("x"))
  expect_equal(.check_factor(factor("x")), factor("x"))

  expect_error(.check_factor(NA), "Input must be a factor or character vector.")
})

test_that("factor unchanged if no missing levels", {

  f1 <- factor(letters[1:3])
  f2 <- .fct_explicit_na(f1)

  expect_identical(f1, f2)
})

test_that("converts implicit NA", {

  f1 <- factor(c("a", NA))
  f2 <- .fct_explicit_na(f1)

  expect_equal(f2, factor(c("a", "(Missing)"), levels = c("a", "(Missing)")))
})

test_that("converts explicit NA", {

  f1 <- factor(c("a", NA), exclude = NULL)
  f2 <- .fct_explicit_na(f1)

  expect_equal(f2, factor(c("a", "(Missing)"), levels = c("a", "(Missing)")))
})



test_that("when chooses the correct action", {

  x <-
    1:5 %>%
    .when(
      sum(.) <=  50 ~ sum(.),
      sum(.) <= 100 ~ sum(.) / 2,
      ~ 0
    )

  expect_equal(x, 15)

  y <-
    1:10 %>%
    .when(
      sum(.) <=  50 ~ sum(.),
      sum(.) <= 100 ~ sum(.) / 2,
      ~ 0
    )

  expect_equal(y, sum(1:10) / 2)

  z <-
    1:100 %>%
    .when(
      sum(.) <=  50 ~ sum(.),
      sum(.) <= 100 ~ sum(.) / 2,
      ~ 0
    )

  expect_equal(z, 0)
})

test_that("named arguments work with when", {

  x <-
    1:10 %>%
    .when(
      sum(.) <=     x ~ sum(.) * x,
      sum(.) <= 2 * x ~ sum(.) * x / 2,
      ~ 0,
      x = 60
    )

  expect_equal(x, sum(1:10) * 60)
})

test_that("default values work without a formula", {

  x <- iris %>%
    subset(Sepal.Length > 10) %>%
    .when(
      nrow(.) > 0 ~ .,
      head(iris, 10)
    )

  expect_equal(x, head(iris, 10))
})

test_that("error when named arguments have no matching conditions", {

  expect_error(1:5 %>% .when(a = sum(.) < 5 ~ 3))
})


