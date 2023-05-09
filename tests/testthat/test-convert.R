test_that("convert amount check works", {

  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  x <- convert_exchange_rates(8)

  expect_equal(class(x), "list")
  expect_equal(class(x$data$EUR$value), "numeric")

  y <- convert_exchange_rates(8, "2021-01-01")

  expect_equal(class(y), "list")
  expect_equal(class(y$data$EUR$value), "numeric")

  z <- convert_exchange_rates(8, "2021-01-01", "EUR")

  expect_equal(class(z), "list")
  expect_equal(class(z$data$USD$value), "numeric")

})
