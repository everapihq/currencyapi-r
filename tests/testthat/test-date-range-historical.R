test_that("date range historical exchange rates check works", {

  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  w <- get_date_range_historical_exchange_rates("2021-01-01", "2021-05-05")

  expect_equal(class(w), "list")
  expect_equal(class(w$data$currencies.EUR.value), "numeric")

  x <- get_date_range_historical_exchange_rates("2021-01-01", "2021-05-05", "day")

  expect_equal(class(x), "list")
  expect_equal(class(x$data$currencies.EUR.value), "numeric")


  y <- get_date_range_historical_exchange_rates("2021-01-01", "2021-05-05", "day", "EUR")

  expect_equal(class(y), "list")
  expect_equal(class(y$data$currencies.USD.value), "numeric")

  z <- get_date_range_historical_exchange_rates("2021-01-01", "2021-05-05", "day", "EUR", "USD,AUD")

  expect_equal(class(z), "list")
  expect_equal(class(z$data$currencies.USD.value), "numeric")
})
