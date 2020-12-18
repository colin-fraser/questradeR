test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("endpoints works", {
  account_set <- list(api_server = "https://abc.com")
  expect_equal(qt_api_url("time", account_set), "https://abc.com/v1/time")
  expect_equal(qt_api_url("accounts", account_set), "https://abc.com/v1/accounts")
  expect_error(qt_api_url("account_positions", account_set))
  expect_equal(qt_api_url("account_positions", account_set, list(account_id = 123)), "https://abc.com/v1/accounts/123/positions")
  expect_equal(qt_api_url("account_balances", account_set, list(account_id = 123)), "https://abc.com/v1/accounts/123/balances")
})

test_that("requests work", {
  auth_token <- Sys.getenv("QUESTRADER_ACCESS_TOKEN")
  expect_s3_class(qt_api_time(auth_token = auth_token), "POSIXct")
})
