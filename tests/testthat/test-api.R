
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
  if (auth_token == "") {
    stop("No environment variable QUESTRADER_ACCESS_TOKEN found. Set it to run the test.")
  }
  expect_s3_class(qt_api_time(auth_token = auth_token), "POSIXct")
})

test_that("qt_api requests return data frames", {
  auth_token <- Sys.getenv("QUESTRADER_ACCESS_TOKEN")
  accnum <- Sys.getenv("QUESTRADER_ACCOUNT_NUMBER")
  if (auth_token == "") {
    stop("No environment variable QUESTRADER_ACCESS_TOKEN found. Set it to run the test.")
  }
  if (accnum == "") {
    stop("No environment variable QUESTRADER_ACCOUNT_NUMBER found. Set it to run the test.")
  }
  expect_s3_class(qt_api_accounts(auth_token = auth_token), "tbl_df")
  expect_s3_class(qt_api_account_positions(accnum, auth_token = auth_token), "tbl_df")
  expect_s3_class(qt_api_account_balances(accnum, auth_token = auth_token)[['perCurrencyBalances']], "tbl_df")
}

)
