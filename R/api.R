qt_api_url <- function(name, account_set = load_account_set(), url_values = NULL) {
  out <- httr::parse_url(account_set$api_server)
  out$path <- glue::glue(switch(name,
    "accounts" = "v1/accounts",
    "time" = "v1/time",
    "account_positions" = "v1/accounts/{account_id}/positions",
    "account_balances" = "v1/accounts/{account_id}/balances",
    "candles" = "v1/markets/candles/{symbol_id}",
    "search_symbol" = "v1/symbols/search"
  ), .envir = url_values)
  httr::build_url(out)
}

#' Get an API response
#'
#' @param name method name
#' @param account_set account set
#' @param query a list to pass as the query
#' @param url_values url values to glue into the url template, usually an ID
#' @param try how many times has this been called
#' @param auth_token a string to use for the access token. Usually this is not needed
#' but is used for testing.
#'
#' @return an httr response
#' @export
#'
qt_api_get <- function(name, account_set = load_account_set(), url_values = NULL, query = NULL,
                       try = 0, auth_token = NULL) {
  end_point <- qt_api_url(name, account_set, url_values)
  if (is.null(auth_token)) {
    auth <- bearer_token(account_set)
  } else {
    auth <- httr::add_headers(Authorization = paste("Bearer", auth_token))
  }
  resp <- httr::GET(end_point, auth, query = query)

  # check for expired token
  if (httr::status_code(resp) == 401) {
    if (try <= 1) {
      message("Invalid token. Attempting to refresh.")
      qt_refresh_token(account_set)
      resp <- qt_api_get(name, account_set, url_values, query, try = try + 1)
    } else {
      stop("Refreshing failed.")
    }
  }
  return(resp)
}

#' Get the current time
#'
#' @param account_set account set
#' @param auth_token override access token. Usually not needed.
#'
#' @return the current time
#' @export
#'
qt_api_time <- function(account_set = load_account_set(), auth_token = NULL) {
  qt_api_get("time", auth_token = auth_token) %>%
    httr::content() %>%
    lubridate::ymd_hms()
}

#' Get list of accounts
#'
#' Gets the list of accounts corresponding to account_set
#'
#' @param account_set account set
#'
#' @return a tibble with a list of accounts
#' @export
#'
qt_api_accounts <- function(account_set = load_account_set()) {
  qt_api_get("accounts") %>%
    pluck_and_map("accounts")
}

#' Get positions
#'
#' @param account_set account set
#' @param account_id account ID.
#'
#' @return a list of tibbles with positions
#' @export
#'
qt_api_account_positions <- function(account_id, account_set = load_account_set()) {
  qt_api_get("account_positions", account_set, list(account_id = account_id)) %>%
    pluck_and_map("positions")
}

#' Get account balances
#'
#' @param account_set account set
#' @param account_id account ID.
#'
#' @return a tibble with a list of account balances
#' @export
#'
qt_api_account_balances <- function(account_id, account_set = load_account_set()) {
  qt_api_get("account_balances", account_set, list(account_id = account_id)) %>%
    httr::stop_for_status("Getting account balance") %>%
    httr::content() %>%
    purrr::map_depth(2, tibble::as_tibble) %>%
    purrr::map(dplyr::bind_rows) %>%
    purrr::map(colnames_from_camel_case)
}

#' Get candlesticks
#'
#' @param account_set account set
#' @param start_time a datetime
#' @param end_time an end time
#' @param interval the time interval
#' @param symbol_id the ID of the symbol to look up.
#'
#' @return the current time
#' @export
#'
qt_api_candles <- function(symbol_id, start_time, end_time, interval =
                             c(
                               "OneMinute",
                               "TwoMinutes",
                               "ThreeMinutes",
                               "FourMinutes",
                               "FiveMinutes",
                               "TenMinutese",
                               "FifteenMinutes.",
                               "TwentyMinutes",
                               "HalfHour",
                               "OneHour",
                               "TwoHours",
                               "FourHours",
                               "OneDay",
                               "OneWeek",
                               "OneMonth",
                               "OneYear"
                             ), account_set = load_account_set()) {
  interval <- match.arg(interval)
  start_time <- format_time(start_time)
  end_time <- format_time(end_time)
  qt_api_get(
    "candles", account_set, list(symbol_id = symbol_id),
    list(startTime = start_time, endTime = end_time, interval = interval)
  ) %>%
    pluck_and_map("candles")
}

#' Search symbol
#'
#' @param prefix prefix to search for
#' @param account_set account set
#'
#' @return a dataframe with symbols
#' @export
#'
qt_search_symbol <- function(prefix, account_set = load_account_set()) {
  qt_api_get("search_symbol", account_set = account_set, query = list(prefix = prefix)) %>%
    pluck_and_map("symbols")
}

pluck_and_map <- function(resp, pluck_val) {
  resp %>%
    httr::content() %>%
    purrr::pluck(pluck_val) %>%
    purrr::map_df(tibble::as_tibble) %>%
    colnames_from_camel_case()
}

add_access_token_to_env <- function(account_set = load_account_set()) {
  Sys.setenv(QUESTRADER_ACCESS_TOKEN = get_stored_token(account_set, ACCESS_TOKEN))
}
