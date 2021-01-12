qt_api_url <- function(name, account_set = load_account_set(), url_values = NULL) {
  out <- httr::parse_url(account_set$api_server)
  out$path <- glue::glue(switch(name,
    "accounts" = "v1/accounts",
    "time" = "v1/time",
    "account_positions" = "v1/accounts/{account_id}/positions",
    "account_balances" = "v1/accounts/{account_id}/balances",
    "candles" = "v1/markets/candles/{symbol_id}",
    "search_symbol" = "v1/symbols/search",
    "symbols" = "v1/symbols",
    "quote" = "v1/markets/quotes/"
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
      resp <- qt_api_get(name, account_set, url_values, query, try = try + 1,auth_token = auth_token)
    } else {
      stop("Refreshing failed.")
    }
  }
  stop_for_error(resp)
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
qt_api_accounts <- function(account_set = load_account_set(), auth_token = NULL) {
  qt_api_get("accounts", auth_token = auth_token) %>%
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
qt_api_account_positions <- function(account_id, account_set = load_account_set(), auth_token = NULL) {
  qt_api_get("account_positions", account_set, list(account_id = account_id),auth_token = auth_token) %>%
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
qt_api_account_balances <- function(account_id, account_set = load_account_set(), auth_token = NULL) {
  qt_api_get("account_balances", account_set, list(account_id = account_id),auth_token = auth_token) %>%
    httr::stop_for_status("Getting account balance") %>%
    httr::content() %>%
    purrr::map_depth(2, tibble::as_tibble) %>%
    purrr::map(dplyr::bind_rows) %>%
    purrr::map(colnames_from_camel_case)
}

qt_api_quotes <- function(ids, account_set = load_account_set(), auth_token = NULL) {
  qt_api_get("quote", account_set, query = list(ids = paste(ids, collapse = ',')),auth_token = auth_token)
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
                             ), account_set = load_account_set(), auth_token = NULL) {
  interval <- match.arg(interval)
  start_time <- format_time(start_time)
  end_time <- format_time(end_time)
  qt_api_get(
    "candles", account_set, list(symbol_id = symbol_id),
    list(startTime = start_time, endTime = end_time, interval = interval)
  ) %>%
    pluck_and_map("candles") %>%
    dplyr::mutate(dplyr::across(c(start, end), lubridate::ymd_hms))
}

#' Search symbol
#'
#' @param prefix prefix to search for
#' @param account_set account set
#'
#' @return a dataframe with symbols
#' @export
#'
qt_api_search_symbol <- function(prefix, account_set = load_account_set(), auth_token = NULL) {
  qt_api_get("search_symbol", account_set = account_set, query = list(prefix = prefix),auth_token = auth_token) %>%
    pluck_and_map("symbols")
}

qt_api_symbols_raw <- function(symbol_ids = NULL, symbol_names = NULL, account_set = load_account_set(), auth_token = NULL) {
  if (is.null(symbol_ids) + is.null(symbol_names) != 1) {
    stop("Exactly 1 of 'symbol_ids' or 'symbol_names' should be NULL")
  }
  if (!is.null(symbol_ids)) {
    payload <- list(ids = paste(symbol_ids, collapse = ","))
  } else if (!is.null(symbol_names)) {
    payload <- list(names = paste(symbol_names, collapse = ","))
  }
  qt_api_get("symbols", query = payload,auth_token = auth_token)
}

#' Get information on symbols
#'
#' @param symbol_names vector of exact names of symbols
#' @param symbol_ids vector of symbol ids
#' @param account_set account set
#'
#' @note Exactly one of symbol_names or symbol_ids should be provided. Not all data is provided by
#' this function—use .qt_api_symbols_raw to get all the data.
#'
#' @return a dataframe of symbol data.
#' @export
#'
qt_api_symbols <- function(symbol_names = NULL, symbol_ids = NULL, account_set = load_account_set()) {
  qt_api_symbols_raw(symbol_ids = symbol_ids, symbol_names = symbol_names, account_set = account_set) %>%
    httr::content() %>%
    purrr::pluck("symbols") %>%
    purrr::map(~ .x[c("symbol", "symbolId", "description", "prevDayClosePrice", "highPrice52", "lowPrice52")]) %>%
    purrr::map_df(as_tibble) %>%
    colnames_from_camel_case()
}

pluck_and_map <- function(resp, pluck_val) {
  resp %>%
    httr::content() %>%
    purrr::pluck(pluck_val) %>%
    purrr::discard(~ is.null(.x)) %>%
    purrr::map_df(tibble::as_tibble) %>%
    colnames_from_camel_case()
}

add_access_token_to_env <- function(account_set = load_account_set()) {
  Sys.setenv(QUESTRADER_ACCESS_TOKEN = get_stored_token(account_set, ACCESS_TOKEN))
}
