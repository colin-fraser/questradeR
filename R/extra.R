positions_incl_cash <- function(position_df, balance_df) {
  TRUE
}

candles <- function(symbol_ids, start_time = lubridate::today() - 365, end_time = lubridate::today(), interval =
                      c(
                        "OneDay",
                        "OneHour",
                        "OneMinute",
                        "TwoMinutes",
                        "ThreeMinutes",
                        "FourMinutes",
                        "FiveMinutes",
                        "TenMinutese",
                        "FifteenMinutes.",
                        "TwentyMinutes",
                        "HalfHour",
                        "TwoHours",
                        "FourHours",
                        "OneWeek",
                        "OneMonth",
                        "OneYear"
                      ), account_set = load_account_set()) {
  interval <- match.arg(interval)
  dfs <- list()
  for (symbol in symbol_ids) {
    message(glue::glue("Getting candles for symbol {symbol}"))
    dfs[[symbol]] <- qt_api_candles(symbol, start_time, end_time, interval, account_set) %>%
      mutate(symbol_id = symbol)
  }
  bind_rows(dfs)
}

#' Plot close prices from candle df
#'
#' @param candle_df data frame returned from candles()
#' @param yaxis 'price' for raw prices, 'return' for returns since the start
#' @param facet facet each symbol?
#'
#' @return a ggplot
#' @export
#' @import ggplot2 dplyr
#'
plot_close <- function(candle_df, yaxis = c("price", "return"), facet = FALSE) {
  yaxis <- match.arg(yaxis)
  if (yaxis == "return") {
    candle_df <- candle_df %>%
      group_by(symbol) %>%
      mutate(close = close / first(close) - 1)
  }

  if (facet) {
    p <- candle_df %>%
      ggplot(aes(x = end, y = close)) +
      geom_line() +
      facet_wrap(~symbol, scales = "free_y")
  }

  else {
    p <- candle_df %>%
      ggplot(aes(x = end, y = close, color = symbol)) +
      geom_line()
  }

  if (yaxis == "return") {
    p <- p +
      scale_y_continuous(labels = scales::label_percent())
  }
}
