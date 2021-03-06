% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extra.R
\name{plot_close}
\alias{plot_close}
\title{Plot close prices from candle df}
\usage{
plot_close(candle_df, yaxis = c("price", "return"), facet = FALSE)
}
\arguments{
\item{candle_df}{data frame returned from candles()}

\item{yaxis}{'price' for raw prices, 'return' for returns since the start}

\item{facet}{facet each symbol?}
}
\value{
a ggplot
}
\description{
Plot close prices from candle df
}
