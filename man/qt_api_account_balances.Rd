% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{qt_api_account_balances}
\alias{qt_api_account_balances}
\title{Get account balances}
\usage{
qt_api_account_balances(account_id, account_set = load_account_set())
}
\arguments{
\item{account_id}{account ID.}

\item{account_set}{account set}
}
\value{
a tibble with a list of account balances
}
\description{
Get account balances
}
