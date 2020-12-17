end_points <- function(name) {
  out <- structure(list(scheme = "https", hostname = NULL, port = NULL, path = NULL,
              query = NULL, params = NULL, fragment = NULL, username = NULL,
              password = NULL), class = "url")
  if (name == "login") {
    if (practice) {
      out$hostname <- "practicelogin.questrade.com"
    } else {
      out$hostname <- "login.questrade.com"
    }
    out$path <- "oauth2/token"
  } else {
    out$hostname <- urltools::domain(get_api_server(service = service))
    out$path <- switch(name,
      "accounts" = "v1/accounts",
      "time" = "v1/time"
    )
  }
  httr::build_url(out)
}

# qt_get <- function(name, service = 'questrader') {
#   end_point <-
# }


