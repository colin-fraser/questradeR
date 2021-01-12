# This file contains the functions for authorizing the package.


# constants -----------------------------------------------------------------------------------

# these are used for the usernames by keyring
REFRESH_TOKEN <- "refresh_token"
ACCESS_TOKEN <- "access_token"
API_SERVER <- "api_server"

# login api urls
LOGIN_URL <- "https://login.questrade.com/oauth2/token"
PRACTICE_LOGIN_URL <- "https://practicelogin.questrade.com/oauth2/token"

# Questrade page where user can generate a refresh key manually
REFRESH_KEY_GEN_URL <- "https://apphub.questrade.com/UI/UserApps.aspx"
PRACTICE_REFRESH_KEY_GEN_URL <- "https://practicelogin.questrade.com/APIAccess/UserApps.aspx"


# functions -----------------------------------------------------------------------------------

#' Get and set stored tokens
#'
#'
#' @param account_set an account_set object
#' @param token_type what type of token to store? Either REFRESH_TOKEN or ACCESS_TOKEN.
#' @param value The value to store. If blank, the user will be prompted for a token.
#'
#' @export
#'
set_stored_token <- function(account_set, token_type = c(REFRESH_TOKEN, ACCESS_TOKEN), value = NULL) {
  token_type <- match.arg(token_type)
  if (!is.null(value)) {
    keyring::key_set_with_value(service = account_set$name, username = token_type, password = value)
  } else {
    keyring::key_set(service = account_set$name, username = token_type)
  }
}

#' Set stored token
#' @return get_stored_token returns an account_set object
#' @export
#' @describeIn set_stored_token get stored tokens
get_stored_token <- function(account_set, token_type = c(REFRESH_TOKEN, ACCESS_TOKEN)) {
  token_type <- match.arg(token_type)
  keyring::key_get(service = account_set$name, username = token_type)
}

#' Request refresh token
#'
#' @param account_set account set object
#' @param rt valid request_token
#'
#' @return an httr response object from questrade.com containing refreshed tokens
#' @export
#'
request_refresh_token <- function(account_set, request_token) {
  end_point <- account_set$login_url

  response <- httr::POST(
    end_point,
    query = list(
      grant_type = REFRESH_TOKEN,
      refresh_token = request_token
    )
  )

  stop_for_error(response)

  response
}

#' Manually set the refresh token.
#'
#' Use this to set the refresh token directly. You can get a refresh token from the Questrade
#' API hub page in your Questrade account.
#'
#' @param account_set an account_set object
#' @param value The value of the refresh token. Leave NULL to prompt the user for this.
#'
#' @export
#'
qt_set_refresh_token_manually <- function(account_set = load_account_set(), value = NULL) {
  set_stored_token(account_set, REFRESH_TOKEN, value)
  message("Refresh token set")
  invisible(account_set)
}

#' Refresh tokens
#'
#' Submits a refresh token to Questrade to exchange for an access token, which can be used for
#' performing account operations. These tokens are stored securely in your platform's credential
#' store. This will also update the account_set object with the api server.
#'
#' @param account_set account_set object
#' @param refresh_token the value of a refresh token. Leave NULL to get this from keyring.
#' @param update_saved_account_set should the updated account set be updated? Almost always should be true.
#'
#' @return an updated account_set object
#' @export
#'
qt_refresh_token <- function(account_set = load_account_set(), refresh_token = NULL,
                             update_saved_account_set = TRUE) {
  rt <- if (is.null(refresh_token)) get_stored_token(account_set, REFRESH_TOKEN) else refresh_token

  resp <- tryCatch(httr::content(request_refresh_token(account_set, rt)),
    error = function(e) {
      new_rt <- .askyesno("Failed to refresh manual access token. You may need to get a new one manually. Do you want to do this now?")
      if (new_rt) {
        if (account_set$practice) {
          key_url <- PRACTICE_REFRESH_KEY_GEN_URL
        } else {
          key_url <- REFRESH_KEY_GEN_URL
        }
        .ask(glue::glue("Visit {key_url} to get a refresh token and enter it in the next prompt. (Press Enter)."))
        qt_set_refresh_token_manually(account_set)
        return(httr::content(request_refresh_token(account_set, get_stored_token(account_set, REFRESH_TOKEN))))
      }
      stop("Failed to exchange refresh token")
    }
  )

  if (class(resp) != "list") {
    stop()
  }

  set_stored_token(account_set, REFRESH_TOKEN, value = resp$refresh_token)
  message("refresh token saved with keyring")

  set_stored_token(account_set, ACCESS_TOKEN, value = resp$access_token)
  message("access token set saved with keyring")

  account_set$api_server <- resp$api_server
  if (update_saved_account_set) {
    account_set_config_filepath <- fs::path(qt_dir(), account_set$name, ext = "yaml")
    yaml::write_yaml(account_set, account_set_config_filepath)
    message("API server updated")
  }

  invisible(account_set)
}

#' Create the authorization header
#'
#' @param account_set account set
#'
#' @return an httr header object with the authorization token
#'
bearer_token <- function(account_set = load_account_set()) {
  httr::add_headers(Authorization = paste("Bearer", get_stored_token(account_set, ACCESS_TOKEN)))
}

login_url <- function(practice) {
  if (practice) {
    return(PRACTICE_LOGIN_URL)
  } else {
    return(LOGIN_URL)
  }
}
