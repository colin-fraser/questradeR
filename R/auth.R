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

# defaults for local storage
DEFAULT_DIR <- "~/.questrader"
DEFAULT_ACCOUNT_SET <- "questrader"


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
request_refresh_token <- function(account_set, rt) {
  end_point <- account_set$login_url
  response <- httr::POST(
    end_point,
    query = list(
      grant_type = REFRESH_TOKEN,
      refresh_token = rt
    ),
    httr::accept_json()
  )

  httr::stop_for_status(response, task = "Login")

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

  resp <- httr::content(request_refresh_token(account_set, rt))

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


#' Stored account information
#'
#' @return The directory to look in for account sets
#' @export
#'
qt_dir <- function() {
  # TODO: change this so that it supports options and other ways to get this
  fs::path_expand(DEFAULT_DIR)
}

#' Default account set filepath
#'
#' @return The default account set directory
#'
qt_default_account_set <- function() {
  fs::path(qt_dir(), DEFAULT_ACCOUNT_SET, ext = "yaml")
}

#' Create the account_set directory
#'
#' @param directory where to store account_set information
#'
#' @return The path to the created object (invisibly).
#' @export
#'
qt_init <- function(directory = qt_dir()) {
  if (fs::dir_exists(directory)) {
    if (!.askyesno(glue::glue("Directory {directory} exists. Re-initialize?"))) {
      stop("Initialization cancelled")
    }
  }

  fs::dir_create(directory)
}

#' Create an account set
#'
#' @param account_set_name the name of the account set
#' @param practice boolean indicating whether the account is a practice account
#'
#' @return an object of class account_set
#' @export
#'
new_account_set <- function(account_set_name, practice) {
  structure(list(
    name = account_set_name,
    practice = practice,
    login_url = login_url(practice),
    api_server = NULL
  ), class = "account_set")
}

#' Load an account set
#'
#' @param account_set_name the name of the stored account_set
#' @param path where to look for the account_set
#'
#' @return the account_set object
#' @export
#'
load_account_set <- function(account_set_name = "questrader", path = qt_dir()) {
  filepath <- fs::path(path, account_set_name, ext = "yaml")
  if (fs::file_exists(filepath)) {
    account_set <- yaml::read_yaml(filepath)
  } else {
    stop(glue::glue("No data stored in {path} for account set '{account_set}'"))
  }
  structure(account_set, class = "account_set")
}

#' Interactively add an account set
#'
#' @param directory where to store the directory
#'
#' @return the created account set
#'
qt_add_account_set <- function(directory = qt_dir()) {
  if (!fs::dir_exists(directory)) {
    stop("No qt directory found. Run qt_init().")
  }

  name <- .ask("What do you want to call the account set? This is {crayon::underline('not')} your Questrade username. (Leave blank for default.)", DEFAULT_ACCOUNT_SET)

  account_set_config_filepath <- fs::path(directory, name, ext = "yaml")
  if (fs::file_exists(account_set_config_filepath)) {
    overwrite <- .askyesno(glue::glue("Account set config already exists at {account_set_config_filepath}. Overwrite?"))
    if (!overwrite) stop("New account set canceled.")
  }

  is_practice <- .askyesno("Is this a practice account?")

  cfg <- new_account_set(name, is_practice)
  if (is_practice) {
    key_url <- PRACTICE_REFRESH_KEY_GEN_URL
  } else {
    key_url <- REFRESH_KEY_GEN_URL
  }

  .ask(glue::glue("Visit {key_url} to get a refresh token and enter it in the next prompt. (Press Enter)."))
  cfg <- qt_set_refresh_token_manually(cfg) %>%
    qt_refresh_token()

  yaml::write_yaml(cfg, account_set_config_filepath)
  message("Account set saved")

  invisible(cfg)
}

#' List stored account sets
#'
#' @param directory where to look for account sets
#'
#' @return character vector of account set names
#' @export
#'
qt_account_set_list <- function(directory = qt_dir()) {
  fs::dir_ls(directory) %>%
    fs::path_file() %>%
    fs::path_ext_remove()
}
