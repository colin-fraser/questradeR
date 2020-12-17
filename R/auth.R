

# constants -----------------------------------------------------------------------------------

# these are used for the usernames by keyring
REFRESH_TOKEN <- "refresh_token"
ACCESS_TOKEN <- "access_token"
API_SERVER <- "api_server"

# login api urls
LOGIN_URL <- "https://login.questrade.com/oauth2/token"
PRACTICE_LOGIN_URL <- "https://practicelogin.questrade.com/oauth2/token"

# defaults for local storage
DEFAULT_DIR <- "~/.questrader"
DEFAULT_ACCOUNT_SET <- 'questrader'


# functions -----------------------------------------------------------------------------------

set_stored_token <- function(account_set, token_type = c(REFRESH_TOKEN, ACCESS_TOKEN), value = NULL) {
  token_type <- match.arg(token_type)
  if (!is.null(value)) {
    keyring::key_set_with_value(service = account_set$name, username = token_type, password = value)
  } else {
    keyring::key_set(service = account_set$name, username = token_type)
  }
}

get_stored_token <- function(account_set, token_type = c(REFRESH_TOKEN, ACCESS_TOKEN)) {
  token_type <- match.arg(token_type)
  keyring::key_get(service = account_set$name, username = token_type)
}

request_refresh_token <- function(account_set, rt) {
  end_point <- account_set$login_url
  response <- httr::POST(
    end_point,
    query = list(
      grant_type = REFRESH_TOKEN,
      refresh_token = rt
    ),
    accept_json()
  )

  httr::stop_for_status(response, task = "Login")

  response
}

qt_refresh_token <- function(account_set) {

  rt <- if (is.null(refresh_token)) get_stored_refresh_token(account_set) else refresh_token

  resp <- httr::content(request_refresh_token(account_set))

  set_refresh_token(account_set, value = resp$refresh_token)
  message("refresh token set")

  set_access_token(account_set, value = resp$access_token)
  message("access token set")

  set_api_server(account_set, value = resp$api_server)
}

bearer_token <- function(account_set = "questrader") {
  httr::add_headers(Authorization = paste("Bearer", get_access_token(account_set)))
}

login_domain <- function(practice) {
  if (practice) {
    return(PRACTICE_LOGIN_URL)
  } else {
    return(LOGIN_URL)
  }
}


qt_register_new <- function() {
  practice <- .askyesno("Is this a practice account?")
  config <- list()
  config[["practice"]] <- practice



  config
}

qt_dir <- function() {
  # todo: change this so that it supports options and other ways to get this
  fs::path_expand(DEFAULT_DIR)
}

qt_init <- function(directory = qt_dir()) {

  if (fs::dir_exists(directory)) {
    if (!.askyesno(glue::glue("Directory {directory} exists. Re-initialize?"))) {
      stop("Initialization cancelled")
    }
  }

  fs::dir_create(directory)

}

new_account_set <- function(account_set_name, practice) {
  list(
    name = account_set_name,
    practice = practice,
    login_url = login_domain(practice),
    api_server = NULL
  )
}

qt_add_account_set <- function(directory = qt_dir()) {

  if (!fs::dir_exists(directory)) {
    stop("No qt directory found. Run qt_init().")
  }

  name <- .ask("What do you want to call the account set? This is {crayon::underline('not')} your Questrade username. (Leave blank for default.)", DEFAULT_ACCOUNT_SET)
  is_practice <- .askyesno("Is this a practice account?")

  account_set_config_filepath <- fs::path(directory, name, ext = 'yaml')
  if (fs::file_exists(account_set_config_filepath)) {
    overwrite <- .askyesno("Account set config already exists at {path}. Overwrite?")
    if(!overwrite) stop("New account set canceled.")
  }

  cfg <- new_account_set(name, is_practice)

  cfg

}
