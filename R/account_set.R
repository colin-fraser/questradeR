
# Constants -----------------------------------------------------------------------------------

# defaults for local storage
DEFAULT_DIR <- "~/.questrader"
DEFAULT_ACCOUNT_SET <- "questrader"

# option names
DEFAULT_DIR_OPTION <- "default.dir"
DEFAULT_ACCOUNT_SET_OPTION <- "default.account.set"

# Functions -----------------------------------------------------------------------------------


#' Stored account information
#'
#' @return The directory to look in for account sets
#' @export
#'
qt_dir <- function() {
  getOption(DEFAULT_DIR_OPTION, default = fs::path_expand(DEFAULT_DIR))
}

#' Default account set filepath
#'
#' @return The default account set directory
#'
qt_default_account_set <- function() {
  default_account_set <- getOption(DEFAULT_ACCOUNT_SET_OPTION, default = DEFAULT_ACCOUNT_SET)
  fs::path(qt_dir(), default_account_set, ext = "yaml")
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
  message(glue::glue("Directory {directory} created."))
  if (.askyesno("Create an account set?")) {
    qt_add_account_set(directory)
  }
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
    stop(glue::glue("No data stored in {path} for account set '{account_set_name}'"))
  }
  structure(account_set, class = "account_set")
}

#' Interactively add an account set
#'
#' @param directory where to store the directory
#'
#' @return the created account set
#' @export
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

qt_delete_account_set <- function(account_set_name, dir = qt_dir()) {
  acs <- load_account_set(account_set_name, dir)
  filename <- fs::path(dir, account_set_name, ext = "yaml")
  fs::file_delete(filename)
  message(glue::glue("{filename} deleted"))
  keyring::key_delete(acs$name, REFRESH_TOKEN)
  message("refresh token deleted")
  keyring::key_delete(acs$name)
  message("access token deleted")
}
