.ask <- function(prompt, default = NULL) {
  prompt <- glue::glue(prompt)
  cat(prompt)
  input <- readline(">> ")
  if (!is.null(default)) {
    if (input == "") {
      input <- default
    }
  }
  input
}

.askyesno <- function(prompt, yes = "y", no = "n", append_opts = TRUE,
                      allow_na = FALSE) {
  if (append_opts) {
    prompt <- glue::glue("{prompt} ({crayon::yellow(yes)}/{crayon::yellow(no)})")
  }
  input <- .ask(prompt)
  while (!(input %in% c(yes, no))) {
    message(glue::glue("Response must be '{yes}' for Yes or '{no}' for No."))
    input <- .ask(prompt)
  }
  if (input == yes) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Format time for questrade
#'
#' @param x
#'
#' @return
#'
#' @importFrom stringr str_replace
format_time <- function(x) {
  str_replace(format(x, "%Y-%m-%dT%H:%M:%S%z"), "(?<=-\\d\\d)(?=00)", ":")
}

from_camel_case <- function(x) {
  x %>%
    stringr::str_split("(?<=[[:lower:]])(?=[[:upper:]])") %>%
    purrr::map(stringr::str_to_lower) %>%
    purrr::map_chr(stringr::str_c, collapse = "_")
}

colnames_from_camel_case <- function(x) {
  colnames(x) <- from_camel_case(colnames(x))
  x
}


stop_for_error <- function(resp, task = "") {
  if (httr::http_error(resp)) {
    stop(glue::glue("HTTP error. Response content:\n\n{httr::content(resp)}"))
  }
  resp
}
