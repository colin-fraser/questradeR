.ask <- function(prompt, default = NULL) {
  prompt <- glue::glue(prompt)
  cat(prompt)
  input <- readline('>> ')
  if (!is.null(default)) {
    if (input == "") {
      input <- default
    }
  }
  input
}

.askyesno <- function(prompt, yes = 'y', no = 'n', append_opts = TRUE,
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
