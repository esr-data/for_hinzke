
box::use(
  stringr = stringr[str_replace_all, str_squish]
)

#' @export
preprocess_str <- function(str) {
  str <- tolower(str) |>
    stringr$str_replace_all("[^ 0-9a-zäöüß&|]", "") |>
    stringr$str_replace_all("ä", "ae") |>
    stringr$str_replace_all("ö", "oe") |>
    stringr$str_replace_all("ü", "ue") |>
    stringr$str_replace_all("ß", "ss") |>
    stringr$str_squish()
  return(str)
}

#' @export
extract_operators <- function(search_term) {
  operators <- unlist(stringr$str_extract_all(search_term, "[\\|&]"))
  operators <- c("", operators)
  return (operators)
}

#' @export
raise_invalid_connection_error <- function() {
  stop("You cannot use svMagpie, therefore you need to pass a valid connection object!")
}