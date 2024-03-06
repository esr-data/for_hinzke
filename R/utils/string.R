
box::use(
  stringr[str_replace_all, str_squish]
)

preprocess_str <- function(str) {
  str <- tolower(str) |>
    str_replace_all("[:]+", " ") |>
    str_replace_all("[^ 0-9a-zäöüß&|]", "") |>
    str_replace_all("ä", "ae") |>
    str_replace_all("ö", "oe") |>
    str_replace_all("ü", "ue") |>
    str_replace_all("ß", "ss") |>
    str_squish()
  return(str)
}
