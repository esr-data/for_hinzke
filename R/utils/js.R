
box::use(
  shiny[HTML, tags]
)

get_js <- function(x, as_script = TRUE){
  x <-
    "js/%s.js" |>
    sprintf(x) |>
    readLines() |>
    paste(collapse = " ") |>
    HTML()
  if (as_script){
    x <- tags$script(x)
  }
  return(x)
}
