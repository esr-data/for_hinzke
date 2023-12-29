
#' Missing description
#' @export

load_embedded_quarto <- function(name){
  paste0(
    '<embed src="quarto/',
    name,
    '/index.html" style="width:90%; height: 90vh;">'
  ) |>
    HTML()
}
