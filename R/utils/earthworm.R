
box::use(
  shiny
)

read_markdown <- function(slug){
  if (is.null(slug))     return(shiny::HTML(""))
  if (length(slug) != 1) return(shiny::HTML(""))
  if (is.na(slug))       return(shiny::HTML(""))
  text <-
    suppressWarnings(
      try(
        readLines(sprintf("http://172.16.0.17:3000/artikel/%s/markdown", slug)),
        silent = TRUE
      )
    )
  if (class(text) == "try-error"){
    text <- try(readLines(sprintf("md/%s.md", slug)), silent = TRUE)
    if (class(text) == "try-error"){
      warning(
        sprintf("Warnung: Markdown nicht gefunden (%s)!", slug)
      )
      return(shiny::HTML(""))
    }
    warning(
      sprintf("Warnung: Lokales Markdown verwendet, weil Earthworm-Server nicht erreicht wurde (slug = '%s')!", slug)
    )
  }
  return(shiny::markdown(text))
}
