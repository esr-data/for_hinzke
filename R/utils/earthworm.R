
box::use(
  shiny,
  httr[timeout, GET]
)

read_markdown <- function(slug){
  if (is.null(slug))     return(shiny::HTML(""))
  if (length(slug) != 1) return(shiny::HTML(""))
  if (is.na(slug))       return(shiny::HTML(""))
  text <-
    suppressWarnings(
      try(
        as.character(
          GET(
            sprintf(
              "http://172.16.0.17:3000/artikel/%s/markdown",
              slug
            ),
            timeout(.5)
          )
        ),
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


markdown_cache <-
  list(
    willkommen        = read_markdown("willkommen"),
    storybeschreibung = read_markdown("storybeschreibung"),
    fdz               = read_markdown("forschungsdatenzentrum-wissenschaftsstatistik"),
    svData            = read_markdown("svData")
  )

read_markdown_cache <- function(slug){
  markdown_cache[[slug]]
}
