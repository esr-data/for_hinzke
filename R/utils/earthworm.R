
box::use(
  ../../R/utils/log[write_log],
  shiny[markdown, HTML],
  httr[timeout, GET]
)

EARTHWORM_URL <- "http://srv-data02:3000/artikel/%s/markdown"

#' @export
read_markdown <- function(slug, use_cache_first = FALSE, to_cache = TRUE){

  if (is.null(slug))     return(HTML(""))
  if (length(slug) != 1) return(HTML(""))
  if (is.na(slug))       return(HTML(""))

  md_text <- NULL
  datei   <- sprintf("md/ew_%s.md", slug)

  if (use_cache_first){
    if (file.exists(datei)){
      md_text <- suppressWarnings(try(readLines(datei), silent = TRUE))
    }
  }

  if (is.null(md_text)){
    md_text <- curl_earthworm(slug)
  }

  if (is.null(md_text) & !use_cache_first){

    md_text <- suppressWarnings(try(readLines(datei), silent = TRUE))

    if (class(md_text) %in% "try-error"){
      write_log(
        sprintf("Warnung: Markdown konnte nicht vom lokalen Verzeichnis geladen werden (slug = '%s')!", slug)
      )
      return(HTML(""))
    }

  } else {

    if (to_cache){
      writeLines(md_text, datei)

      if (file.exists(datei)){
        write_log("Markdown vom Earthworm gespeichert (slug = '%s')")
      } else {
        write_log("Markdown vom Earthworm konnte nicht gespeichert (slug = '%s')")
      }
    }

  }

  return(markdown(md_text))
}

#'@noRd
curl_earthworm <- function(slug, timeout = .5){

  url <- sprintf(EARTHWORM_URL, slug)
  md_text <-
    suppressWarnings(
      try(
        as.character(GET(url, timeout(timeout))),
        silent = TRUE
      )
    )

  if (class(md_text) %in% "try-error"){
    write_log(
      sprintf("Warnung: Markdown konnte nicht vom Earthworm geladen werden (slug = '%s')!", slug),
      warn = TRUE
    )
    return(NULL)
  }

  return(md_text)
}
