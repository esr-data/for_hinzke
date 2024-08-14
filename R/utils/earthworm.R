
box::use(
  ../../R/utils/log[write_log],
  shiny[markdown, HTML],
  httr[timeout, GET]
)

#'@noRd
get_cache <- function(){
  dateien <- list.files("md")
  dateien <- dateien[grepl("ew_", dateien)]
  output <-
    lapply(
      dateien,
      \(.){
        markdown(readLines(file.path("md", .)))
      }
    )

  names(output) <- substr(dateien, 4, nchar(dateien) - 3)
  return(output)
}

EARTHWORM_URL   <- "http://srv-data02:3000/artikel/%s/markdown"
USE_CACHE_FIRST <- TRUE
ONLY_USE_CACHE  <- TRUE
CACHE_EW        <- get_cache()

#' @export
read_markdown <- function(slug, to_cache = TRUE){

  if (is.null(slug))     return(HTML(""))
  if (length(slug) != 1) return(HTML(""))
  if (is.na(slug))       return(HTML(""))

  md_text <- NULL
  datei   <- sprintf("md/ew_%s.md", slug)

  if (USE_CACHE_FIRST){
    if (slug %in% names(CACHE_EW)){
      return(CACHE_EW[[slug]])
    }
  }

  if (ONLY_USE_CACHE){
    return(HTML(""))
  }

  if (is.null(md_text)){
    md_text <- "" #md_text <- curl_earthworm(slug)
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

