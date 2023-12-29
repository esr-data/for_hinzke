box::use(
  shiny.router[get_query_param]
)

#' Missing description
#' @export

get_hf_param <- function(url = NULL){

  if (is.null(url)){

    param <- get_query_param()
    if (is.null(param)){
      return(0)
    }

    hf <- param$hf
    if (is.null(hf)){
      return(0)
    }

    if (all(hf %in% 0:2)){
      return(hf[1])
    }

  } else {

    url <- (strsplit(url, "?", fixed = TRUE)[[1]])
    if (length(url) < 2){
      return(0)
    }
    url <- strsplit(url[2], "&")[[1]]
    if (any(substr(url, 1, 3) == "hf=")){
      url <- url[match("hf=", substr(url, 1, 3))]
      url <- substr(url, 4, 4)
      if (url %in% 1:2){
        return(url)
      }
    }
  }

  return(0)
}


#' Missing description
#' @export

add_param_in_url <- function(current_url, current_page, parameter, value, old_value){

  current_page <- paste0("#!/", current_page)
  new_url <- current_url
  if (grepl(current_page, current_url, fixed = TRUE)){

    if (value == ""){
      new_url <-
        gsub(
          paste0(paste0("?", parameter, "="), old_value),
          value,
          new_url,
          fixed = TRUE
        )
      new_url <-
        gsub(
          paste0(paste0("&", parameter, "="), old_value),
          value,
          new_url,
          fixed = TRUE
        )

    } else if (current_url == current_page){
      new_url <- paste0(current_url, paste0("?", parameter, "="), value)

    } else if (grepl(parameter, current_url, fixed = TRUE)){
      new_url <-
        gsub(
          paste0(paste0(parameter, "="), old_value),
          paste0(paste0(parameter, "="), value),
          current_url,
          fixed = TRUE
        )

    } else if (grepl(paste0(current_page, "?"), current_url, fixed = TRUE)){
      new_url <- paste0(current_url, paste0("&", parameter, "="), value)

    }
  }
  return(new_url)
}

#' Missing description
#' @export

recode_parameter <- function(x, is_vector = FALSE, is_integer = TRUE, value_set = NULL){
  if (is.null(x)) return("")
  if (any(is.na(x))) return("")
  if (is_vector) x <- strsplit(x, ",")[[1]]
  if (is_integer) x <- suppressWarnings(as.integer(x))
  if (!is.null(value_set)) x <- x[x %in% value_set]
  if (length(x) < 1) return("")
  if (any(is.na(x))) return("")
  return(sort(x))
}
