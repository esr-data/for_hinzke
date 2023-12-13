

load_embedded_quarto <- function(name){
  paste0(
    '<embed src="quarto/',
    name,
    '/index.html" style="width:90%; height: 90vh;">'
  ) |>
    HTML()
}

add_param_in_url <- function(current_url, current_page, parameter, value, old_value){

  # LÖSCHEN
  # current_url <<- current_url
  # current_page <<- current_page
  # parameter <<- parameter
  # value  <<- value
  # old_value  <<- old_value
  #

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

get_sql <- function(x, query_sql = FALSE){

  sql <-
    paste0("SQL/", x, ".sql") |>
    readLines() |>
    paste(collapse = " ")

  if (query_sql){
    sql <- DBI::dbGetQuery(con, sql)
  }

  return(sql)
}
