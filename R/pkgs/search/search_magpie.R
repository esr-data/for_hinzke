
preprocess_str <- function(str) {
  str <- tolower(str) %>%
    str_replace_all("[^ 0-9a-zäöüß&|]", "") %>%
    str_replace_all("ä", "ae") %>%
    str_replace_all("ö", "oe") %>%
    str_replace_all("ü", "ue") %>%
    str_replace_all("ß", "ss")
  return(str)
}

get_valid_conn <- function(conn) {
  
  conn <- tryCatch(
    {
      if (is.na(conn)) {
        conn <- svMagpie::connect_magpie()
      }
      return(conn)
    },
    warning=function(cond) {
      if (DBI::dbIsValid(conn)){
        conn <- conn
      }
      else {
        conn <- svMagpie::connect_magpie()
      }
      return(conn)
    })
}

get_table_vector <- function(table = NA,
                             conn,
                             relevant_tables = c(
                               "quelle",
                               "reichweite",
                               "reichweite_klasse",
                               "reichweite_typ",
                               "tag",
                               "variable",
                               "wert_einheit"
                             )) {
  if (is.na(table)) {
    # only use tables, not views
    tables <- DBI::dbListTables(conn)
    tables <- tables[grepl("view_", tables) == FALSE]
  }
  else {
    tables <- c(table)
  }
  
  tables <- tables[tables %in% relevant_tables]
  
  if (length(tables) == 0) {
    stop(paste0("Tables must be from ", paste(relevant_tables, collapse = ", ")))
  }
  
  return(tables)
}

get_logical_match_result <- function(str_a, str_b, max_dist) {
  contains_match <- agrepl(
    str_a,
    str_b,
    max.distance = max_dist,
    costs = list(
      "insertions" = 1,
      "deletions" = 1,
      "substitutions" = 1
    ),
    ignore.case = TRUE,
    fixed = TRUE
  )
  return(contains_match)
}

get_numeric_adist <- function(str_a, str_b, partial = TRUE) {
  adists <- as.numeric(adist(
    str_a,
    str_b,
    fixed = TRUE,
    partial = partial,
    ignore.case = TRUE
  ))
  return (adists)
}

get_matches_from_table_column <- function(table_column,
                                          column_name,
                                          search_str_prepped,
                                          max_distance) {
  table_column[is.na(table_column[[column_name]]), c(column_name)] <-
    ""
  
  # preprocess column (will not be saved after function execution)
  table_column$str_prepped <-
    preprocess_str(table_column[[column_name]])
  
  table_column <- table_column[table_column[[column_name]] != "",]
  
  match_1 <- get_logical_match_result(str_a = search_str_prepped,
                           str_b = table_column$str_prepped,
                           max_dist = max_distance)
  match_2 <- unlist(
    lapply(
      table_column$str_prepped,
      FUN = get_logical_match_result,
      str_b = search_str_prepped,
      max_dist = max_distance
    )
  )
  
  table_column <- table_column[Reduce(`|`, list(match_1, match_2)), ]
  
  # get partial overlap
  table_column$ed_part_1 <-
    get_numeric_adist(str_a = search_str_prepped,
                      str_b = table_column$str_prepped)
  
  table_column$ed_part_2 <-
    get_numeric_adist(str_a = table_column$str_prepped,
                      str_b = search_str_prepped)
  
  # add minimum of both submatches
  table_column$ed_part <-
    pmin(table_column$ed_part_1, table_column$ed_part_2)
  table_column[is.na(table_column$ed_part), c("ed_part")] <- Inf
  
  # if there are matches, append each match to the result list
  matches <-
    table_column[table_column$ed_part <= max_distance &
                   !is.na(table_column$str_prepped) &
                   table_column$str_prepped != "",]
  
  # find overall (not partial) edit distance of matches
  matches$ed_abs <- get_numeric_adist(str_a = search_str_prepped,
                                      str_b = matches$str_prepped,
                                      partial = FALSE)
 
  return(matches)
}

adapt_max_distance_to_search_str_len <- function(max_distance, search_str_prepped) {
  if (nchar(search_str_prepped) <= max_distance) {
    max_distance <- nchar(search_str_prepped)
    message(
      paste0(
        "max_distance was reduced because search string ",
        search_str_prepped,
        " was too short"
      )
    )
  }
  return(max_distance)
}



search_magpie <- function(search_str,
                          conn = NA,
                          table = NA,
                          max_distance = 2) {
  # parse arguments
  search_str_prepped <- preprocess_str(search_str)
  
  max_distance <-
    adapt_max_distance_to_search_str_len(max_distance, search_str_prepped)
  
  conn <- get_valid_conn(conn)
  
  tables <- get_table_vector(table, conn = conn)
  
  # create result data frame
  result_vars <- c(
    "id",
    "db_content",
    "str_prepped",
    "ed_part_1",
    "ed_part_2",
    "ed_part",
    "ed_abs",
    "table",
    "column",
    "search_str",
    "search_str_prepped"
  )
  results <-
    as.data.frame(matrix(ncol = length(result_vars), nrow = 0))
  names(results) <- result_vars
  
  for (table in tables) {
    # get the data from db
    data <-
      DBI::dbGetQuery(conn, paste0("SELECT * FROM home.", table))
    # search in all string columns
    for (column in names(data)[sapply(data, is.character)]) {
      tmp <- data[c("id", column)]
      
      matches <-
        get_matches_from_table_column(
          table_column = tmp,
          column_name = column,
          search_str_prepped = search_str_prepped,
          max_distance = max_distance
        )
      
      # update result df if necessary
      if (nrow(matches) > 0) {
        names(matches)[names(matches) == column] <- "db_content"
        matches$table <- table
        matches$column <- column
        matches$search_str <- search_str
        matches$search_str_prepped <- search_str_prepped
        matches$id <- as.integer(matches$id)
        results <- rbind(results, matches)
      }
    }
  }
  
  results <- dplyr::arrange(results, ed_part, ed_abs)
  
  return(results)
}