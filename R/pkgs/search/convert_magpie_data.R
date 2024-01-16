
box::use(
  #svMagpie = svMagpie[connect_magpie],
  DBI = DBI[dbGetQuery, dbIsValid],
  tidyr = tidyr[separate_longer_delim],
  dplyr = dplyr[filter, mutate],
  stringr = stringr[str_squish],
  ms_string = . / string_prep
)

print_messages    <- FALSE
search_view       <- "view_daten_reichweite_menge"
list_concatenator <- "|"
concept_separator <- "---"

#' @export
get_valid_conn <- function(conn) {
  conn <- tryCatch({
    if (is.na(conn)) {
      #conn <- svMagpie$connect_magpie()
    }
    return(conn)
  },
  warning = function(cond) {
    if (DBI$dbIsValid(conn)) {
      conn <- conn
    }
    else {
      #conn <- svMagpie$connect_magpie()
    }
    return(conn)
  })
}


#' @export
get_column_vector <- function(table = NA,
                              conn,
                              exclude_cols = c("wert", "reichweite_az")) {
  if (is.na(table)) {
    view <-
      DBI$dbGetQuery(conn, paste0("SELECT * FROM ", search_view, " LIMIT 0;"))
    columns <- names(view[sapply(view, is.character)])
    columns <-
      columns[!grepl("_id", columns)] |> setdiff(exclude_cols)
  }
  else {
    columns <- columns[table %in% columns]

    if (table == "reichweite") {
      columns <- columns[!"klasse" %in% columns & !"typ" %in% columns]
    }
  }
  return(columns)
}

transform_table_column <-
  function(table_column_and_id) {
    table_column_and_id$column <- colnames(table_column_and_id)[2]
    colnames(table_column_and_id) <-
      c("id", "db_content", "column")

    # Split the tags column and create a new row for each tag
    new_data <- table_column_and_id |>
      tidyr$separate_longer_delim(db_content, delim = concept_separator) |>
      tidyr$separate_longer_delim(db_content, delim = list_concatenator) |>
      dplyr$mutate(db_content = stringr::str_squish(db_content)) |>
      dplyr$filter(db_content != "" & !is.na(db_content))

    return(new_data)
  }

#' @export
get_db_data_long <- function(conn = NA) {
  conn <- get_valid_conn(conn)
  columns_to_search_in <-
    get_column_vector(NA, conn = conn)

  long <- data.frame(id = c(),
                     db_content = c(),
                     column = c())

  # get the data from db
  data <-
    DBI::dbGetQuery(conn,
                    paste0(
                      "SELECT daten_id as id, ",
                      paste(columns_to_search_in, collapse = ", "),
                      " FROM ",
                      search_view,
                      ";"
                    ))
  if (print_messages) message("Queried data from DB")

  # search in all string columns
  for (column in columns_to_search_in) {
    print(column)
    long <-
      rbind(long, transform_table_column(data[c("id", column)]))
  }
  return(long)
}

#' @export
get_db_data_unique <- function(data_long) {
  long_unique <-
    data_long[c("db_content")] |>
    unique() |>
    mutate(str_prepped = ms_string$preprocess_str(db_content)) |>
    filter(!is.na(str_prepped) & str_prepped != "") |>
    unique()

  if (print_messages) message(paste0("Found ", nrow(long_unique), " unique entries to search in."))
  return(long_unique)
}
