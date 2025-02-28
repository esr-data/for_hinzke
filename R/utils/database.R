#' Necessary Packages/Functions
#'

box::use(
  ../../R/pkgs/search/search[search_tables, get_data_to_search],
  ../../R/utils/log[write_log],
  ../../R/pkgs/svNum/numeric[convert_zeit],
  shiny[HTML, div, icon, p],
  DBI[dbGetQuery, dbConnect, dbDisconnect],
  RSQLite[SQLite],
  data.table[fread],
  duckdb[duckdb],
  dplyr[tbl]
)

con <- dbConnect(duckdb(), "data/magpie.db", read_only = TRUE)
data_to_search <- get_data_to_search(con = con)

#' Missing description
#' @export

get_sql <- function(x, query_sql = FALSE){

  sql <-
    paste0("SQL/", x, ".sql") |>
    readLines() |>
    paste(collapse = " ")

  if (query_sql){
    sql <- get_query(sql)
  }

  return(sql)
}

#' Missing description
#' @export

get_query <- function(x){
  dbGetQuery(conn = con, x)
}

#' Missing description
#' @export

get_search_data <- function(){
  data_to_search
}

#' Missing description
#' @export

load_table_by_variable <- function(variable){

  daten <-
    get_query(
      sprintf(
        "SELECT daten.id, variable.beschr as variable, zeit_start, zeit_ende, zeit_einheit.beschr as zeit_einheit ,
                wert, wert_einheit.beschr as einheit
         FROM daten
         LEFT JOIN wert_einheit ON daten.wert_einheit_id = wert_einheit.id
         LEFT JOIN zeit_einheit ON daten.zeit_einheit_id = zeit_einheit.id
         LEFT JOIN variable     ON daten.variable_id = variable.id
         WHERE variable_id = %s",
        variable
      )
    )

  reichweite <-
    "SELECT reichweite.id as id, reichweite.beschr as reichweite, rtyp.beschr as reichweite_typ, rklasse.beschr as reichweite_klasse
     FROM reichweite
     LEFT JOIN reichweite_typ rtyp ON reichweite.reichweite_typ_id = rtyp.id
     LEFT JOIN reichweite_klasse rklasse ON rtyp.reichweite_klasse_id = rklasse.id
     WHERE reichweite.id IN (SELECT reichweite_id FROM daten_reichweite WHERE daten_id IN (SELECT id FROM daten WHERE variable_id = %s))" |>
    sprintf(variable) |>
    get_query()

  daten_reichweite <-
    get_query(
      sprintf(
        "SELECT daten_id, reichweite_id
         FROM daten_reichweite
         WHERE daten_id IN (SELECT id FROM daten WHERE variable_id = %s)",
        variable
      )
    )

  reichweite$gruppe <-
    ifelse(
      reichweite$reichweite_klasse == "Räumliche Gebiete",
      reichweite$reichweite_klasse,
      reichweite$reichweite_typ
    )

  daten[,unique(reichweite$gruppe)] <- NA

  for (i in 1:nrow(daten)){
    x <- daten_reichweite[daten_reichweite$daten_id == daten$id[i],]
    for (j in 1:nrow(x)){
      k <- reichweite$id == x$reichweite_id[j]
      l <- c(daten[i, reichweite$gruppe[k]], reichweite$reichweite[k])
      l <- l[!is.na(l)]
      daten[i, reichweite$gruppe[k]] <- paste(l, collapse = ", ")
    }
  }

  for (i in unique(reichweite$gruppe)){
    daten[,i] <- ifelse(is.na(daten[,i]), "Insgesamt", daten[,i])
  }

  daten$Zeit <- convert_zeit(daten)

  return(list(daten = daten, gruppe = unique(reichweite$gruppe)))
}

#' Missing description
#' @export

search_database <- function(search_term, table = NA){
  results <-
    search_tables(
      search_term,
      min_ranking_weight     = 0.53,
      data_to_search_through = data_to_search,
      beautify_results       = FALSE,
      table                  = table,
      print_messages         = FALSE
    )

  return(results)
}

#' Missing description
#' @export

get_cache_labels <- function(){
  dbGetQuery(con, "SELECT * FROM search_cache_column")
}

#' Missing description
#' @export

disconnect_db <- function(){
  write_log("Verbindung zur Datenbank geschlossen!")
  dbDisconnect(con, shutdown = TRUE)
  return(invisible(NULL))
}

# provisorisch tbl für datalab-Daten
#' Missing description
#' @export

capture_tbl <- function(var){
  tbl(src = con, from = var)
}
