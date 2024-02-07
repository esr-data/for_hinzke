
box::use(
  ../../R/pkgs/search/convert_magpie_data[get_db_data_long],
  data.table[fwrite],
  DBI,
  RSQLite
)

con_duck    <- DBI::dbConnect(duckdb::duckdb(), "data/magpie.db", read_only = TRUE)
daten <- get_db_data_long(conn = con_duck)
DBI::dbDisconnect(con_duck, shutdown = TRUE)

search_columns <- data.frame(bez = unique(daten$column))
search_columns$label <- gsub("_list", "", search_columns$bez)
search_columns$label <- gsub("_einheit", "einheit", search_columns$label)
search_columns$label <- gsub("_beschr", "", search_columns$label)
search_columns$label <- gsub("reichweite_", "reichweiten", search_columns$label)
search_columns$label <- gsub("menge_zugeh_reichweiten", "menge", search_columns$label)
search_columns$label <- paste0(toupper(substr(search_columns$label, 1, 1)), substr(search_columns$label, 2, nchar(search_columns$label)))

# con_cache <- DBI$dbConnect(RSQLite$SQLite(), "data/search_cache.sqlite")
con_cache <- DBI::dbConnect(duckdb::duckdb(), "data/magpie.db")
DBI$dbWriteTable(con_cache, "search_cache", daten)
DBI$dbWriteTable(con_cache, "search_cache_column", search_columns)
data.table::fwrite(daten, "data/search_cache.csv")
DBI$dbDisconnect(con_cache, shutdown = TRUE)

