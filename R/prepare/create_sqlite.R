con <- svMagpie::connectDB()
tabs <-
  c(
    DBI::dbListTables(con),
    DBI::dbGetQuery(con, "SELECT matviewname AS view_name FROM pg_matviews;")[,1]
  )
con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), "data/magpie.sqlite")
con_duck   <- DBI::dbConnect(duckdb::duckdb(), "data/magpie.db")

for (i in tabs){
  print(i)
  x <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", i))
  if ("zeit_start" %in% names(x)) x$jahr <- as.numeric(substr(x$zeit_start, 1, 4))
  DBI::dbWriteTable(con_sqlite, i, x)
  DBI::dbWriteTable(con_duck, i, x)
  rm(x)
}

DBI::dbDisconnect(con)
DBI::dbDisconnect(con_sqlite)
DBI::dbDisconnect(con_duck, shutdown = TRUE)
rm(tabs, i, con_sqlite, con, con_duck)

