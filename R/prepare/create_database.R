

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
  if ("id" %in% names(x)) x$id <- as.integer(x$id)
  DBI::dbWriteTable(con_duck, i, x)
  if ("zeit_start" %in% names(x)) x$zeit_start <- as.character(x$zeit_start)
  if ("zeit_ende" %in% names(x))  x$zeit_start <- as.character(x$zeit_ende)
  DBI::dbWriteTable(con_sqlite, i, x)
  rm(x)
}

DBI::dbDisconnect(con)
DBI::dbDisconnect(con_sqlite)
DBI::dbDisconnect(con_duck, shutdown = TRUE)
rm(tabs, i, con_sqlite, con, con_duck)

