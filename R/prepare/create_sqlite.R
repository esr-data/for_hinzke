con <- svMagpie::connect_magpie()
tabs <- DBI::dbListTables(con)
con2 <- DBI::dbConnect(RSQLite::SQLite(), "data/magpie.sqlite")

for (i in tabs){
  print(i)
  x <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", i))
  if ("zeit_start" %in% names(x)) x$jahr <- as.numeric(substr(x$zeit_start, 1, 4))
  DBI::dbWriteTable(con2, i, x)
  rm(x)
}

DBI::dbDisconnect(con)
DBI::dbDisconnect(con2)
rm(tabs, i, con2, con)
