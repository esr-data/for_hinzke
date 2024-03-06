
con <- svMagpie::connectDB()
tabs <-
  c(
    DBI::dbListTables(con),
    DBI::dbGetQuery(con, "SELECT matviewname AS view_name FROM pg_matviews;")[,1]
  )

con_duck   <- DBI::dbConnect(duckdb::duckdb(), "data/magpie.db")

for (i in tabs){
  print(i)
  x <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", i))
  if ("id" %in% names(x)) x$id <- as.integer(x$id)
  for (j in grep("_id", names(x), value = TRUE)){
    if (class(x[,j]) == "integer64"){
      test_x <- x[,j]
      x[,j] <- as.integer(x[,j])
      if (any(is.na(x[,j]))) message(paste(i, "|", j))
      stopifnot("neue NAs" = all(is.na(x[,j]) == is.na(test_x)))
      rm(test_x)
    }
  }
  rm(j)
  DBI::dbWriteTable(con_duck, i, x)
  rm(x)
}

daten_link <- dbGetQuery(con, paste(readLines("SQL/daten_link.sql"), collapse = " "))
daten_link$daten_id <- as.integer(daten_link$daten_id)
DBI::dbWriteTable(con_duck, "view_daten_link", daten_link)

#---
create_daten_struktur <- function(){
  daten_link <-
    "SELECT daten_id, reihe_id, tabelle_id, tabelle_id = (SELECT id FROM tabelle WHERE bez = 'reichweite') as reichweite
   FROM view_daten_link
   WHERE
     ebene = 0 AND
     herkunft_tabelle_id IS NULL AND
     tabelle_id IN (SELECT id FROM tabelle WHERE NOT bez IN ('tag', 'reichweite_typ', 'reichweite_klasse', 'quelle'))" |>
    DBI::dbGetQuery(conn = con_duck)

  agg_reichweite <- aggregate(reichweite ~ daten_id, daten_link, sum)

  daten_link$paste_id <- paste(daten_link$tabelle_id, daten_link$reihe_id, sep = "-")
  agg_paste_id <- aggregate(paste_id ~ daten_id, daten_link, \(.) paste(sort(.), collapse = "_"))
  paste_id <- data.frame(string = unique(agg_paste_id$paste_id))
  paste_id$gruppen_id <- 1:nrow(paste_id)
  agg_paste_id$gruppen_id <- paste_id$gruppen_id[match(agg_paste_id$paste_id, paste_id$string)]

  daten <-
    "SELECT id, zeit_ende FROM daten" |>
    DBI::dbGetQuery(conn = con_duck)
  daten$tiefe      <- agg_reichweite$reichweite[match(daten$id, agg_reichweite$daten_id)]
  daten$gruppen_id <- agg_paste_id$gruppen_id[match(daten$id, agg_paste_id$daten_id)]


  agg_paste_id <- aggregate(paste_id ~ daten_id, daten_link[!daten_link$reichweite,], \(.) paste(sort(.), collapse = "_"))
  paste_id <- data.frame(string = unique(agg_paste_id$paste_id))
  paste_id$gruppen_id <- 1:nrow(paste_id)
  agg_paste_id$gruppen_id <- paste_id$gruppen_id[match(agg_paste_id$paste_id, paste_id$string)]

  daten$geschwister_id <- agg_paste_id$gruppen_id[match(daten$id, agg_paste_id$daten_id)]
  names(daten)[1] <- "daten_id"

  daten <- daten[order(daten$gruppen_id, daten$zeit_ende, daten$tiefe * -1, decreasing = TRUE),]
  rownames(daten) <- 1:nrow(daten)
  daten$rang <- 1:nrow(daten)
  daten
}

daten_struktur <- create_daten_struktur()
daten_struktur$daten_id <- as.integer(daten_struktur$daten_id)
DBI::dbWriteTable(con_duck, "view_daten_struktur", daten_struktur)


#---

DBI::dbDisconnect(con)
DBI::dbDisconnect(con_duck, shutdown = TRUE)
rm(tabs, i, con, con_duck, daten_link)

