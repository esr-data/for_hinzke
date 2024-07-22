box::use(
  DBI, svMagpie,
  stats[aggregate],
  ../../R/pkgs/svNum/numeric[convert_zeit],
  ../../R/utils/string[preprocess_str]
)

export_standard_tables <- function(con_duck){
  con  <- svMagpie::connect_magpie()
  tabs <-
    c(
      DBI::dbListTables(con),
      DBI::dbGetQuery(con, "SELECT matviewname AS view_name FROM pg_matviews;")[,1]
    )

  for (i in tabs){
    message(paste0("Export Tabelle: ", i))
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
  DBI::dbDisconnect(con)
  return(invisible(NULL))
}

create_view_daten_link <- function(con_duck){
  con  <- svMagpie::connect_magpie()
  daten_link <- DBI::dbGetQuery(con, paste(readLines("SQL/daten_link.sql"), collapse = " "))
  DBI::dbDisconnect(con)
  daten_link$daten_id <- as.integer(daten_link$daten_id)
  DBI::dbWriteTable(con_duck, "view_daten_link", daten_link)
  return(invisible(NULL))
}

create_view_daten_struktur <- function(con_duck){
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
  daten_struktur <- daten
  daten_struktur$daten_id <- as.integer(daten_struktur$daten_id)
  DBI::dbWriteTable(con_duck, "view_daten_struktur", daten_struktur)
  return(invisible(NULL))
}

create_view_daten_detailed <- function(con){
  view_daten <-
    sprintf(
      "WITH r as (
            SELECT *, (SELECT id FROM tabelle WHERE bez IN ('reichweite')) as tabelle_id
            FROM reichweite
            LEFT JOIN reichweite_typ ON reichweite.reichweite_typ_id = reichweite_typ.id
           ),
           beschreibung as (%s),
           datensammlung as (
            SELECT vdl.daten_id, vdl.reihe_id, vdl.tabelle_id, r.reichweite_typ_id, r.reichweite_klasse_id
            FROM view_daten_link vdl
            LEFT JOIN r ON vdl.reihe_id = r.id AND vdl.tabelle_id = r.tabelle_id
            WHERE vdl.daten_id IN (SELECT DISTINCT daten_id FROM view_daten_link) AND
                  NOT vdl.tabelle_id IN (SELECT id FROM tabelle WHERE bez IN ('reichweite_typ', 'reichweite_klasse'))
           ),
           datensammlung_prepared as (
            SELECT d.daten_id, d.reihe_id as reihe_id, t.bez as tabelle, rt.beschr as reichweite_typ, rk.beschr as reichweite_klasse
            FROM datensammlung d
            LEFT JOIN tabelle t ON d.tabelle_id = t.id
            LEFT JOIN reichweite_typ rt ON d.reichweite_typ_id = rt.id
            LEFT JOIN reichweite_klasse rk ON d.reichweite_klasse_id = rk.id
           )
           SELECT daten_id, beschr, d.tabelle, reichweite_typ, reichweite_klasse
           FROM datensammlung_prepared d
           LEFT JOIN beschreibung b ON d.tabelle = b.tabelle AND d.reihe_id = b.reihe_id",
      (
        c("variable", "quelle", "wert_einheit", "zeit_einheit", "reichweite", "tag") |>
          lapply(\(.) sprintf("SELECT id as reihe_id, '%s' as tabelle, beschr FROM %s", ., .)) |>
          paste(collapse = " UNION ")
      )
    ) |>
    DBI::dbGetQuery(conn = con)

  view_daten$gruppe <- ifelse(!is.na(view_daten$reichweite_typ), view_daten$reichweite_typ, view_daten$tabelle)

  view_daten <- aggregate(beschr ~ gruppe + daten_id, view_daten, \(.) paste(sort(unique(.)), collapse = ", "))
  view_daten_prepared <- view_daten[view_daten$gruppe %in% "variable",]
  view_daten_prepared$gruppe <- NULL
  names(view_daten_prepared)[names(view_daten_prepared) %in% "beschr"] <- "variable"

  for (i in unique(view_daten$gruppe)){
    if (!(i %in% "variable")){
      view_daten_i <- view_daten[view_daten$gruppe %in% i,]
      view_daten_prepared[,i] <- view_daten_i$beschr[match(view_daten_prepared$daten_id, view_daten_i$daten_id)]
      rm(view_daten_i)
    }
  }

  daten      <- DBI::dbGetQuery(conn = con, "SELECT daten.*, zeit_einheit.beschr as zeit_einheit FROM daten LEFT JOIN zeit_einheit ON daten.zeit_einheit_id = zeit_einheit.id")
  daten$zeit <- convert_zeit(daten)
  view_daten_prepared[,c("zeit", "wert")] <-
    daten[match(view_daten_prepared$daten_id, daten$id), c("zeit", "wert")]

  reichweite_typ <- DBI::dbGetQuery(conn = con, "SELECT beschr FROM reichweite_typ")$beschr

  names_reichweite <- names(view_daten_prepared)[names(view_daten_prepared) %in% reichweite_typ]

  view_daten_prepared <- view_daten_prepared[,c("daten_id", "variable", "zeit", "zeit_einheit", names_reichweite, "wert", "wert_einheit", "quelle", "tag")]
  names(view_daten_prepared) <-
    ifelse(
      names(view_daten_prepared) %in% names_reichweite,
      gsub(" ", "__", preprocess_str(names(view_daten_prepared))),
      names(view_daten_prepared)
    )

  DBI::dbWriteTable(con, "view_daten_detailed", view_daten_prepared)
  return(invisible(NULL))
}
