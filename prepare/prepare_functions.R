box::use(
  DBI, svMagpie,
  stats[aggregate],
  ../R/pkgs/svNum/numeric[convert_zeit],
  ../R/utils/string[preprocess_str]
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


create_view_daten_reichweite_menge <-
  function(con){
    wrangling_input_view_daten <-
      DBI::dbGetQuery(
        conn = con,
        "WITH rw_to_m AS (
         SELECT mz.reichweite_id,
            string_agg(m.id::text, ','::text ORDER BY mz.menge_id) AS menge_id_list,
            string_agg(vm.reichweiten, ','::text) AS menge_reichweiten
           FROM menge_zugehoerig mz
             JOIN menge m ON m.id = mz.menge_id
             LEFT JOIN view_menge vm ON vm.id = m.id
          GROUP BY mz.reichweite_id
          ORDER BY mz.reichweite_id
        ), rdarray AS (
         SELECT dr.daten_id,
            count(dr.daten_id) AS reichweite_az,
            string_agg(dr.reichweite_id::text, ','::text ORDER BY dr.reichweite_id) AS reichweite_id_list,
            string_agg(r.beschr, ' | '::text ORDER BY dr.reichweite_id) AS reichweite_beschr_list,
            string_agg((('['::text || dr.reichweite_id) || '] '::text) || r.beschr, ' | '::text ORDER BY dr.reichweite_id) AS reichweite_id_beschr_list,
            string_agg((dr.reichweite_id::text || '::'::text) || rm.menge_id_list, ';'::text) AS reichweite_menge_id_list,
            string_agg((r.beschr || '::'::text) || vr.typ_beschr, ' | '::text ORDER BY dr.reichweite_id) AS reichweite_typ_list,
            string_agg(vr.typ_beschr, ' | '::text ORDER BY vr.typ_beschr) AS typ_list,
            string_agg((r.beschr || '::'::text) || vr.klasse_beschr, ' | '::text ORDER BY dr.reichweite_id) AS reichweite_klasse_list,
            string_agg(vr.klasse_beschr, ' | '::text ORDER BY vr.klasse_beschr) AS klasse_list,
            string_agg(rm.menge_reichweiten, ' --- '::text) AS menge_zugeh_reichweiten
           FROM daten_reichweite dr
             JOIN reichweite r ON r.id = dr.reichweite_id
             LEFT JOIN rw_to_m rm ON rm.reichweite_id = dr.reichweite_id
             LEFT JOIN view_reichweite vr ON vr.id = dr.reichweite_id
          GROUP BY dr.daten_id
        ), datenquelle AS (
         SELECT dq_1.daten_id,
            string_agg(q.text, ' --- '::text) AS quelle_list
           FROM daten_quelle dq_1
             LEFT JOIN view_quelle_baum_lesbar q ON q.id = dq_1.quelle_id
          GROUP BY dq_1.daten_id
        ), tag AS (
         SELECT tl.reihe_id AS daten_id,
            string_agg(vtbl.text, ' --- '::text) AS tag_list
           FROM tag_link tl
             LEFT JOIN view_tag_baum_lesbar vtbl ON tl.tag_id = vtbl.id
          WHERE tl.tabelle_id = 1
          GROUP BY tl.reihe_id
        )
 SELECT d.id AS daten_id,
    v.id AS variable_id,
    v.beschr AS variable_beschr,
    d.zeit_start,
    d.zeit_ende,
    ze.beschr AS zeit_einheit,
    d.wert,
    we.beschr AS wert_einheit,
    rdarray.reichweite_az,
    rdarray.reichweite_id_list,
    rdarray.reichweite_beschr_list,
    rdarray.reichweite_id_beschr_list,
    rdarray.reichweite_typ_list,
    rdarray.typ_list,
    rdarray.reichweite_klasse_list,
    rdarray.klasse_list,
    rdarray.reichweite_menge_id_list,
    rdarray.menge_zugeh_reichweiten,
    dq.quelle_list,
    t.tag_list
   FROM daten d
     LEFT JOIN rdarray ON rdarray.daten_id = d.id
     JOIN variable v ON v.id = d.variable_id
     JOIN zeit_einheit ze ON ze.id = d.zeit_einheit_id
     JOIN wert_einheit we ON we.id = d.wert_einheit_id
     LEFT JOIN datenquelle dq ON dq.daten_id = d.id
     LEFT JOIN tag t ON t.daten_id = d.id;

      "
      )

    DBI::dbWriteTable(con, "wrangling_input_view_daten", wrangling_input_view_daten)
    return(invisible(NULL))
  }
