
box::purge_cache()
box::use(
  DBI,
  . / prepare_functions[
    export_standard_tables,
    create_view_daten_link,
    create_view_daten_struktur,
    create_view_daten_detailed
  ]
)

DB_FILE <- "data/magpie.db"

file.remove(DB_FILE)
con_duck <- DBI::dbConnect(duckdb::duckdb(), DB_FILE)

export_standard_tables(con_duck)
create_view_daten_link(con_duck)
create_view_daten_struktur(con_duck)
create_view_daten_detailed(con_duck)

DBI::dbDisconnect(con_duck, shutdown = TRUE)
rm(con_duck, DB_FILE)
