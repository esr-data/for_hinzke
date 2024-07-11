
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

#data_pfad <- "https://stifterverband.sharepoint.com/:f:/s/DatenmenscheninSVundTchtern/EtSyfMRLh1pMi698TgLRWl0BWNRHHA586NEpFqQ5usdsXQ?e=2vFntG"
#funktioniert bei mir grade nicht mit Link nach sharepoint/Teams, nur mit lokalen oder "normalen" SV-Daten Sharepoint Links
#data_pfad <- "C:/Users/kbr/Desktop/temp"
load(file = sprintf("%s/studierende.rda", data_pfad))
load(file = sprintf("%s/studierende_detailliert.rda", data_pfad))
studierende <- as.data.frame(studierende)
studierende_detailliert <- as.data.frame(studierende_detailliert)
DBI::dbWriteTable(con_duck, 'studierende', studierende, overwrite = TRUE, append = FALSE)
DBI::dbWriteTable(con_duck, 'studierende_detailliert', studierende_detailliert, overwrite = TRUE, append = FALSE)

DBI::dbDisconnect(con_duck, shutdown = TRUE)
rm(con_duck, DB_FILE, data_pfad, studierende, studierende_detailliert)
