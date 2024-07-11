
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

#data_pfad <- "https://stifterverband.sharepoint.com/:f:/s/DatenmenscheninSVundTchtern/EtSyfMRLh1pMi698TgLRWl0BWNRHHA586NEpFqQ5usdsXQ?e=Grgpts"
#data_pfad <- "C:/Users/kbr/Desktop/temp"
# klappt bei mir grad nicht mit dem Sharepoint/Teams-Pfad, aber sonst immer mit "normalen" Sharepoint-Pfaden (kbr)

germany_choropleth_federal_states <- readRDS(file = sprintf("%s/germany_choropleth_federal_states.rds", 
                                                            data_pfad))
germany_choropleth_federal_states$geometry_wkt <- sf::st_as_text(germany_choropleth_federal_states$geometry)
germany_choropleth_federal_states$geometry <- NULL
names(germany_choropleth_federal_states)[names(germany_choropleth_federal_states) 
                                         == "geometry_wkt"] <- "geometry"

middle_points_of_ger_federal_states <- readRDS(file = sprintf("%s/middle_points_of_ger_federal_states.rds", 
                                                              data_pfad))
chart_options_rules <- readxl::read_xlsx(sprintf("%s/chart_options_rules.xlsx", data_pfad))
DBI::dbWriteTable(con_duck, 'germany_choropleth_federal_states', 
                  germany_choropleth_federal_states, overwrite = TRUE, append = FALSE)
DBI::dbWriteTable(con_duck, 'middle_points_of_ger_federal_states', 
                  middle_points_of_ger_federal_states, overwrite = TRUE, append = FALSE)
DBI::dbWriteTable(con_duck, 'chart_options_rules', chart_options_rules, 
                  overwrite = TRUE, append = FALSE)

DBI::dbDisconnect(con_duck, shutdown = TRUE)
rm(con_duck, DB_FILE, data_pfad, studierende, studierende_detailliert)
