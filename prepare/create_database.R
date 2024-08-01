
box::purge_cache()
box::use(
  DBI,
  . / prepare_functions[
    export_standard_tables,
    create_view_daten_link,
    create_view_daten_struktur,
    create_view_daten_detailed,
    create_view_daten_reichweite_menge
  ]
)

DB_FILE <- "data/magpie.db"

file.remove(DB_FILE)
con_duck <- DBI::dbConnect(duckdb::duckdb(), DB_FILE)

export_standard_tables(con_duck)
create_view_daten_link(con_duck)
create_view_daten_struktur(con_duck)
create_view_daten_detailed(con_duck)
create_view_daten_reichweite_menge(con_duck)

#data_pfad <- "https://stifterverband.sharepoint.com/:f:/s/DatenmenscheninSVundTchtern/EtSyfMRLh1pMi698TgLRWl0BWNRHHA586NEpFqQ5usdsXQ?e=Grgpts"
#data_pfad <- "C:/Users/kbr/Desktop/temp"
# klappt bei mir grad nicht mit dem Sharepoint/Teams-Pfad, aber sonst immer mit "normalen" Sharepoint-Pfaden (kbr)

# Lade die Daten
load(file = "data/studierende.rda")
load(file = "data/studierende_detailliert.rda")
load(file = "data/kurse.rda")
studierende <- as.data.frame(studierende)
studierende_detailliert <- as.data.frame(studierende_detailliert)
kurse <- as.data.frame(kurse)

# Schreibe die Daten in die Datenbank
DBI::dbWriteTable(con_duck, 'studierende', studierende, overwrite = TRUE, append = FALSE)
DBI::dbWriteTable(con_duck, 'studierende_detailliert', studierende_detailliert, overwrite = TRUE, append = FALSE)
DBI::dbWriteTable(con_duck, 'kurse', kurse, overwrite = TRUE, append = FALSE)

# Lade die Geometrie-Daten und passe sie an
germany_choropleth_federal_states <- readRDS("data/germany_choropleth_federal_states.rds")
germany_choropleth_federal_states$geometry_wkt <- sf::st_as_text(germany_choropleth_federal_states$geometry)
germany_choropleth_federal_states$geometry <- NULL
names(germany_choropleth_federal_states)[names(germany_choropleth_federal_states) == "geometry_wkt"] <- "geometry"

# Lade die Mittelpunkte der Bundesländer
middle_points_of_ger_federal_states <- readRDS("data/middle_points_of_ger_federal_states.rds")

chart_options_rules <- readxl::read_xlsx("data/chart_options_rules.xlsx")
DBI::dbWriteTable(con_duck, 'germany_choropleth_federal_states',
                  germany_choropleth_federal_states, overwrite = TRUE, append = FALSE)
DBI::dbWriteTable(con_duck, 'middle_points_of_ger_federal_states',
                  middle_points_of_ger_federal_states, overwrite = TRUE, append = FALSE)
DBI::dbWriteTable(con_duck, 'chart_options_rules', chart_options_rules,
                  overwrite = TRUE, append = FALSE)

DBI::dbDisconnect(con_duck, shutdown = TRUE)
rm(con_duck, DB_FILE, germany_choropleth_federal_states, middle_points_of_ger_federal_states,
   studierende, studierende_detailliert, kurse, chart_options_rules)
