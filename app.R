
# --- Packages laden -------------------------------------------------------------------------------

library(dplyr)
library(shiny)
library(shiny.router)
library(shinyWidgets)
library(bsplus)
library(shinyBS)
library(DBI)
library(reactable)
library(sortable)
library(shinyjs)
library(purrr)
library(lorem)

# --- Code laden -----------------------------------------------------------------------------------

for (j in c("utils", "pages", "build")){
  for (i in file.path("R", j) |> list.files()){
    source(file.path("R", j, i))
  }
}
rm(i, j)

source("test_data.R")
content_list_monitor_subpage_structure <- content_list_monitor_subpages_structure_full[["bildung_ganztag"]]

# --- App starten ----------------------------------------------------------------------------------

shinyApp(
  draw_ui(),
  server,
  onStart = function() {con <<- dbConnect(RSQLite::SQLite(), "data/magpie.sqlite")}
)
