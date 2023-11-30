
# --- Packages laden -------------------------------------------------------------------------------

# Basics + Databases
library(dplyr)
library(DBI)

# Shiny + Routing
library(shiny)
library(shiny.router)

# Widgets + UI Elements
library(shinyWidgets)
library(bsplus)
library(shinyBS)
library(reactable)
library(sortable)

# Loading-Screens
library(shinycssloaders)
library(waiter)

# --- Code laden -----------------------------------------------------------------------------------

for (j in c("utils", "pages", "build")){
  for (i in file.path("R", j) |> list.files()){
    source(file.path("R", j, i))
  }
}
rm(i, j)

# --- App starten ----------------------------------------------------------------------------------

shinyApp(
  draw_ui(),
  server,
  onStart = function() {
    con <<- dbConnect(RSQLite::SQLite(), "data/magpie.sqlite")
  }
)

