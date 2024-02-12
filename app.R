
# options(shiny.reactlog = TRUE)

# App starten
rm(list = ls())
box::purge_cache()
box::use(
  R/build/server[server],
  R/build/ui[draw_ui],
  shiny[shinyApp, onStop],
  R/utils/database[disconnect_db],
  R/utils/log[write_log]
)

shinyApp(
  draw_ui(),
  server,
  onStart = function() {
    write_log("Datenportal wird gestartet")
    onStop(function() {
      disconnect_db()
      write_log("Datenportal wird beendet!")
    })
  }
)
