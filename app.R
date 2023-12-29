
# options(shiny.reactlog = TRUE)

# App starten
rm(list = ls())
box::purge_cache()
box::use(
  R/build/server[server],
  R/build/ui[draw_ui]
)

shinyApp(draw_ui(), server)
