
# App starten

box::use(
  R/build/server[server],
  R/build/ui[draw_ui]
)

shinyApp(draw_ui(), server)
