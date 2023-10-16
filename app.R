
library(shiny)
library(shinyBS)
library(shiny.router)
library(shiny.fluent)

for (i in list.files("R")) source(file.path("R", i))
rm(i)

shinyApp(draw_ui(), server) #, options = list(port = 80)
