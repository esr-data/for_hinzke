# This is an example app to understand how nested shiny modules are
# working and what is the general principle

library(shiny)

# Inner Module representing a widget
# that contains of UI rendered parts (text2)
# and server rendered parts (text1)
ui_inner <- function(id, text2) {
  ns <- NS(id)
  tagList(
    h2("Inner"),
    textOutput(ns("text1")),
    p(text2)
  )
}

# Server fun, simply rendering text1
server_inner <- function(input, output, session) {
  output$text1 <- renderText({
    "xx"
  })
}

# Outer module that contains only the inner module and
# a headline
ui_outer <- function(id) {

  ns <- NS(id)
  tagList(
    h1("Outer"),
    shiny::tags$hr(),
    uiOutput(outputId = ns("inner"))
  )
}

server_outer <- function(input, output, session) {

  multiple_inner <- c(
    "mytext" = "Random Generated 1",
    "mytext2" = "Random Generated Text 2"
  )

  output$inner <- renderUI({
    do.call(tagList,
            lapply(names(multiple_inner), function(inner_id) {
              ui_inner(session$ns(inner_id), text2 = multiple_inner[inner_id])
            })
    )
  })

  for (inner_id in names(multiple_inner)) {
    # Outside nested modules automatically know the namespace of themselves
    callModule(server_inner, inner_id)
  }
}

ui <- function(){
  fluidPage(
    title = "Example with nested module calls",
    ui_outer("id_outer")
  )
}

server <- function(input, output, session) {
  callModule(server_outer, "id_outer")
}

shinyApp(ui, server)
