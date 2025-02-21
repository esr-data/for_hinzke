#' Necessary Packages/Functions

box::use(
  ../../R/utils/routing[get_hf_param],
  ../../R/utils/ui[with_loader],
  shiny[
    NS, moduleServer, observeEvent,
    reactiveValuesToList, reactiveValues,
    fluidPage, tagList,
    markdown, actionButton,
    h2, div, icon,
    uiOutput, renderUI
  ],
  shiny.router[change_page, get_query_param, get_page]
)

#' @noRd
module_explorer_ui <- function(id = "explorer", label = "m_explorer") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      uiOutput(ns("intro")),
      div(
        id = "exp_auswahl",
        style = "display: flex; flex-direction: row; flex-wrap: wrap; max-width: 800px; margin: 0 auto;",
        actionButton(ns("suche"),     label = "Suche",       class = "link_button", icon = icon("magnifying-glass")),
        actionButton(ns("indikator"), label = "Indikatoren", class = "link_button", icon = icon("chart-bar")),
        actionButton(ns("datensatz"), label = "Datensätze",  class = "link_button", icon = icon("database")),
        actionButton(ns("karten"),    label = "Karten",      class = "link_button", icon = icon("earth-europe"))
      )
    )
  )
}

#' @noRd
module_explorer_server <- function(id = "explorer") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(input$suche,     {change_explorer_page("suchen")})
      observeEvent(input$indikator, {change_explorer_page("indikator_auswahl")})
      observeEvent(input$datensatz, {change_explorer_page("datensaetze")})
      observeEvent(input$karten,    {change_explorer_page("karten")})

      observeEvent(
        get_query_param(), {
          if (get_page() == "explorer"){

            param_hf <- get_query_param("hf")
            if (is.null(param_hf)) param_hf <- 0

            if (param_hf == 1){
              ui_element <- draw_explorer_details("handlung1")
            } else if (param_hf == 2){
              ui_element <- draw_explorer_details("handlung2")
            } else {
              ui_element <- draw_explorer_details("keins")
            }

            output$intro <- renderUI({ui_element})
          }
        },
        ignoreNULL = FALSE
      )

    }
  )
}

#' Missing description
#' @noRd

draw_explorer_details <- function(type){

  text_in_box <- markdown(readLines("md/explorer_standard.md"))

  if (type == "handlung1") {
    color_class <- "handlung1_triangle"
    titel <- "Explorer - Bildung & Kompetenzen"
  } else if (type == "handlung2") {
    color_class <- "handlung2_triangle"
    titel <- "Explorer - Forschung & Innovationen"
  } else {
    color_class <- "keine_handlung_triangle"
    titel <- "Explorer - Alle Indikatoren"
  }

  return(
    tagList(
      h2(titel, style = "text-align: center;"),
      div(
        style = "display: flex; flex-direction: row; margin: 1%;",
        div(class = color_class, style = "width: 200px; height: auto; min-width: 200px; min-height: 200px;"),
        div(
          style = "padding: 0 30px;",
          div(
            style = "width: 100%;",
            text_in_box
          )
        )
      )
    )
  )

}

#' @noRd
report_explorer_subpages <- function(){
  explorer <-
      data.frame(
        url   = c("explorer",     "suchen",             "indikator",              "datensaetze",            "karten"),
        id    = c("sbd_explorer", "sbd_explorer_suche", "sbd_explorer_indikator", "sbd_explorer_datensatz", "sbd_explorer_karten"),
        label = c("Überblick",    "Suche",              "Indikatoren",            "Datensätze",             "Karten")
      )
}

#' Missing description
#' @noRd

change_explorer_page <- function(url){
  change_page(paste0(url, "?hf=", get_hf_param()))
  return(invisible(NULL))
}
