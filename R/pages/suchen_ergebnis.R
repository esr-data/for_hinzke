#' Necessary Packages/Functions

box::use(
  ../../R/utils/ui[draw_under_construction],
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, fluidRow, tagList,
    uiOutput, renderUI,
    div, HTML, h5,
    actionButton
  ],
  shiny.router[get_query_param, get_page, change_page]
)

#' Missing description
#' @export

module_suche_ergebnis_ui <- function(id = "suche_ergebnis", label = "m_suche_ergebnis") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      uiOutput(ns("ergebnisse"))
    )
  )
}

#' Missing description
#' @export

module_suche_ergebnis_server <- function(id = "suche_ergebnis") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(
        get_query_param(), {
          if (get_page() == "suchergebnisse"){

            ergebnis_id <- get_query_param("input")
            if (!grepl("^[0-9_-]+$", ergebnis_id)){
              ergebnis_id <- NA
            }
            if (is.null(ergebnis_id)){
              change_page("suchen")
            } else if (is.na(ergebnis_id) | nchar(ergebnis_id) == 0){
              change_page("suchen")
            }

            output$ergebnisse <- renderUI({draw_ergebnisse(ergebnis_id)})

          }
        },
        ignoreNULL = FALSE
      )

    }
  )
}

draw_ergebnisse <- function(ergebnis_id){
  div(
    HTML(sprintf("<h5>%s</h5>", ergebnis_id)),
    draw_under_construction()
  )
}

