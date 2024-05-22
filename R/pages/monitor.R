#' Necessary Packages/Functions

box::use(
  ../../R/utils/routing[get_hf_param],
  ../../R/utils/ui[draw_under_construction],
  ../../R/utils/js[get_js],
  shiny[
    NS, moduleServer, observeEvent, actionButton,
    fluidPage, div, HTML, h1, icon, tags, p, br, h2,
    uiOutput, renderUI, markdown, fluidRow, column
  ],
  shiny.router[get_query_param, get_page, change_page]
)

#' Missing description
#' @export

module_monitor_ui <- function(id = "monitor", label = "m_monitor") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      uiOutput(ns("header")),
      uiOutput(ns("beschreibung")),
      div(
        id = ns("mon"),
        uiOutput(ns("back_btn")),
        uiOutput(ns("monitor_svg"))
      ),
      get_js("monitor_svg_click")
    )
  )
}

#' Missing description
#' @export

module_monitor_server <- function(id = "monitor", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(
        get_query_param(), {
          if (get_page() == "monitor"){

            param_hf <- get_query_param("hf")

            if (is.null(param_hf)) param_hf <- 0
            if (length(param_hf) != 1) param_hf <- 0
            if (is.na(param_hf)) param_hf <- 0
            if (!(param_hf %in% 0:2)) param_hf <- 0

            if (param_hf == 1){
              hf_header <- HTML("Monitoring zu<br>Bildung & Kompetenzen")
              css_class_add <- " hf1"
            } else if (param_hf == 2){
              hf_header <- HTML("Monitoring zu<br>Forschung & Innovation")
              css_class_add <- " hf2"
            } else {
              hf_header <- HTML("Monitoring<br>zu unseren Handlungsfeldern")
              css_class_add <- ""
            }

            output$header <-
              renderUI({
                div(
                  class = "subpage-title",
                  h1(hf_header, class = paste0("subpage-title-headline", css_class_add)),
                  div(class = paste0("header-title-clipgraph", css_class_add)),
                )
              })

            output$beschreibung <-
              renderUI({
                div(
                  style = "background-color: #EAEDEF; padding: 20px; margin: 20px;",
                  div(
                    style = "color: #195365; margin: 0;",
                    markdown(readLines("md/information_monitor.md"))
                  )
                )
              })

            if(css_class_add == "") {

              output$back_btn <- renderUI("")
              output$monitor_svg <- renderUI({
                div(
                  br(),
                  br(),
                  h2("AUSWAHL", style = "text-align: center;"),
                  br(),
                  br(),
                div(
                  class = "monitor_overview",
                  actionButton(
                    ns("monitor_hf1_button"),
                    HTML("Handlungsfeld I:<br><strong>Bildung und Kompetenzen</strong><br>Ziel: Mehr Menschen können die Transformation gestalten"),
                    class = "monitor_hf1_button"
                  ),
                  div(class = "monitor_arrow", "⇆"),
                  actionButton(
                    ns("monitor_hf2_button"),
                    HTML("Handlungsfeld II:<br><strong>Kollaborative Forschung und Innovation</strong><br>Ziel: Besserer Wissenstransfer und mehr Innovationen"),
                    class = "monitor_hf2_button"
                  )
                ))
              })

            } else if (css_class_add == " hf1") {

              output$back_btn <- renderUI({
                fluidRow(
                  actionButton(
                    ns("back_button"),
                    class = "back_button_monitor",
                    label = HTML("Zurück<br>zur Auswahl")
                  )
                )
              })

              output$monitor_svg <- renderUI({HTML(readLines("www/img/Test_Monitor.svg"))})

            } else {

              output$back_btn <- renderUI({
                fluidRow(
                  actionButton(
                    ns("back_button"),
                    class = "back_button_monitor",
                    label = HTML("Zurück<br>zur Auswahl")
                  )
                )
              })

              output$monitor_svg <- # ToDo: Streichen/ersetzen, wenn Monitor HF2 an Start geht
                renderUI({
                  fluidRow(
                    br(),
                    br(),
                    column(
                      width = 2,
                      style = "display: flex; justify-content: center; align-items: center; color: var(--red); font-size: 130px;",
                      icon(
                        style = "margin-right: 10px;",
                        "screwdriver-wrench"
                      )
                    ),
                    column(
                      width = 9,
                      p(
                        tags$b("Hier wird noch gebaut:"),
                        "Das Monitoring für das Handlungsfeld kollaborative
                        Forschung und Innovation befindet sich gerade noch im
                        Aufbau. Bald finden Sie hier Fragen, Ziele und Indikatoren
                        zu unseren Fokusthemen in diesem Handlungsfeld. Akutelle
                        Meldungen zu neuen Featuren und/oder Datenbeständen in
                        unserem Datenportal finden Sie immer im Bereich
                        \"Aktuelles\" auf unserer", tags$a(href = "https://www.stifterverband-datenportal.org", "Startseite"), "."
                      )
                    )
                  )
                })

            }
          }
        }
      )

      observeEvent(
        input$mon, {
          new_url <- paste0("monitor_inhalt?hf=1&ind=1&tp=", gsub("circle_", "", input$mon))
          change_page(new_url)
        }
      )

      observeEvent(
        input$back_button, {
          change_page("monitor?hf=0")
        }
      )

      observeEvent(
        input$monitor_hf1_button, {
          change_page("monitor?hf=1")
        }
      )

      observeEvent(
        input$monitor_hf2_button, {
          change_page("monitor?hf=2")
        }
      )

    }
  )
}
