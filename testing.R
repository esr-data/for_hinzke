# Test Data

content_list_monitor_subpages_structure_full <-
  list(
    "bildung_ganztag" = list(
      "Titel" = "Ganztag als Bildungszeit",
      "Untertitel" = "Wie steht es um den Ausbau der Ganztagsschule?",
      "Einfuehrungstext" = lorem::ipsum(sentences = 4, avg_words_per_sentence = 14),
      "Ueberschriften" = c(
        "Ausbau der Ganztagsangebote",
        "Vielfalt der Ganztagsangebote",
        "Kooperationen zwischen Schule und Zivilgesellschaft im Ganztag",
        "Sozialer Ausgleich bei Ausbau der Ganztagsangebote",
        "Multiprofessionele Teams an Schulen",
        "IT-Infrastruktur an Schulen",
        "Die Lage an den Schulen aus Sicht der Schulleitungen"
      ),
      "Fragen" = c(
        "Schaffen wir quantitativ genügend Ganztagsangebote?",
        "Schaffen wir vielfältige Ganztagsangebote?",
        "Schaffen wir Kooperationen zwischen Schule und Zivilgesellschaft?",
        "Schaffen wir es Ganztagsschule sozial einzuführen?",
        "Schaffen wir es multiprofessionelle Teams in den Schulen zu verankern?",
        "Schaffen wir die Voraussetzungen für digitale Bildungsangebote?",
        "Schaffen wir es die wahrgenomme Lage an den Schulen zu verbessern?"
      ),
      "Ziele" = c(
        "Schulen in Deutschland sind Ganztagsschulen",
        "Ganztagsunterricht wird vielseitig gestaltet",
        "Zivilgesellschaftlicher Akteure sind in den Ganztag eingebunden",
        "Insbesondere in Schulen mit niedrigem Sozialindex sind zivilgesellschaftliche Akteure eingebunden",
        "Multiprofessionelle Teams an Schulen",
        "Schulen haben WLAN",
        "Die Lage an Schulen wird als gut eingeschätzt"
      ),
      "Indikatoren" = c(
        "Anteil der Ganztagsschulen in Deutschland",
        "Vielfalt der Angebote im Rahmen der Ganztagsbetreuung",
        "Einbindung zivilgesellschaftlicher Akteure",
        "Einbindung zivilgesellschaftlicher Akteure nach Sozialindex",
        "Anteil Schulpersonal außerhalb der Lehrkräfte an Schulpersonal",
        "Anteil Schulen mit WLAN über 100 Mbit",
        "Anteil Schulleitungen die Lage an ihrer Schule insgesamt als gut bezeichnen"
      ),
      "Aktivitäten" = c(
        "Ganztag Aktivität 1",
        "Ganztag Aktivität 2",
        "Ganztag Aktivität 3"
      ),
      "Datenbasis" = c(
        "Ganztag Datenbasis 1",
        "Ganztag Datenbasis 2"
      ),
      "Links" = c(
        "Ganztag Link 1",
        "Ganztag Link 2",
        "Ganztag Link 3"
      )
    ),
    "bildung_berufsorientierung" = list(
      "Titel" = "Berufsorientierung stärken",
      "Ueberschriften" = c(
        "Berufsorientierung 1",
        "Berufsorientierung 2",
        "Berufsorientierung 3"
      ),
      "Fragen" = c(
        "Frage 1",
        "Frage 2",
        "Frage 3"
      ),
      "Ziele" = c(
        "Ziel 1",
        "Ziel 2",
        "Ziel 3"
      ),
      "Indikatoren" = c(
        "Indikator 1",
        "Indikator 2",
        "Indikator 3"
      ),
      "Aktivitäten" = c(
        "Berufsorientierung Aktivität 1",
        "Berufsorientierung Aktivität 2",
        "Berufsorientierung Aktivität 3"
      ),
      "Datenbasis" = c(
        "Berufsorientierung Datenbasis 1",
        "Berufsorientierung Datenbasis 2"
      ),
      "Links" = c(
        "Berufsorientierung Link 1",
        "Berufsorientierung Link 2",
        "Berufsorientierung Link 3"
      )
    )
  )

content_list_monitor_subpage_structure <- content_list_monitor_subpages_structure_full[["bildung_ganztag"]]

# UI

module_monitor_ganztag_als_bildungszeit_ui <- function(id = "monitor_ganztag_als_bildungszeit", label = "m_monitor_ganztag_als_bildungszeit", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$head(
        tags$style(
          HTML(
            "
              /* Monitor subpages */

              .monitor-sidbar-row {
                 margin-bottom: 15px;
                 padding: 10px;
                 background-color: #fff;
                 border: 2px solid #91bea0;
                 border-radius: 5px;
                 box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
              }

              .monitor-sidbar-row button {
                 background-color: #91bea0;
                 color: #ffffff;
                 border: 1px solid #ffffff;
                 padding: 5px 10px;
                 border-radius: 4px;
              }

              .monitor-sidbar-row button:hover {
                 background-color: #238823;
              }

              .monitor-sidbar-row button:focus {
                 border-color: #ffcc00;
                 outline: none;
                 box-shadow: 0 0 5px #ffcc00;
              }

              ul {
                padding-left: 20px;
              }

              ul li {
                margin-bottom: 5px;
              }

              .well{
                background-color: #fff;
                border: none;
                box-shadow: none;
              }
            "
          )
        )
      ),
      titlePanel(
        fluidRow(
          actionButton("backBtn", label = "Zurück zum Überblick", icon = icon("arrow-left")),
          content_list_monitor_subpage_structure[["Titel"]]
        ),
      ),
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fluidRow(
            width = 12,
            div(
              class = "fragen-indikatoren-ziele-box monitor-sidbar-row",
              div(
                shinyWidgets::radioGroupButtons(
                  ns("switch_fragen_indikatoren_ziele"),
                  "",
                  choices = c("Fragen", "Indikatoren", "Ziele"),
                  justified = TRUE
                )
              ),
              uiOutput(ns("list_fragen_indikatoren"))
            )
          ),
          fluidRow(
            class = "aktivitaeten-box monitor-sidbar-row",
            width = 12,
            div(
              h4("Aktivitäten des Stifterverbandes"),
              tags$ul(
                lapply(content_list_monitor_subpage_structure[["Aktivitäten"]], tags$li)
              )
            )
          ),
          fluidRow(
            class = "database-box monitor-sidbar-row",
            width = 12,
            div(

              h4("Datenbasis"),
              tags$ul(
                lapply(content_list_monitor_subpage_structure[["Datenbasis"]], tags$li)
              )
            )
          ),
          fluidRow(
            class = "link-box monitor-sidbar-row",
            width= 12,
            div(
              h4("weiterführende Links"),
              tags$ul(
                lapply(content_list_monitor_subpage_structure[["Links"]], tags$li)
              )
            )
          )
        ),
        mainPanel(
          width = 9,
          div(
            class = "intro-text-monitor-subsite",
            h4(content_list_monitor_subpage_structure[["Untertitel"]]),
            content_list_monitor_subpage_structure[["Einfuehrungstext"]]
          ),
          br(),
          div(
            h4("Das wichtigste in Kürze"),
            img(
              src = "img/4er.svg",
              alt = "Wichtige Kennzahlen zu Ganztag"
            ),
          ),
          br(),
          div(
            class = "content-monitor-subsite",
            purrr::map(
              content_list_monitor_subpage_structure[["Ueberschriften"]],
              ~{
                shinydashboard::box(
                  width= 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  title = .
                )
            })
          )
        )
      )
    )
  )
}

# Server

module_monitor_ganztag_als_bildungszeit_server <- function(id = "monitor_ganztag_als_bildungszeit", con, type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # outputs

      output$list_fragen_indikatoren <- renderUI({
        switch_fragen_indikatoren_ziele_value <- input$switch_fragen_indikatoren_ziele
        tags$ul(lapply(content_list_monitor_subpage_structure[[switch_fragen_indikatoren_ziele_value]], tags$li))
      })


    }
  )
}

# Demo / Debugging
ui <- fluidPage(
  shinyjs::useShinyjs(),
  module_monitor_ganztag_als_bildungszeit_ui("monitor_ganztag_als_bildungszeit")
)

server <- function(input, output, session) {
  module_monitor_ganztag_als_bildungszeit_server("monitor_ganztag_als_bildungszeit")
}

shinyApp(ui, server)
# profvis::profvis(shinyApp(ui, server)) #
