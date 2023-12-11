con <<- dbConnect(RSQLite::SQLite(), "data/magpie.sqlite")
source("test_data.R")
source("test_content_module.R")
content_list_monitor_subpage_structure <- content_list_monitor_subpages_structure_full[["bildung_ganztag"]]

# UI

module_monitor_subject_ui <- function(id, label = "m_monitor_ganztag_als_bildungszeit", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = paste0("test_monitor_subpages.css?version=", Sys.time())),
        tags$script(
          HTML(
            "
              $(document).ready(function() {
                $('.collapsible-header').click(function() {
                  $(this).next('.collapsible-content').slideToggle();
                });
              });
             "
          )
        )
      ),
      div(
        class = "monitor-subpage-title",
        h1(
          class = "monitor-subpage-title-headline",
          content_list_monitor_subpage_structure[["Titel"]]
        ),
        div(
          class = "monitor-subpage-title-clipgraph"
        )
      ),
      br(),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fluidRow(
            class = "back_button_row",
            actionButton("back_button", label = HTML("Zurück<br>zur Übersicht"))
          ),
          fluidRow(
            width = 12,
            div(
              class = "fragen-indikatoren-ziele-box monitor-sidbar-row",
              div(
                shinyWidgets::radioGroupButtons(
                  ns("switch_fragen_indikatoren_ziele"),
                  "",
                  choices = c("Fragen", "Ziele", "Indikatoren"),
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
              h4(
                class = "collapsible-header",
                "Aktivitäten des Stifterverbandes",
                tags$i(class = "arrow-down")
              ),
              div(
                class = "collapsible-content",
                tags$ul(
                  map2(
                    content_list_monitor_subpage_structure[["Aktivitaeten_Link"]],
                    content_list_monitor_subpage_structure[["Aktivitaeten"]],
                    ~ tags$li(
                      tags$a(
                        class = "link",
                        href = paste0("#", .x),
                        .y,
                        tags$svg(
                          class = "link__arrow",
                          width = "10",
                          height = "14",
                          viewBox = "0 0 10 14",
                          `xmlns` = "http://www.w3.org/2000/svg",
                          tags$path(
                            class = "link__arrow-path",
                            d = "M2.55058 14L0.854004 12.3497L6.4527 6.99767L0.854004 1.65025L2.55058 0L9.84645 6.99767L2.55058 14Z"
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            class = "database-box monitor-sidbar-row",
            width = 12,
            div(
              h4(
                class = "collapsible-header",
                "Datenbasis",
                tags$i(class = "arrow-down")
              ),
              div(
                class = "collapsible-content",
                tags$ul(
                  map2(
                    content_list_monitor_subpage_structure[["Datenbasis_Link"]],
                    content_list_monitor_subpage_structure[["Datenbasis"]],
                    ~ tags$li(
                      tags$a(
                        class = "link",
                        href = paste0("#", .x),
                        .y,
                        tags$svg(
                          class = "link__arrow",
                          width = "10",
                          height = "14",
                          viewBox = "0 0 10 14",
                          `xmlns` = "http://www.w3.org/2000/svg",
                          tags$path(
                            class = "link__arrow-path",
                            d = "M2.55058 14L0.854004 12.3497L6.4527 6.99767L0.854004 1.65025L2.55058 0L9.84645 6.99767L2.55058 14Z"
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            class = "link-box monitor-sidbar-row",
            width= 12,
            div(
              h4(
                class = "collapsible-header",
                "weiterführende Links",
                tags$i(class = "arrow-down")
              ),
              div(
                class = "collapsible-content",
                tags$ul(
                  map2(
                    content_list_monitor_subpage_structure[["Links_Link"]],
                    content_list_monitor_subpage_structure[["Links"]],
                    ~ tags$li(
                      tags$a(
                        class = "link",
                        href = paste0("#", .x),
                        .y,
                        tags$svg(
                          class = "link__arrow",
                          width = "10",
                          height = "14",
                          viewBox = "0 0 10 14",
                          `xmlns` = "http://www.w3.org/2000/svg",
                          tags$path(
                            class = "link__arrow-path",
                            d = "M2.55058 14L0.854004 12.3497L6.4527 6.99767L0.854004 1.65025L2.55058 0L9.84645 6.99767L2.55058 14Z"
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        mainPanel(
          width = 9,
          div(
            class = "intro-text-monitor-subsite",
            h3(content_list_monitor_subpage_structure[["Untertitel"]]),
            content_list_monitor_subpage_structure[["Einfuehrungstext"]]
          ),
          div(
            class = "info-box-data",
            tags$i(class = "fa fa-info-circle"),
            p(
              tags$b("Hinweis:"), "Siese Seite ist derzeit noch im sogenannten
              Alpha-Entwicklungsstadium. Das heißt, dass der Daten- und
              Funktionsumfang noch erweitert werden und Programmfehler beseitigt
              werden. Der Stifterverband beginnt mit dem Monitoring
              ab dem Jahr 2024. Entsprechend sind noch nicht alle Daten
              vorhanden und/oder es finden sich ältere Daten. Fehlende
              Erhebungen oder Aktualisierungen führt der Stifterverband in den
              kommenden Monaten durch und stellt die Ergebnisse hier zur
              Verfügung."
            )
          ),
          br(),
          h4("Das Wichtigste in Kürze:"),
          div(
            class = "important-values-graph",
            img(
              src = "img/4er_Ganztag.svg",
              alt = "Wichtige Kennzahlen zum Ausbau der Ganztagsschule in Duetschland"
            )
          ),
          br(),
          div(
            class = "content-monitor-subsite",
            purrr::map(
              1:length(content_list_monitor_subpage_structure[["Ueberschriften"]]),
              ~{
                div(
                  h3(
                    class = "collapsible-header collapsible-header-main",
                    id = content_list_monitor_subpage_structure[["ID"]][[.]],
                    content_list_monitor_subpage_structure[["Ueberschriften"]][[.]],
                    tags$i(class = "arrow-down")
                  ),
                  div(
                    class = "collapsible-content collapsible-content-main",
                    div(
                      class = "content-ask-aim-ind",
                      tags$ul(
                        tags$li(tags$i(class = "fa-solid fa-clipboard-question"),tags$b("Frage:"), content_list_monitor_subpage_structure[["Fragen"]][[.]]),
                        tags$li(tags$i(class = "fa-solid fa-bullseye"),tags$b("Ziel:"), content_list_monitor_subpage_structure[["Ziele"]][[.]]),
                        tags$li(tags$i(class = "fa-solid fa-ruler"),tags$b("Indikator:"), content_list_monitor_subpage_structure[["Indikatoren"]][[.]])
                      )
                    ),
                    br(),
                    div(
                      class = "real-content",
                      monitor_indicator_main_content_ui(ns(content_list_monitor_subpage_structure[["Indikator_Inhalt_IDs"]][[.]]), load_table_by_variable_monitor(138))
                     # content_list_monitor_subpage_structure[["Indikator_Inhalt_UI"]][[.]]()
                     # eval(parse(text = unlist(content_list_monitor_subpage_structure["Indikator_Inhalt_UI"])[.])),
                    ),
                    br(),
                    div(
                      class = "content-footer",
                      p("Notes")
                    )
                  )
                )
              }
            )
          )
        )
      )
    )
  )
}

# Server

module_monitor_subject_server <- function(id, con, type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      map(
        1:length(content_list_monitor_subpage_structure[["Ueberschriften"]]),
        ~ content_list_monitor_subpage_structure[["Indikator_Inhalt_Server"]][[.]]()
      )

      # toggles

      runjs(
        "$('.collapsible-header').click(function() {
           $(this).find('i').toggleClass('arrow-down arrow-up');
         });"
      )

      # outputs

      output$list_fragen_indikatoren <- renderUI({
        switch_fragen_indikatoren_ziele_value <- input$switch_fragen_indikatoren_ziele
        tags$ul(
          map2(
            content_list_monitor_subpage_structure[["ID"]],
            content_list_monitor_subpage_structure[[switch_fragen_indikatoren_ziele_value]],
            ~ tags$li(
              tags$a(
                class = "link",
                href = paste0("#", .x),
                .y,
                tags$svg(
                  class = "link__arrow",
                  width = "10",
                  height = "14",
                  viewBox = "0 0 10 14",
                  `xmlns` = "http://www.w3.org/2000/svg",
                  tags$path(
                    class = "link__arrow-path",
                    d = "M2.55058 14L0.854004 12.3497L6.4527 6.99767L0.854004 1.65025L2.55058 0L9.84645 6.99767L2.55058 14Z"
                  )
                )
              )
            )
          )
        )
      })
    }
  )
}

# Demo / Debugging
ui <- fluidPage(
  shinyjs::useShinyjs(),
  module_monitor_subject_ui("monitor_ganztag_als_bildungszeit")
)

server <- function(input, output, session) {
  module_monitor_subject_server("monitor_ganztag_als_bildungszeit")
}

shinyApp(ui, server)
# profvis::profvis(shinyApp(ui, server)) #
