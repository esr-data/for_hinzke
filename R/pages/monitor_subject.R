# library(dplyr)
# library(shiny)
# library(shiny.router)
# library(shinyWidgets)
# library(bsplus)
# library(shinyBS)
# library(DBI)
# library(reactable)
# library(sortable)
# library(shinyjs)
# library(purrr)
# library(lorem)
# library(plotly)
# library(ggplot2)
# library(ggpubr)
# library(stringr)
# library(DT)
#
#con <<- DBI::dbConnect(RSQLite::SQLite(), "data/magpie.sqlite")
# source("test_data.R")
# source("test_content_module.R")
# content_list_monitor_subpage_structure <- content_list_monitor_subpages_structure_full[["bildung_ganztag"]]


box::use(

  # sources

  ../../R/utils/monitor_content[
    create_collapsible_panel,
    create_link_with_svg,
    load_table_by_variable_monitor
  ],
  ../../R/utils/monitor_content_module[
    monitor_indicator_main_content_server,
    monitor_indicator_main_content_ui
  ],

  # packages

  purrr[
    map,
    map2
  ],
  shiny[
    actionButton,
    br,
    column, #
    conditionalPanel, #
    div,
    fluidPage,
    fluidRow,
    h1,
    h3,
    h4,
    HTML,
    img,
    mainPanel,
    markdown,
    moduleServer,
    numericInput, #
    NS,
    observe, #
    observeEvent,
    p,
    radioButtons, #
    reactive, #
    renderUI,
    req, #
    selectInput, #
    sidebarLayout,
    sidebarPanel,
    tagList,
    tags,
    uiOutput
  ],
  shinyjs[
    runjs
  ],
  shinyWidgets[
    pickerInput,
    updatePickerInput
  ]
)

# UI

module_monitor_subject_ui <- function(id = "subject_id", label = "m_monitor_ganztag_als_bildungszeit", type = "all", mon_value, content_list_monitor_subpage_structure) {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = paste0("monitor_styles.css?version=", Sys.time()))

      ),
      uiOutput(ns("subject_ui")),
    )
  )
}

# Server

module_monitor_subject_server <- function(id = "subject_id", con, type = "all", mon_value, content_list_monitor_subpage_structure) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # observeEvent(
      #   content_list_monitor_subpage_structure(), {
      #     map(
      #       1:length(content_list_monitor_subpage_structure()[["Ueberschriften"]]),
      #       ~ content_list_monitor_subpage_structure()[["Indikator_Inhalt_Server"]][[.]]()
      #     )
      #   }
      # )
      #
      # # outputs
      #
      # output$list_fragen_indikatoren <- renderUI({
      #   switch_fragen_indikatoren_ziele_value <- input$switch_fragen_indikatoren_ziele
      #   tags$ul(
      #     map2(
      #       content_list_monitor_subpage_structure()[["ID"]],
      #       content_list_monitor_subpage_structure()[[switch_fragen_indikatoren_ziele_value]],
      #       ~ create_link_with_svg(.x, .y)
      #     )
      #   )
      # })

      output$subject_ui <- renderUI({
        tagList(
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
          ),
          runjs(
            "$('.collapsible-header').click(function() {
           $(this).find('i').toggleClass('arrow-down arrow-up');
         });"
          ),
          div(
            class = "monitor-subpage-title",
            h1(
              class = "monitor-subpage-title-headline",
              content_list_monitor_subpage_structure()[["Titel"]]
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
                      "switch_fragen_indikatoren_ziele",
                      "",
                      choices = c("Fragen", "Ziele", "Indikatoren"),
                      justified = TRUE
                    )
                  ),
                  uiOutput("list_fragen_indikatoren")
                )
              ),
              create_collapsible_panel("Aktivitäten des Stifterverbandes", content_list_monitor_subpage_structure()[["Aktivitaeten_Link"]], content_list_monitor_subpage_structure()[["Aktivitaeten"]], "aktivitaeten-box"),
              create_collapsible_panel("Datenbasis", content_list_monitor_subpage_structure()[["Datenbasis_Link"]], content_list_monitor_subpage_structure()[["Datenbasis"]], "database-box"),
              create_collapsible_panel("weiterführende Links", content_list_monitor_subpage_structure()[["Links_Link"]], content_list_monitor_subpage_structure()[["Links"]], "link-box"),
            ),
            mainPanel(
              width = 9,
              div(
                class = "intro-text-monitor-subsite",
                h3(content_list_monitor_subpage_structure()[["Untertitel"]]),
                content_list_monitor_subpage_structure()[["Einfuehrungstext"]]
              ),
              div(
                class = "info-box-data",
                tags$i(class = "fa fa-info-circle"),
                markdown(readLines("md/monitor_hinweis_datenportal_projektstand.md"))
              ),
              br(),
              h4("Das Wichtigste in Kürze:"),
              div( # TODO - function/flex für Imagegenerierung anhand der gefilterten Liste
                class = "important-values-graph",
                img(
                  src = "img/4er_Ganztag.svg",
                  alt = "Wichtige Kennzahlen zum Ausbau der Ganztagsschule in Duetschland"
                )
              ),
              br(),
              div(
                class = "content-monitor-subsite",
                map(
                  1:length(content_list_monitor_subpage_structure()[["Ueberschriften"]]),
                  ~{
                    div(
                      h3(
                        class = "collapsible-header collapsible-header-main",
                        id = content_list_monitor_subpage_structure()[["ID"]][[.]],
                        content_list_monitor_subpage_structure()[["Ueberschriften"]][[.]],
                        tags$i(class = "arrow-down")
                      ),
                      div(
                        class = "collapsible-content collapsible-content-main",
                        div(
                          class = "content-ask-aim-ind",
                          tags$ul(
                            tags$li(tags$i(class = "fa-solid fa-clipboard-question"),tags$b("Frage:"), content_list_monitor_subpage_structure()[["Fragen"]][[.]]),
                            tags$li(tags$i(class = "fa-solid fa-bullseye"),tags$b("Ziel:"), content_list_monitor_subpage_structure()[["Ziele"]][[.]]),
                            tags$li(tags$i(class = "fa-solid fa-ruler"),tags$b("Indikator:"), content_list_monitor_subpage_structure()[["Indikatoren"]][[.]])
                          )
                        ),
                        br(),
                        div(
                          class = "real-content",
                          monitor_indicator_main_content_ui(paste0("content", .), load_table_by_variable_monitor(138, con), con),
                          monitor_indicator_main_content_server(paste0("content", .), load_table_by_variable_monitor(138, con), con)
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
      })

    }
  )
}

# Demo / Debugging
# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   module_monitor_subject_ui("monitor_ganztag_als_bildungszeit")
# )
#
# server <- function(input, output, session) {
#   module_monitor_subject_server("monitor_ganztag_als_bildungszeit")
# }
#
# shinyApp(ui, server)
# profvis::profvis(shinyApp(ui, server)) #
