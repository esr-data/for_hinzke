
box::use(

  # sources

  ../../R/utils/monitor_content[
    create_collapsible_panel,
    create_link_with_svg,
    load_table_by_variable_monitor
  ],
  ../../R/utils/monitor_content[
    get_content_monitor_bildung,
    monitor_indicator_main_content_server,
    monitor_indicator_main_content_ui
  ],
  purrr[map, map2],
  shiny[
    actionButton,
    br,
    column,
    conditionalPanel,
    div,
    fluidPage, fluidRow,
    h1, h3, h4,
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
    reactiveValues,
    renderUI,
    req, #
    selectInput, #
    sidebarLayout,
    sidebarPanel,
    tagList,
    tags,
    uiOutput
  ],
  shinyjs[runjs],
  shiny.router[get_query_param, get_page, change_page],
  shinyWidgets[
    pickerInput,
    updatePickerInput
  ]
)

content_monitor <- get_content_monitor_bildung()

# UI

module_monitor_bildung_ui <- function(id = "monitor_bildung", label = "m_monitor_bildung") {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = paste0("monitor_styles.css?version=", Sys.time()))
    ),
    fluidPage(
      div(
        class = "panel-content",
        uiOutput(ns("monitor_content"))
      )
    )
  )
}

# Server

module_monitor_bildung_server <- function(id = "monitor_bildung", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      current <- reactiveValues(content = NULL)

      observeEvent(
        get_query_param(), {
          if (get_page() == "monitor_bildung"){
            param_tp <- get_query_param()$tp
            if (!is.null(param_tp)){
              if (param_tp %in% names(content_monitor)){
                current$content <- content_monitor[[param_tp]]
              } else {
                current$content <- NULL
                change_page("monitor?hf=1")
              }
            } else {
              current$content <- NULL
              change_page("monitor?hf=1")
            }
          }
        }
      )

      output$monitor_content <- renderUI({draw_content(current$content, con = con)})
    }
  )
}


draw_content <- function(content, con){

  if (is.null(content)) return(HTML(""))

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
        content[["Titel"]]
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
        create_collapsible_panel("Aktivitäten des Stifterverbandes", content[["Aktivitaeten_Link"]], content[["Aktivitaeten"]], "aktivitaeten-box"),
        create_collapsible_panel("Datenbasis", content[["Datenbasis_Link"]], content[["Datenbasis"]], "database-box"),
        create_collapsible_panel("weiterführende Links", content[["Links_Link"]], content[["Links"]], "link-box"),
      ),
      mainPanel(
        width = 9,
        div(
          class = "intro-text-monitor-subsite",
          h3(content[["Untertitel"]]),
          content[["Einfuehrungstext"]]
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
            1:length(content[["Ueberschriften"]]),
            ~{
              div(
                h3(
                  class = "collapsible-header collapsible-header-main",
                  id = content[["ID"]][[.]],
                  content[["Ueberschriften"]][[.]],
                  tags$i(class = "arrow-down")
                ),
                div(
                  class = "collapsible-content collapsible-content-main",
                  div(
                    class = "content-ask-aim-ind",
                    tags$ul(
                      tags$li(tags$i(class = "fa-solid fa-clipboard-question"),tags$b("Frage:"), content[["Fragen"]][[.]]),
                      tags$li(tags$i(class = "fa-solid fa-bullseye"),tags$b("Ziel:"), content[["Ziele"]][[.]]),
                      tags$li(tags$i(class = "fa-solid fa-ruler"),tags$b("Indikator:"), content[["Indikatoren"]][[.]])
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
}
