
box::use(
  ../../R/utils/monitor_content[
    create_collapsible_panel,
    create_link_with_svg,
    load_table_by_variable_monitor
  ],
  ../../R/utils/monitor_content[
    get_content_monitor_bildung,
    monitor_bildung_box_server,
    monitor_bildung_box_ui
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
    updatePickerInput,
    radioGroupButtons
  ],
  shinycssloaders[withSpinner]
)

content_monitor <- get_content_monitor_bildung()

# UI

module_monitor_bildung_ui <- function(id = "monitor_bildung", label = "m_monitor_bildung") {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0("monitor_styles.css?version=", Sys.time())
      )
    ),
    fluidPage(
      div(
        class = "panel-content",
        withSpinner(uiOutput(ns("monitor_content")))
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
        input$back_button,{
          current$content <- NULL
          change_page("monitor?hf=1")
        }
      )

      observeEvent(
        input$switch_fragen_indikatoren_ziele, {
          if (!is.null(current$content)){
            output$list_fragen_indikatoren <-
              renderUI({
                draw_fragen_ziele_indikatoren(current$content, input$switch_fragen_indikatoren_ziele)
              })
          }
        }
      )

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

      output$monitor_content <- renderUI({draw_content(current$content, con = con, ns = ns)})

      observeEvent(
        input$gliederungsauswahlcontent1_in, {

        }
      )

    }
  )
}


draw_content <- function(content, con, ns){

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
          actionButton(ns("back_button"), label = HTML("Zurück<br>zur Übersicht"))
        ),
        fluidRow(
          width = 12,
          div(
            class = "fragen-indikatoren-ziele-box monitor-sidbar-row",
            div(
              radioGroupButtons(
                ns("switch_fragen_indikatoren_ziele"),
                "",
                choices = c("Fragen", "Ziele", "Indikatoren"),
                justified = TRUE
              )
            ),
            uiOutput(ns("list_fragen_indikatoren"))
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
                    monitor_bildung_box_ui(paste0("content", .), load_table_by_variable_monitor(138, con), con, ns)#,
                    #monitor_bildung_box_server(paste0("content", .), load_table_by_variable_monitor(138, con), con)
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

draw_fragen_ziele_indikatoren <- function(content, input){
  tags$ul(
    map2(
      unlist(content["ID"]),
      unlist(content[input]),
      ~ tags$li(
        tags$a(
          class = "link",
          href = paste0("#", .x),
          .y,
          draw_svg_arrow()
        )
      )
    )
  )
}

draw_svg_arrow <- function(){
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
}
