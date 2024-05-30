
box::use(
  ../../R/utils/routing[get_hf_param, add_param_in_url, recode_parameter],
  ../../R/utils/database[get_query, load_table_by_variable],
  ../../R/utils/js[get_js],
  ../../R/utils/monitor[get_content_monitor],
  tibble[
    tibble
  ],
  reactable[
    renderReactable,
    reactable,
    reactableOutput
  ],
  shiny[
    textOutput,
    span,
    actionButton,
    br,
    column,
    conditionalPanel,
    div,
    fluidPage, fluidRow,
    h1, h2, h3, h4,
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
    updateRadioButtons,
    reactiveVal,
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
  shiny.router[get_query_param, get_page, change_page],
  shinyWidgets[
    pickerInput,       updatePickerInput,
    radioGroupButtons, updateRadioGroupButtons
  ],
  plotly[
    plotlyOutput, renderPlotly
  ],
  shinycssloaders[withSpinner],
  dplyr[rename],
  purrr[map2],
  DT[datatable, JS, renderDataTable, dataTableOutput, renderDT],
  shinyjs[
    removeCssClass,
    addCssClass
  ]
)
#TODO irrelevanten Verknüpfungen aussortieren!

# Global Variables

MAX_VARIABLEN   <- 12
content_monitor <- get_content_monitor()
darstellungen   <- list(a = c("Zeitverlauf", "Tabelle", "Karte"))

# UI-Modul -----------------------------------------------------------------------------------------

MAX_VARIABLEN   <- 12
content_monitor <- get_content_monitor()
darstellungen   <- list(a = c("Zeitverlauf", "Tabelle", "Karte"))

# UI-Modul -----------------------------------------------------------------------------------------

module_monitor_inhalt_ui <- function(id = "monitor_inhalt", label = "m_monitor_inhalt") {
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
        div(
          class = "monitor-subpage-title",
          uiOutput(ns("title")),
          div(class = "monitor-subpage-title-clipgraph")
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
                radioGroupButtons(
                  ns("switch_fragen_indikatoren_ziele"),
                  selected = c("Fragen"),
                  choices = c("Fragen", "Ziele", "Indikatoren"),
                  justified = TRUE
                ),
                uiOutput(ns("liste_fragen_ziele_indikatoren"))
              )
            ),
            uiOutput(ns("information_box"))
          ),
          mainPanel(
            width = 9,
            uiOutput(ns("main_panel")),
            div(
              style = "margin: 0 auto; display: table; margin-bottom: 20px;",
              h3(
                style = "font-weight: 600; text-align: center; max-width: 175px; border-bottom: 3px solid var(--blue); border-radius: 3px; padding-bottom: 6px;",
                "Indikatoren"
              )
            ),
            div(
              class = "content-ask-aim-ind",
              reactableOutput(ns("content_table"))
            )
          )
        )
      )
    )
  )
}

# Server-Modul -------------------------------------------------------------------------------------

module_monitor_inhalt_server <- function(id = "monitor_inhalt") {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns

                 current <-
                   reactiveValues(
                     content   = NULL,
                     variable  = NULL,
                     parameter = list(
                       hf  = "",
                       tp  = "",
                       fr  = "",
                       ind = ""
                     )
                   )

                 observeEvent(
                   get_query_param(), {

                     if (get_page() == "monitor_inhalt"){

                       change_in_choice <- FALSE
                       skip_from_now_on <- FALSE
                       parameter        <- get_query_param()

                       # PARAMETER tp -----------------------------------------------------------------------
                       param_tp <-
                         parameter$tp |>
                         recode_parameter(
                           is_integer = FALSE,
                           value_set  = names(content_monitor)
                         )

                       if (param_tp != current$parameter$tp){
                         current$parameter$tp <- param_tp
                         if (param_tp == ""){
                           skip_from_now_on  <- TRUE
                           # current$parameter <- parameter_default
                           # change_page("monitor?hf=1")
                         } else {
                           current$content        <- content_monitor[[param_tp]]
                           output$title           <- renderUI({h1(class = "monitor-subpage-title-headline", current$content[["Titel"]])})
                           output$information_box <- renderUI({draw_information_box(current$content)})
                           output$main_panel      <- renderUI({draw_main_content(current$content)})
                         }
                       }

                       if (!skip_from_now_on){

                         # PARAMETER fr ---------------------------------------------------------------------
                         select_choices <- c("Fragen", "Ziele", "Indikatoren")
                         param_fr <-
                           parameter$fr |>
                           recode_parameter(
                             is_integer = FALSE,
                             value_set  = substr(select_choices, 1, 2)
                           )
                         if (param_fr == "") param_fr <- "Fr"
                         if (param_fr != current$parameter$fr){
                           current$parameter$fr <- param_fr
                           if (!is.null(input$switch_fragen_indikatoren_ziele)){
                             selected_choice <- select_choices[param_fr == substr(select_choices, 1, 2)]
                             if (selected_choice != input$switch_fragen_indikatoren_ziele){
                               updateRadioGroupButtons(
                                 session = session,
                                 inputId = "switch_fragen_indikatoren_ziele",
                                 selected = selected_choice
                               )
                             }
                           }

                           output$liste_fragen_ziele_indikatoren <-
                             renderUI({
                               draw_fragen_ziele_indikatoren(
                                 current$content,
                                 select_choices[substr(select_choices, 1, 2) == param_fr]
                               )
                             })
                         }

                         # PARAMETER ind Teil 1 -------------------------------------------------------------
                         param_ind <-
                           parameter$ind |>
                           recode_parameter(
                             is_integer = TRUE,
                             value_set  = 1:length(current$content$Inhalt$Titel)
                           )
                         if (param_ind == ""){
                           param_ind <- 1
                         }
                       }
                     }

                   }
                 )

                 observeEvent(
                   input$back_button, {
                     param_hf <- get_hf_param()
                     if (param_hf != "") param_hf <- sprintf("?hf=%s", param_hf)
                     change_page(paste0("monitor", param_hf))
                   }
                 )

                 observeEvent(
                   input$switch_fragen_indikatoren_ziele, {
                     if (!is.null(current$content)){
                       if (get_page() == "monitor_inhalt"){
                         current_url <- session$clientData$url_hash
                         new_url <-
                           add_param_in_url(
                             current_url  = current_url,
                             current_page = "monitor_inhalt",
                             parameter    = "fr",
                             value        = substr(input$switch_fragen_indikatoren_ziele, 1, 2),
                             old_value    = get_query_param("fr")
                           )
                         if (new_url != current_url){
                           change_page(new_url)
                         }
                       }
                     }
                   }
                 )

                 output$content_table <- renderReactable({
                   reactable(
                     tibble(section = unlist(current$content[["Inhalt"]][["Titel"]])),
                     details = function(index) {
                       fluidPage(
                         div(
                           class = "content-ask-aim-ind",
                           tags$ul(
                             tags$li(tags$i(class = "fa-solid fa-clipboard-question"), tags$b("Frage: "), current$content[["Inhalt"]][["Frage"]][[index]]),
                             tags$li(tags$i(class = "fa-solid fa-bullseye"), tags$b("Ziel: "), current$content[["Inhalt"]][["Ziel"]][[index]]),
                             tags$li(tags$i(class = "fa-solid fa-ruler"), tags$b("Indikator: "), current$content[["Inhalt"]][["Indikator"]][[index]])
                           )
                         ),
                         br(),
                         div(
                           draw_table_row_content(current$content$Inhalt$ID[[index]], ns)
                         )
                       )
                     }
                   )
                 })

                 # ganztag -----------------------------------------------------

                 output$Tabelle_ganztag_vielfalt <- renderDT({
                   datatable(
                     spark_data,
                     escape = FALSE,
                     rownames = FALSE,
                     options = list(
                       fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}'),
                       width = "100%",
                       dom = 'Bft',
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                       pageLength = 100,
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#195365', 'color': '#fff'});",
                         "this.api().columns().every(function () {",
                         "var column = this;",
                         "var input = $('<input type=\"text\" placeholder=\"Filter...\"/>')",
                         ".appendTo($(column.header()).empty())",
                         ".on('keyup change', function () {",
                         "  if (column.search() !== this.value) {",
                         "    column.search(this.value).draw();",
                         "  }",
                         "});",
                         "});",
                         "}"
                       ),
                       language = list(
                         url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json',
                         search = "Suche:"
                       )
                     )
                   ) %>% spk_add_deps()
                 })


               }
  )
}

#' Missing description
#' @noRd

draw_fragen_ziele_indikatoren <- function(content, input){
  #TODO Lösung ohne gsub
  input <- gsub("Fragen", "Frage", input)
  input <- gsub("Indikatoren", "Indikator", input)
  input <- gsub("Ziele", "Ziel", input)
  tags$ul(
    map2(
      unlist(content["ID"]),
      unlist(content$Inhalt[input]),
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

#' Missing description
#' @noRd

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

#' Missing description
#' @noRd

draw_information_box <- function(content){
  tagList(
    get_js("click_collapsible_header"),
    create_collapsible_panel(
      "Aktivitäten des Stifterverbandes",
      content$Aktivitaet$URL,
      content$Aktivitaet$Label,
      "aktivitaeten-box"
    ),
    create_collapsible_panel(
      "Datenbasis",
      content$Datenbasis$URL,
      content$Datenbasis$Label,
      "database-box"
    ),
    create_collapsible_panel(
      "weiterführende Links",
      content$Link$URL,
      content$Link$Label,
      "link-box"
    )
  )
}

#' Missing description
#' @noRd

draw_main_content <- function(content){
  tagList(
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
    br()
  )
}

#' Missing description
#' @noRd

create_collapsible_panel <- function(title, content_links, content_texts, class_suffix) {
  fluidRow(
    class = paste0(class_suffix, " monitor-sidbar-row"),
    width = 12,
    div(
      h4(
        class = "collapsible-header",
        title,
        tags$i(class = "arrow-down")
      ),
      div(
        class = "collapsible-content",
        tags$ul(
          map2(content_links, content_texts, ~ create_link_with_svg(.x, .y))
        )
      )
    )
  )
}

#' Missing description
#' @noRd

create_link_with_svg <- function(link_id, link_text) {
  tags$li(
    tags$a(
      class = "link",
      href = paste0("#", link_id),
      link_text,
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
}

#' Missing description
#' @noRD

draw_table_row_content <- function(indikator_ID, ns) {

  # bildung

  # ganztag

  if(indikator_ID == "ganztag_quantitativ") {
    fluidPage(
      p(
        "Aktueller Stand: Derzeit erfüllen ", tags$b("73,2 Prozent aller allgemeinbildeneden Schulen in
        Primar- und Sekundarbereich I"), " die ", span("KMK Definition für Ganztag", id = "tooltip_KMK_Def_Ganztag"),". Dabei gibt es jedoch
        nach Schulträgerschaft, Schulform, Ganztagsform und Bundesländern große
        Unterschiede. ", tags$i("Nutzen Sie die Filter, um die Sie interessierenden Zahlen zu erhalten")
      ),
      fluidRow(
        column(
          width = 3,
          selectInput(ns("Auswahl_ganztag_quantitativ_land"), "Land", choices = c("a", "b")),
          selectInput(ns("Auswahl_ganztag_quantitativ_schulform"), "Schulform", choices = c("a", "b")),
          selectInput(ns("Auswahl_ganztag_quantitativ_trägerschaft"), "Trägerschaft", choices = c("a", "b")),
          selectInput(ns("Auswahl_ganztag_quantitativ_ganztagsform"), "Ganztagsform", choices = c("a", "b"))
        ),
        column(
          width = 9,
          plotlyOutput(ns("Grafik_ganztag_quantitativ"))
        )
      ),
      p(
        "Unter Berücksichtigung der steigenden Zahl an Schülerinnen und Schülern
        gemäß KMK-Prognose, der derzeitigen Ausbaugeschwindigkeit sowie des
        bestehenden und prognostiziertem Lehrkräftemangel ist das Projekt Ausbau Ganztagsschule sowie
        die tatsächliche Erfüllung des Rechtsanspruchs Ganztagsschule im Primarbereich ab 2026 aktuell nicht wie gewünscht zu realisieren.
        Über ein Drittel der Grundschuleitungen gehen davon aus, dass eine Ganztagsbetreuung von der jeweiligen Kommune bis 2026/2027 nciht sichergestellt werden kann.
        Der Ganztagsschulausbau muss als gesamtgesellschaftliche Aufgabe begriffen und die Koperation von Schulen
        und zivilgesellschaftlichen Akteuren an dieser Stelle weiter vorangebracht werden."
      ),
      p("Anmerkungen:"),
      textOutput(ns("Anmerkungen_bildung_ganztag_quantitativ"))
    )
  } else if (indikator_ID == "ganztag_vielfalt"){
    fluidPage(
      p("Das Bildungsangebot in den Ganztagsschulen sollte möglichst vielfältig sein und sollt eim Idealfall die folgenden Angebotsbereiche umfassen:"),
      tags$ul(
        tags$li("lernunterstützende Angebote,"),
        tags$li("MINT-Angebote,"),
        tags$li("sprachliche und geisteswissenschaftliche Angebote,"),
        tags$li("musisch-kulturelle, lebenspraktische und berufsorientierende Angebote sowie"),
        tags$li("Angebote zu Freizeit, Bewegung, Gesundheit und sozialem Lernen. Die nachfolgende Tabelle zeigt nach Schularten wie viel")
      ),
      fluidRow(
        column(
          width = 3,
          radioButtons(ns("Auswahl_ganztag_vielfalt_absolut_prozent"), "Schulform", choices = c("a", "b"))
        ),
        column(
          width = 9,
          dataTableOutput(ns("Tabelle_ganztag_vielfalt"))
        )
      ),
      p("Anmerkungen"),
      textOutput(ns("Anmerkungen_bildung_ganztag_vielfalt"))
    )
  } else if (indikator_ID == "ganztag_kooperation"){
    fluidPage(
      p("Zur Gestaltung unterrichtsbezogener Angebote kooperieren gemäß Schulleitungsbefragung ",
        tags$b("2023"), " der Telekom Stiftung ", tags$b("86 Prozent"), " der allgemeinbildenden öffentlichen Schulen
        mit außerschulischen Lernorten und/oder Bildungspartnern. Schulen kooperieren vor allem mit
        Sportvereinen, Bibliotheken, Musikschulen sowie Einrichtungen der Kinder- und Jugendarbeit."),
      p("Die Kooperation findet dabei im allgemeinen (70 Prozent) auf Basis eines gemeinsam formulierten
        Pädagogischen Konzept statt."),
      fluidRow(
        column(
          width = 12,
          plotlyOutput(ns("Grafik_ganztag_kooperation"))
        ),
        p("Anmerkungen"),
        textOutput(ns("Anmerkungen_bildung_ganztag_kooperation"))
      )
    )
  } else if (indikator_ID == "ganztag_multiprofessionel") {
    fluidPage(
      p("Bei der Umsetzung von Ganztag können und sollten multiprofessionelle Teams an Schulen eingesetzt werden.
        Oftmals arbeiten an Schulen - neben Gebäudemanagement und Sekretariat - jedoch ausschließlich Lehrkräfte.
        Unterstützungsstrukturen etwa durch IT-, und Verwaltungsfachkräfte und/oder Sozial-, Medien-, Kulturpädagoginnen
        und -pädagogen sowie Projektmanagement etc. fehlen. Konkrete Zahlen liefert zu diesem Befund eine Schulleitungsbefragung
        zu Multiprofessionalität an allgemeinbildenden Schulen, dargestellt in der folgenden Tabelle:"
      ),
      fluidRow(
        column(
          width = 12,
          dataTableOutput(ns("Tabelle_ganztag_multiprofessionel"))
        )
      ),
      p("Anmerkungen"),
      textOutput(ns("Anmerkungen_bildung_ganztag_multiprofessionel"))
    )
  } else if (indikator_ID == "ganztag_lage") {
    fluidPage(
      p(
        "Wie bewerten die Schulleitungen ganz allgemein die Lage an den Schulen? Flapsig: Wie ist die Stimmung?
        Dieser Frage nährt sich die Schulleitungsbefragung des Verband Bildung und Erziehung (VBE)
        mit den Fragen danach, inwieweit die Schulleitungen mit die an sie gestellten Aufgaben zu ihrer eigenen
        Zufriedenheit erfüllen können und ob sie eine Weiterempfehlung für den Beruf abgeben."
      ),
      fluidRow(
        column(
          width = 3,
          radioButtons(ns("Auswahl_ganztag_lage_frage"), "Auswahl:", choices = c("a", "b"))
        ),
        column(
          width = 9,
          plotlyOutput(ns("Grafik_ganztag_lage"))
        )
      ),
      p(
        "Die Ergebnisse: Schulleitungen geben über die Zeit immer seltener an, dass Sie die Aufgaben zu Ihrer eigenen Zufriedenheit immer oder häufig erfüllen können - zuletzt sagten dies nur noch 57 Prozent.
        Auch die positive Weiterempfehlung des Berufs geht weiter zurück, zuletzt auf XX Prozent (Wahrscheinliche Empfehlung/auf jeden Fall)"
      ),
      p("Anmerkungen"),
      textOutput(ns("Anmerkungen_bildung_ganztag_lage"))
    )
  }
}
