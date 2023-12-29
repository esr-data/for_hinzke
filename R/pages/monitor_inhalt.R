
box::use(
  ../../R/utils/routing[get_hf_param, add_param_in_url, recode_parameter],
  ../../R/utils/database[get_query, load_table_by_variable],
  ../../R/utils/js[get_js],
  ../../R/utils/monitor[get_content_monitor],
  shiny[
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
  shinycssloaders[withSpinner],
  dplyr[rename],
  purrr[map2],
  DT[datatable, JS]
)
#TODO irrelevanten Verknüpfungen aussortieren!

# Global Variables

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
            uiOutput(ns("select_variable_top")),
            fluidPage(
              class = "monitor-variable-content",
              column(
                width = 3,
                pickerInput(
                  inputId = ns("gliederung"),
                  label = "Wähle eine Gliederung",
                  choices = c(),
                  multiple = FALSE
                ),
                pickerInput(
                  inputId = ns("kategorie"),
                  label = "Filtern",
                  choices = c(),
                  options  = list(
                    `actions-box`        = TRUE,
                    `none-selected-text` = "nichts ausgewählt",
                    `select-all-text`    = "alle auswählen",
                    `deselect-all-text`  = "nichts auswählen",
                    `live-search`        = TRUE
                  ),
                  multiple = TRUE
                ),
                radioButtons(
                  inputId  = ns("darstellung"),
                  label    = "Darstellungsweise:",
                  choices  = c("Zeitverlauf", "Tabelle"),
                  selected = "Zeitverlauf"
                )
              ),
              column(
                width = 9,
                uiOutput(ns("output_variable"))
              )
            ),
            uiOutput(ns("select_variable_bottom"))
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
          filter    = c(),
          buttonset = "a",
          parameter = list(
            hf  = "",
            tp  = "",
            fr  = "",
            ind = "",
            gl  = "",
            kat = "",
            da  = ""
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
                current$content <- content_monitor[[param_tp]]
                output$title    <- renderUI({h1(class = "monitor-subpage-title-headline", current$content[["Titel"]])})
                output$information_box <- renderUI({draw_information_box(current$content)})
                output$main_panel <- renderUI({draw_main_content(current$content)})
                output$select_variable <-
                  renderUI({
                    pickerInput(
                      inputId = ns("picker_content"),
                      choices = current$content$Titel,
                      multiple = FALSE
                    )
                  })
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

              if (param_ind != current$parameter$ind){
                current$parameter$ind <- param_ind
                change_in_choice <- TRUE

                variable_ueberschriften <- current$content$Inhalt$Titel

                current$variable <- load_table_by_variable(138)

                updatePickerInput(
                  session  = session,
                  inputId  = "gliederung",
                  choices  = c("---", current$variable$gruppe),
                  selected = c("---")
                )

                create_var_buttons <- function(x, buttonset, choosed){
                  div(
                    class = "monitor-variable-button-box",
                    tagList(
                      lapply(
                        x,
                        \(y){
                          actionButton(
                            ns(paste0("button_variable_", buttonset, y)),
                            class =
                              paste(
                                "monitor-variable-button",
                                ifelse(choosed == y, "monitor-variable-button-choosed", "")
                              ),
                            label = variable_ueberschriften[y]
                          )
                        }
                      )
                    )
                  )
                }

                max_variables <- length(variable_ueberschriften)
                if (current$buttonset == "a"){
                  current$buttonset <- "b"
                } else {
                  current$buttonset <- "a"
                }
                output$select_variable_top <- renderUI({HTML("")})
                output$select_variable_bottom <- renderUI({HTML("")})

                if (param_ind > 0 & length(max_variables) > 0){
                  output$select_variable_top <-
                    renderUI({
                      create_var_buttons(1:(param_ind), current$buttonset, param_ind)
                    })
                }

                if (param_ind < max_variables & length(max_variables) > 0){
                  output$select_variable_bottom <-
                    renderUI({
                      create_var_buttons((param_ind + 1):max_variables, current$buttonset, param_ind)
                    })
                }


              }

              # PARAMETER gl ---------------------------------------------------------------------
              param_gl <-
                parameter$gl |>
                recode_parameter(
                  is_integer = TRUE,
                  value_set  = 1:length(current$variable$gruppe)
                )

              if (param_gl != current$parameter$gl){
                current$parameter$gl <- param_gl
                change_in_choice <- TRUE

                if (param_gl != ""){
                  selected_choice <- current$variable$gruppe[param_gl]

                  unique_values <-
                    current$variable$daten[,current$variable$gruppe[param_gl]] |>
                    unique() |>
                    sort()

                  unique_values <- unique_values[unique_values != "Insgesamt"]
                  current$filter <- unique_values
                  selected_values <- NULL
                  if(length(unique_values) < 6) {
                    selected_values <- unique_values
                  } else {
                    if (current$variable$gruppe[param_gl] %in% "Räumliche Gebiete") {
                      selected_values <- c("Deutschland", "Bayern", "Baden-Württemberg", "Nordrhein-Westfalen")
                    } else {
                      selected_values <- unique_values[1:5]
                    }
                  }
                  updatePickerInput(
                    session = session,
                    inputId = "kategorie",
                    choices = unique_values,
                    selected = selected_values
                  )

                } else {
                  selected_choice <- "---"
                  updatePickerInput(
                    session = session,
                    inputId = "kategorie",
                    choices = c("---"),
                    selected = c("---")
                  )

                }

                updatePickerInput(
                  session  = session,
                  inputId  = "gliederung",
                  selected = selected_choice
                )
              }

              # PARAMETER kat --------------------------------------------------------------------
              param_kat <-
                parameter$kat  |>
                recode_parameter(
                  is_integer = TRUE,
                  is_vector  = TRUE,
                  value_set  = 1:length(current$filter)
                )

              if (
                !(all(param_kat %in% current$parameter$kat) &
                  all(current$parameter$kat %in% param_kat))
              ){
                current$parameter$kat <- param_kat
                change_in_choice <- TRUE
                if (all(param_kat != "")){
                  updatePickerInput(
                    session = session,
                    inputId = "kategorie",
                    selected = current$filter[param_kat]
                  )
                }

              }

              # PARAMETER da -------------------------------------------------------------------------
              param_da <-
                parameter$da  |>
                recode_parameter(
                  is_integer = FALSE,
                  is_vector  = TRUE,
                  value_set  = substr(darstellungen$a, 1, 2)
                )
              if (param_da == ""){
                param_da <-
                  darstellungen$a[1] |>
                  substr(1, 2)
              }

              if (param_da != current$parameter$da){
                current$parameter$da <- param_da
                change_in_choice <- TRUE
                updateRadioButtons(
                  inputId = "darstellung",
                  selected = darstellungen$a[match(param_da, substr(darstellungen$a, 1, 2))]
                )
                # TODO Update Picker
              }
            }

            if (change_in_choice){

              output$output_variable <-
                renderUI({
                  draw_indikator_content(
                    variable    = current$variable,
                    darstellung = input$darstellung,
                    gliederung  = input$gliederung,
                    kategorie   = input$kategorie
                  )
                })

              # output$output_variable <-
              #   renderUI({
              #     HTML(paste(current$parameter$gl, paste(current$parameter$kat, collapse = ","), current$parameter$da, sep = "<br>"))
              #   })
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

      observeEvent(
        input$gliederung, {
          if (!is.null(current$content) & !is.null(current$variable)){
            if (get_page() == "monitor_inhalt"){
              input_gliederung <- match(input$gliederung, current$variable$gruppe, NA)
              if (any(is.na(input_gliederung))){
                input_gliederung <- ""
              }
              if (all(!is.na(input_gliederung))){
                current_url <- session$clientData$url_hash
                new_url <-
                  add_param_in_url(
                    current_url  = current_url,
                    current_page = "monitor_inhalt",
                    parameter    = "gl",
                    value        = input_gliederung,
                    old_value    = get_query_param("gl")
                  )
                if (new_url != current_url){
                  change_page(new_url)
                }
              }
            }
          }
        }
      )

      observeEvent(
        input$kategorie, {
          if (!is.null(current$content) & !is.null(current$variable)){
            if (get_page() == "monitor_inhalt"){

              input_kategorie <- input$kategorie
              param_gl <- suppressWarnings(as.numeric(get_query_param()$gl))

              if (length(input_kategorie) > 0 & length(current$variable$gruppe) > 0 & !is.null(param_gl)){
                if (all(!is.na(param_gl)) & all(param_gl > 0) & length(param_gl) == 1){

                  unique_values <-
                    current$variable$daten[,current$variable$gruppe[param_gl]] |>
                    unique() |>
                    sort()
                  unique_values <- unique_values[unique_values != "Insgesamt"]
                  unique_values <- match(input_kategorie, input_kategorie)
                  unique_values <- unique_values[!is.na(unique_values)]


                  if (length(unique_values) == 0){
                    unique_values <- 0
                  }

                  unique_values <- paste(unique_values, collapse = ",")
                  current_url <- session$clientData$url_hash
                  new_url <-
                    add_param_in_url(
                      current_url  = current_url,
                      current_page = "monitor_inhalt",
                      parameter    = "kat",
                      value        = unique_values,
                      old_value    = get_query_param("kat")
                    )
                  if (new_url != current_url){
                    change_page(new_url)
                  }
                }
              }

            }
          }
        }
      )

      observeEvent(
        input$darstellung, {
          if (!is.null(current$content) & !is.null(current$variable)){
            if (get_page() == "monitor_inhalt"){
              input_darstellung <- input$darstellung
              if (any(is.na(input_darstellung)) | length(input_darstellung) != 1){
                input_darstellung <-
                  darstellungen$a[1] |>
                  substr(1, 2)
              } else {
                input_darstellung <-
                  input_darstellung |>
                  substr(1, 2)
              }
              current_url <- session$clientData$url_hash
              new_url <-
                add_param_in_url(
                  current_url  = current_url,
                  current_page = "monitor_inhalt",
                  parameter    = "da",
                  value        = input_darstellung,
                  old_value    = get_query_param("da")
                )
              if (new_url != current_url){
                change_page(new_url)
              }

            }
          }

        }
      )

      eval(
        parse(
          text =
            lapply(
              unlist(lapply(1:MAX_VARIABLEN, \(z) paste0(c("a", "b"), z))),
              \(x){
                'observeEvent(
                        input$button_variable_ERSETZEN, {
                          if (!is.null(current$content) & !is.null(current$variable)){
                          neue_ind <- substr("ERSETZEN", 2, 2)
                            if (get_page() == "monitor_inhalt"){
                              if (get_query_param("ind") == neue_ind){
                                if (neue_ind > 1){
                                  neue_ind <- as.numeric(neue_ind) - 1
                                } else {
                                  neue_ind <- 2
                                }
                              }
                              current_url <- session$clientData$url_hash
                              new_url <-
                                add_param_in_url(
                                  current_url  = current_url,
                                  current_page = "monitor_inhalt",
                                  parameter    = "ind",
                                  value        = neue_ind,
                                  old_value    = get_query_param("ind")
                                )
                              if (new_url != current_url){
                                change_page(new_url)
                              }
                            }
                          }
                        }
                      )' |>
                  gsub(pattern = "ERSETZEN", replacement = x)
              }
            ) |>
            unlist() |>
            paste(collapse = "\n")
        )
      )



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


draw_indikator_content <- function(variable, darstellung, gliederung, kategorie){

  if (is.null(variable)) return(HTML(""))
  if (is.null(darstellung)) return(HTML(""))
  if (is.null(gliederung)) gliederung <- "---"

  daten       <- variable$daten
  daten$id    <- NULL
  print(gliederung)
  gruppen     <- variable$gruppe[!(variable$gruppe %in% gliederung)]

  if (gliederung %in% "---"){

  }

  gliederung2 <- variable$gruppe[variable$gruppe %in% gliederung]
  if (length(gliederung) == 0){
    kategorie <- c()
  }

  for (i in gruppen){
    daten <- daten[daten[,i] %in% c("Insgesamt", "Deutschland"),]
    daten[,i] <- NULL
  }

  if (length(kategorie) > 0){
    daten <- daten[daten[,gliederung2] %in% kategorie,]
  }


  daten <- daten[,!(names(daten) %in% variable$gruppe) | names(daten) == gliederung]

  # if (!is.null(gliederung)){
  #   if (length(gliederung) > 0){
  #     if (gliederung != "---"){
  #
  #       gliederung_angepasst <- TRUE
  #
  #     }
  #   }
  # }

  daten[,gliederung] %in% kategorie

  if (darstellung == "Tabelle") {

    return(
      datatable(
        daten,
        extensions = 'Buttons',
        rownames   = FALSE,
        options = list(
          dom = 'lBfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 20,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#185365', 'color': '#fff'});",
            "}"
          ),
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json')
        )
      )
    )
  } else if (darstellung == "Zeitverlauf"){
    # daten$gruppe <- daten[,gliederung2]
    # daten$wert <- as.numeric(daten$wert)
    # config(
    #   toImageButtonOptions = list(filename = str_c("SV Monitor Wissenschaft")),
    #   modeBarButtonsToRemove = c("lasso2d", "zoomIn2d", "zoomOut2d"),
    #   locale = "de",
    #   displaylogo = FALSE,
    #   p = ggplotly(
    #     ggplot(daten, aes(x = Zeit, y = wert, group = gruppe)) +
    #       geom_line() +
    #       suppressWarnings(geom_point(aes(
    #         text = paste0("Jahr: ", Zeit, "<br>Wert: ",
    #                       prettyNum(round(wert, 1), big.mark = ".", decimal.mark = ","))), color = "#195365")) +
    #       theme_pubr() +
    #       theme(plot.title = element_text(hjust = -0.45, vjust = 2.12),
    #             plot.margin = unit(c(1, 0.5, 1, 1), "cm"),
    #             axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
    #             axis.text.x = element_text(angle = 45, hjust = 1)
    #       ) +
    #       labs(
    #         x = "",
    #         y = paste0(daten$Messeinheit[1], "\n"),
    #         title = paste0("", "\n")
    #       ) +
    #       scale_x_continuous(breaks = c(
    #         min(daten$Zeit):max(daten$Zeit)
    #       )) +
    #       scale_color_manual(values = "#195365")
    #   )
    # ) %>%
    #   layout(
    #     annotations =
    #       list(
    #         x = 1,
    #         y = -0.3,
    #         text = paste0(
    #           "Quelle: "
    #         ),
    #         showarrow = F,
    #         xref = 'paper',
    #         yref = 'paper',
    #         xanchor = 'right',
    #         yanchor = 'auto',
    #         xshift = 0,
    #         yshift = 0,
    #         font = list(size = 10, color = "black")
    #       )
    #   )

  }

  return(HTML(""))
}

