#' Necessary Packages/Functions

box::use(
  ../../R/utils/database[get_query, get_sql, load_table_by_variable],
  ../../R/utils/routing[add_param_in_url],
  shiny[
    NS, moduleServer, observeEvent,
    uiOutput, renderUI,
    fluidPage, fluidRow,
    HTML, tagList, div, h2,
    reactiveVal, reactiveValues
  ],
  shinyWidgets[pickerInput, updatePickerInput],
  shiny.router[get_page, get_query_param, change_page],
  shinycssloaders[withSpinner],
  reactable[reactable, reactableOutput, renderReactable, colDef],
  sortable[bucket_list, add_rank_list]
)

#' Missing description
#' @export

module_indikator_ui <- function(id = "indikator", label = "m_indikator", type = "all") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      h2("Explorer - Auswahl an Indikatoren"),
      fluidRow(
        style = "padding: 10px; display: flex; margin: 0;",
        div(
          class = "content-box",
          style = "width: 100%;",
          div(
            style = "padding: 10px; display: flex; flex-direction: row; flex-wrap: wrap;",
            pickerInput(
              inputId = ns("select_variable"),
              label = "Liste der Variablen",
              choices = c(""),
              options  = list(
                `actions-box`        = FALSE,
                `none-selected-text` = "nichts ausgewählt",
                `select-all-text`    = "alle auswählen",
                `deselect-all-text`  = "nichts auswählen",
                `live-search`        = TRUE,
                `max-options`        = 1
              ),
              multiple = TRUE
            ),
            pickerInput(
              inputId = ns("select_tag"),
              label = "Themenbereiche",
              choices = c(""),
              options  = list(
                `actions-box`        = TRUE,
                `none-selected-text` = "nichts ausgewählt",
                `select-all-text`    = "alle auswählen",
                `deselect-all-text`  = "nichts auswählen",
                `live-search`        = TRUE
              ),
              multiple = TRUE
            )
          ),
          uiOutput(ns("variable")),
          div(
            style = "display: flex; flex-wrap: wrap;",
            div(uiOutput(ns("select_reichweite")), style = "max-width: 650px; width: 80%;"),
            div(uiOutput(ns("filter_reichweite"),  style = "max-width: 250px; width: 100%"))
          )

        )
      ),
      withSpinner(reactableOutput(ns("table")))
    )
  )
}

#' Missing description
#' @export

module_indikator_server <- function(id = "indikator", type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # gesperrt wird verwendet, um die Aktualisierung im Rahmen von observe/observeEvents temporär zu verhindern
      gesperrt  <- reactiveVal(FALSE)

      daten <-
        reactiveValues(
          werte   = data.frame(),
          gruppen = c(),
          auswahl = c(),
          filter  = data.frame(type = c(), werte = c())[0,]
        )

      input_var <- reactiveVal(data.frame())
      input_tag <- reactiveVal(data.frame())

      filter_param <- indikator_translate_filter_param()

      # URL-Parameter werden zwischengespeichert, um den Verlauf/eine Aktualisierung besser nachzuvollziehen
      # Jeder mögliche URL-Parameter hat einen Eintrag in der reaktiven Liste und "" steht für NULL/Kein Eintrag
      eval(
        parse(
          text =
            paste0(
              "parameter <- reactiveValues(",
              paste(
                paste(c("in_gp", "in_tg", "hf", "in_vr", filter_param$param), "= ''"),
                collapse = ","
              ),
              ")"
            )
        )
      )

      # Das zentrale Event bezieht sich auf die URL-Parameter; auf dieser Basis werden alle Anpassungen vorgenommen
      observeEvent(
        get_query_param(), {
          if (!gesperrt()){
            if (get_page() == "indikator"){

              if (is.null(get_query_param())){
                change_page("indikator?hf=0")
              } else {
                gesperrt(TRUE)
                aenderung <- FALSE

                # PARAMETER HF - Update des Handlungsfeldes / der Themen Auswahl __________________

                param_hf <-
                  get_query_param("hf") |>
                  indikator_recode_param_int()
                if (param_hf == ""){
                  param_hf <- 0
                }

                if (param_hf != parameter$hf){
                  parameter$hf <- param_hf

                  if (param_hf == 0){
                    input_var(indikator_get_variables()) #TODO
                    input_tag(get_sql("indikator_get_tag_by_variable", TRUE))
                    updatePickerInput(session, "select_tag",      choices = sort(input_tag()$beschr))
                    updatePickerInput(session, "select_variable", choices = sort(input_var()$beschr[input_var()$relevant]))
                    aenderung <- TRUE
                  }

                }

                # PARAMETER IN_TG - Update der Tags __________________________________________________

                param_in_tg <-
                  get_query_param("in_tg") |>
                  indikator_recode_param_int(vec = TRUE)

                # Nur Änderungen vornehmen, wenn sich etwas geändert hat
                if (!(all(param_in_tg %in% parameter$in_tg) &
                      all(parameter$in_tg %in% param_in_tg))){

                  parameter$in_tg <- param_in_tg
                  selected_tags   <- input_tag()$beschr[input_tag()$id %in% param_in_tg]

                  # Picker updaten, wenn ich die ausgewählten Items unterscheiden:
                  # Das ist der Fall, wenn die Paramter-Eingabe über die URL und nicht den Picker erfolgt
                  if (!(all(selected_tags %in% input$select_tag) &
                        all(input$select_tag %in% selected_tags))){
                    updatePickerInput(session, "select_tag", selected = input_tag()$beschr[input_tag()$id %in% param_in_tg])
                  }

                  # Aktualisierung der Variablen aus Basis der Tags
                  input_var(indikator_get_variables_by_tags(input_var(), selected_tags))
                  updatePickerInput(
                    session = session,
                    inputId = "select_variable",
                    selected = input$select_variable,
                    choices = sort(input_var()$beschr[input_var()$relevant])
                  )
                  aenderung <- TRUE
                }

                # PARAMETER IN_VR - Update der Variablen _____________________________________________

                param_in_vr <-
                  get_query_param("in_vr") |>
                  indikator_recode_param_int()

                if (param_in_vr != parameter$in_vr){
                  parameter$in_vr <- param_in_vr

                  if (param_in_vr == ""){

                    daten$filter  <- data.frame()
                    daten$gruppen <- c()
                    daten$werte   <- data.frame()
                    daten$auswahl <- c()
                    updatePickerInput(session, "select_variable", selected = NULL)
                    output$filter_reichweite <- renderUI({HTML("")})
                    output$select_reichweite <- renderUI({HTML("")})

                  } else {

                    variable <- get_query(paste0("SELECT id, beschr FROM variable WHERE id =", param_in_vr))

                    if (nrow(variable) == 1){

                      tabelle <- load_table_by_variable(variable$id)

                      daten$filter  <- data.frame()
                      daten$gruppen <- sort(c(tabelle$gruppe, "Zeit"))
                      daten$werte   <- tabelle$daten
                      daten$auswahl <- c()

                      output$filter_reichweite <- renderUI({HTML("")})
                      output$select_reichweite <- renderUI({indikator_draw_eimer(daten$gruppen, ns = ns)})
                      updatePickerInput(session, "select_variable", selected = variable$beschr[1])

                    }

                  }
                  aenderung <- TRUE
                }

                # PARAMETER IN_GP - Update der Gruppenauswahl / Eimer ________________________________

                param_in_gp <-
                  get_query_param("in_gp") |>
                  indikator_recode_param_int(vec = TRUE)

                if (!(all(param_in_gp %in% parameter$in_gp) &
                      all(parameter$in_gp %in% param_in_gp))){

                  parameter$in_gp <- param_in_gp
                  daten_gruppe <- 1:length(daten$gruppen)
                  daten_gruppe <- daten_gruppe[daten_gruppe %in% param_in_gp]

                  if (length(daten_gruppe) > 0){
                    daten$auswahl <- daten$gruppen[daten_gruppe]
                  } else {
                    daten$auswahl <- c()
                  }

                  if (length(daten$auswahl) > 0){

                    # Änderung, die nicht im UI-Element persistiert ist?
                    eimer_aenderung <- FALSE
                    if (length(input$eimer_unterscheiden) != length(daten$auswahl)){
                      eimer_aenderung <- TRUE
                    } else if (length(daten$auswahl) > 0){
                      if (!(all(daten$auswahl %in% input$eimer_unterscheiden) &
                            all(input$eimer_unterscheiden %in% daten$auswahl))){
                        eimer_aenderung <- TRUE
                      }
                    }
                    if (eimer_aenderung){
                      output$select_reichweite <-
                        renderUI({
                          indikator_draw_eimer(daten$gruppen, daten$auswahl, ns)
                        })
                    }

                    # Filter aktualisieren
                    output$filter_reichweite <-
                      renderUI({
                        indikator_draw_filter(daten$werte, daten$auswahl, ns)
                      })

                  }


                }

                # PARAMETER der Filter _______________________________________________________________

                for (filter_id in filter_param$param){
                  # in_re15
                  #filter_id <- "in_re15"
                  param_filter <-
                    get_query_param(filter_id) |>
                    indikator_recode_param_int(vec = TRUE)

                  "test <- !(all(param_filter %in% parameter$ERSETZEN) &  all(parameter$ERSETZEN %in% param_filter))" |>
                    gsub(pattern = "ERSETZEN", replacement = filter_id) |>
                    parse(text = _) |>
                    eval()

                  if (test){

                    "parameter$ERSETZEN <- param_filter" |>
                      gsub(pattern = "ERSETZEN", replacement = filter_id) |>
                      parse(text = _) |>
                      eval()

                    eval(parse(text = paste0("input_filter <- input$", filter_param$bez[filter_param$param == filter_id])))
                    auswahl <-
                      daten$werte[,filter_param$beschr[filter_param$param == filter_id]] |>
                      unique() |>
                      sort()
                    auswahl <- auswahl[param_filter]

                    update_filter <-
                      "updatePickerInput(session, '%s', selected = auswahl)" |>
                      sprintf(fmt = _, filter_param$bez[filter_param$param == filter_id]) |>
                      parse(text = _)

                    if (length(input_filter) != length(auswahl)){
                      eval(update_filter)
                    } else if (length(input_filter) > 0){
                      if (!(all(input_filter %in% auswahl) &
                            all(auswahl %in% input_filter))){
                        eval(update_filter)
                      }
                    }

                    daten_filter <- daten$filter
                    daten_filter <- daten_filter[daten_filter$type != filter_param$beschr[filter_param$param == filter_id],]

                    if (length(auswahl) > 0){
                      daten_filter <-
                        rbind(
                          daten_filter,
                          data.frame(
                            type  = filter_param$beschr[filter_param$param == filter_id],
                            werte = auswahl
                          )
                        )
                    }

                    daten$filter <- daten_filter[!is.na(daten_filter$werte),]
                    rm(update_filter, auswahl, param_filter, daten_filter)

                  }
                  rm(test)
                }

                # Am Ende nochmal die Tabelle aktualisieren __________________________________________

                if (aenderung){
                  output$table <-
                    renderReactable({
                      indikator_draw_reactable(
                        manage_explorer_data( # TODO
                          werte         = daten$werte,
                          gruppe        = daten$gruppen[daten$gruppen != "Zeit"],
                          unterscheiden = daten$auswahl,
                          filtern       = daten$filter
                        )
                      )
                    })
                }
              }

            }
          }
          gesperrt(FALSE)
        },
        ignoreNULL = FALSE
      )

      observeEvent(
        input$select_tag, {

          if (!gesperrt() & !is.null(get_query_param("hf"))){
            input_select_tag <- input$select_tag
            indikator_tags   <- input_tag()
            new_value        <- ""
            current_url      <- session$clientData$url_hash

            if (!is.null(input_select_tag)){
              if (nrow(indikator_tags) > 0){
                new_value <-
                  paste(
                    sort(indikator_tags$id[indikator_tags$beschr %in% input_select_tag]),
                    collapse = ","
                  )
              }
            }

            new_url <-
              add_param_in_url(
                current_url  = current_url,
                current_page = "indikator",
                parameter    = "in_tg",
                value        = new_value,
                old_value    = get_query_param("in_tg")
              )

            if (new_url != current_url){
              # print(paste("tag", 1)) # BENCH-PRINT
              change_page(new_url)
            }
          }
        },
        ignoreNULL = FALSE
      )

      observeEvent(
        input$select_variable, {

          if (!gesperrt() & !is.null(get_query_param("hf"))){
            input_select_var   <- input$select_variable
            new_value          <- ""
            current_url        <- session$clientData$url_hash
            indikator_variable <- input_var()

            if (!is.null(input_select_var) & nrow(indikator_variable) > 0){
              new_value <- (indikator_variable$id[indikator_variable$beschr %in% input_select_var])[1]
            }

            new_url <-
              add_param_in_url(
                current_url  = current_url,
                current_page = "indikator",
                parameter    = "in_vr",
                value        = new_value,
                old_value    = get_query_param("in_vr")
              )

            if (new_url != current_url){
              # print(paste("var", 1)) # BENCH-PRINT
              change_page(new_url)
            }
          }
        },
        ignoreNULL = FALSE
      )

      observeEvent(
        input$eimer_unterscheiden, {

          if (!gesperrt() & !is.null(get_query_param("hf"))){

            if (length(daten$gruppen) > 0){

              current_url <- session$clientData$url_hash
              new_value   <- (1:length(daten$gruppen))[daten$gruppen %in% input$eimer_unterscheiden]
              if (length(new_value) == 0){
                new_value <- ""
              } else {
                new_value <- paste(new_value, collapse = ",")
              }

              new_url <-
                add_param_in_url(
                  current_url  = current_url,
                  current_page = "indikator",
                  parameter    = "in_gp",
                  value        = new_value,
                  old_value    = get_query_param("in_gp")
                )

              if (new_url != current_url){
                change_page(new_url)
              }
            }
          }

        }
      )

      for (i in write_explorer_filter_observer(filter_param$bez)){
        eval(parse(text = i))
      }

    }
  )
}

#' Missing description
#' @noRd

indikator_draw_reactable <- function(daten){

  if (is.null(daten))  return(reactable(data.frame(keine = "daten")))
  if (nrow(daten) < 1) return(reactable(data.frame(keine = "daten")))

  reactable(
    daten,
    showSortable = TRUE,
    searchable   = TRUE,
    filterable   = TRUE,
    highlight    = TRUE,
    defaultColDef = colDef(minWidth = 200, align = "center"),
    columns = list(
      Zeit    = colDef(name = "Zeit (Jahr)", minWidth = 75),
      wert    = colDef(
        name  = "Wert",
        width = 100,
        align = "right",
        cell  = function(value) ifelse(suppressWarnings(!is.na(as.numeric(value))), format(as.numeric(value), big.mark = ".", decimal.mark = ","), value)
      ),
      einheit  = colDef(name = "Einheit",  minWidth = 100, align = "left")
    )
  )
}

#' Missing description
#' @noRd

indikator_draw_eimer <- function(reichweiten = list(), selected = NULL, ns){

  if (is.null(reichweiten)){
    return(HTML(""))
  }

  if (length(reichweiten) < 1){
    return(HTML(""))
  }

  if (!is.null(selected)){
    reichweiten <- reichweiten[!(reichweiten %in% selected)]
  }

  bucket_list(
    header      = NULL,
    group_name  = "eimer_reichweiten",
    orientation = "horizontal",
    add_rank_list(
      text      = "Ignoriert",
      labels    = reichweiten,
      input_id  = ns("eimer_ignorieren")
    ),
    add_rank_list(
      text      = "Unterscheiden",
      labels    = selected,
      input_id  = ns("eimer_unterscheiden")
    )
  )
}

#' Missing description
#' @noRd

manage_explorer_data <- function(werte, gruppe = NULL, unterscheiden = NULL, filtern = NULL){

  if (nrow(werte) < 1) return(NULL)

  if (is.null(gruppe)){
    gruppe <- c()
  }

  neue_gruppe <- gruppe
  if (is.null(unterscheiden)){
    unterscheiden <- c()
  }

  nicht_unterscheiden <- gruppe[!(gruppe %in% unterscheiden)]

  if (!is.null(filtern)){
    if (nrow(filtern) > 0){
      for (i in unique(filtern$type)){
        werte <- werte[werte[,match(i, names(werte))] %in% filtern[filtern$type == i,"werte"],]
      }
    }
  }

  for (i in nicht_unterscheiden){
    if ("Insgesamt" %in% werte[,i]) {
      werte <- werte[werte[,i] == "Insgesamt", names(werte) != i]
    } else if ("Deutschland" %in% werte[,i]) {
      werte <- werte[werte[,i] == "Deutschland", names(werte) != i]
    } else {
      x <- table(werte[,i])
      x <- (names(x)[max(x) == x])[1]
      werte <- werte[werte[,i] == x, names(werte) != i]
      rm(x)
    }
  }

  if (!("Zeit" %in% unterscheiden)){
    werte <- werte[werte$Zeit == max(werte$Zeit),]
  }

  neue_gruppe <- gruppe[!(gruppe %in% nicht_unterscheiden)]
  return(werte[,c("Zeit", "wert", "einheit", neue_gruppe)])
}

#' Missing description
#' @noRd

write_explorer_filter_observer <- function(filter_param_bez){

  gsub_name <- function(x){gsub("[^a-z]", "", tolower(x))}

  write_code <- function(x){
    "observeEvent(input$ERSETZEN, {
      if (!gesperrt() & !is.null(get_query_param('hf'))){

        param_attr    <- filter_param[filter_param$bez == 'ERSETZEN',]
        auspraegungen <- sort(unique(daten$werte[,param_attr$beschr[1]]))
        auspraegungen <- (1:length(auspraegungen))[auspraegungen %in% input$ERSETZEN]
        current_url   <- session$clientData$url_hash

        new_url <-
          add_param_in_url(
            current_url  = current_url,
            current_page = 'indikator',
            parameter    = param_attr$param[1],
            value        = paste(auspraegungen, collapse = ','),
            old_value    = get_query_param(param_attr$param[1])
          )
        if (new_url != current_url){
          change_page(new_url)
        }
      }
    }, ignoreNULL = FALSE
    )" |>
      gsub(
        pattern = "ERSETZEN",
        replacement = x
      )
  }

  return(sapply(filter_param_bez, write_code))
}

#' Missing description
#' @noRd

indikator_get_variables <- function(con){
  output <- get_query("SELECT id, beschr FROM variable")
  output$relevant <- TRUE
  return(output)
}

#' Missing description
#' @noRd

indikator_recode_url_parameter <- function(x){
  if (is.null(x)) return("")
  if (is.na(x))   return("")
  return(x)
}

#' Missing description
#' @noRd

indikator_ini_param_vr <- function(x){
  if (is.null(x)) return("")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("")
  return(x)
}

#' Missing description
#' @noRd

indikator_ini_param_tg <- function(x){
  if (is.null(x)) return("")
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x)) return("")
  return(x)
}

#' Missing description
#' @noRd

#' Missing description
#' @noRd

indikator_ini_param_hnd <- function(x){
  if (is.null(x)) return("")
  x <-
    suppressWarnings(
      as.numeric(strsplit(x, ",")[[1]])
    )
  if (any(is.na(x))) return("")
  return(x)
}

# ---- ab hier neu

#' Missing description
#' @noRd

indikator_recode_param_int <- function(x, vec = FALSE){
  if (is.null(x)) return("")
  if (vec) x <- strsplit(x, ",")[[1]]
  x <- suppressWarnings(as.integer(x))
  if (any(is.na(x))) return("")
  return(sort(x))
}

#' Missing description
#' @noRd

indikator_get_variables_by_tags <- function(variable, selected_tags){

  if (length(selected_tags) == 0){
    variable$relevant <- TRUE
    return(variable)
  }

  tag_link <-
    get_query(
      "SELECT reihe_id, tags
       FROM view_tag_link_aggregated_beschr
       WHERE tabelle_id = (SELECT id FROM tabelle WHERE bez = 'variable')"
    )
  tag_link <- tag_link[tag_link$reihe_id %in% variable$id,]
  variable_id <-
    tag_link$reihe_id[
      apply(sapply(selected_tags, \(x) grepl(x, tag_link$tags)), 1, any)
    ]

  variable$relevant <- variable$id %in% variable_id
  return(variable)
}

#' Missing description
#' @noRd

indikator_translate_filter_param <- function(con){

  gsub_name <- function(x){gsub("[^a-z]", "", tolower(x))}

  output <-
    data.frame(
      beschr =
        c(
          get_query(
            "SELECT beschr
       FROM reichweite_typ
       WHERE reichweite_klasse_id != (SELECT id FROM reichweite_klasse WHERE beschr = 'Räumliche Gebiete')"
          )[,1],
          "Räumliche Gebiete",
          "Zeit"
        )
    )

  output$bez <- gsub_name(output$beschr)
  output$param <- paste0("in_", paste0(substr(output$bez, 1, 1), substr(output$bez, nchar(output$bez), nchar(output$bez)), nchar(output$bez)))
  stopifnot("Parameter der Filter im Explorer sind nicht einzigartig" = all(!duplicated(output$param)))
  return(output)
}

#' Missing description
#' @noRd

indikator_gsub_zeit <- function(x, type = 1){
  if (type == 1) return(gsub("Zeit", "zeit", x))
  return(x)
}

#' Missing description
#' @noRd

indikator_draw_filter <- function(werte, unterscheiden = NULL, ns){

  if (nrow(werte) < 1){
    return(HTML(""))
  }

  filter_ui <- HTML("")

  if (is.null(unterscheiden)){
    unterscheiden <- c()
  }

  if (length(unterscheiden) > 0){
    gsub_name <- function(x){gsub("[^a-z]", "", tolower(x))}

    filter_ui <-
      tagList(
        lapply(
          unterscheiden,
          \(x){
            pickerInput(
              inputId  = ns(gsub_name(x)),
              label    = x,
              choices  = unique(werte[,x]),
              selected = NULL,
              options  = list(
                `actions-box`        = TRUE,
                `none-selected-text` = "nichts ausgewählt",
                `select-all-text`    = "alle auswählen",
                `deselect-all-text`  = "nichts auswählen",
                `live-search`        = TRUE
              ),
              multiple = TRUE
            )
          }
        )
      )
  }

  return(filter_ui)
}
