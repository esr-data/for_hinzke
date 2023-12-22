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
# con <<- DBI::dbConnect(RSQLite::SQLite(), "data/magpie.sqlite")
# source("test_data.R")
# content_list_monitor_subpage_structure <- content_list_monitor_subpages_structure_full[["bildung_ganztag"]]

box::use(

  # sources

  ../../R/utils/monitor_content[
    load_table_by_variable_monitor,
    create_link_with_svg
  ],

  # packages

  DT[
    renderDataTable
  ],
  shiny[
    column,
    conditionalPanel,
    fluidPage,
    moduleServer,
    numericInput,
    NS,
    observe,
    observeEvent,
    radioButtons,
    reactive,
    renderUI,
    req,
    selectInput,
    uiOutput,
  ],
  shinyWidgets[
    pickerInput,
    updatePickerInput
  ]
)

#' Missing description
#' @noRd

monitor_indicator_main_content_ui <- function(id, var_table, con = con) {
  ns <- NS(id)

  auswahlmoeglichkeiten <- setdiff(unique(colnames(var_table$daten)), c("id", "variable", "Jahr", "Wert", "Messeinheit"))

  fluidPage(
    column(
      width = 3,
      selectInput(
        inputId = ns("gliederungsauswahl1_in"),
        label = "Bitte wählen Sie eine Gliederungsebene",
        choices = c("---", auswahlmoeglichkeiten)
      ),
      conditionalPanel(
        condition = "input.gliederungsauswahl1_in != '---'", ns = ns,
        pickerInput(
          inputId = ns("kategorieauswahl1_in"),
          label = "Bitte wählen Sie, welche Daten Ihnen angezeigt werden sollen",
          choices = "---",
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
      radioButtons(
        inputId = ns("darstellungsweise1_in"),
        label = "Darstellungsweise:",
        choices = c("Zeitverlauf", "Tabelle"),
        selected = "Zeitverlauf"
      )
    ),
    column(
      width = 9,
      conditionalPanel(
        condition = "input.darstellungsweise1_in == 'Karte'", ns = ns,
        numericInput(
          inputId = ns("karte1_jahr_in"),
          label = "Jahr:",
          value = max(var_table$daten$Jahr),
          min = min(var_table$daten$Jahr),
          max = max(var_table$daten$Jahr)
        )
      ),
      uiOutput(ns("output_list"))
    )
  )
}

#' Missing description
#' @noRd

monitor_indicator_main_content_server <- function(id, var_table, con = con) {
  moduleServer(id, function(input, output, session) {

    # reactives

    filtered_data_gliederung <- reactive({
      manage_explorer_data_monitor(
        werte = var_table$daten,
        gruppe = var_table$gruppe,
        unterscheiden = input$gliederungsauswahl1_in
      )
    })

    filtered_data_gliederung_kategorie <- reactive({
      if (input$gliederungsauswahl1_in != "---" & !is.null(input$gliederungsauswahl1_in)) {
        filter_col <- input$gliederungsauswahl1_in
        filter_kat <- input$kategorieauswahl1_in

        return(filtered_data_gliederung() %>% filter(.[[filter_col]] %in% filter_kat))
      } else {
        return(NULL)
      }
    })

    filtered_data_gliederung_kategorie_karte <- reactive({
      req(filtered_data_gliederung_kategorie())
      if (input$darstellungsweise1_in == "Karte") {
        return(filtered_data_gliederung_kategorie() %>% filter(.[["Jahr"]] == as.numeric(input$karte1_jahr_in)))
      } else {
        return(NULL)
      }
    })

    plot_data <- reactive({
      if (input$gliederungsauswahl1_in == "---") {
        filtered_data_gliederung()
      } else if (input$darstellungsweise1_in == "Karte") {
        filtered_data_gliederung_kategorie_karte()
      } else {
        filtered_data_gliederung_kategorie()
      }
    })

    # observes

    observeEvent(input$gliederungsauswahl1_in, {
      req(input$gliederungsauswahl1_in != "---")
      req(input$gliederungsauswahl1_in %in% colnames(filtered_data_gliederung()))
      unique_values <- unique(filtered_data_gliederung()[[input$gliederungsauswahl1_in]])
      selected_values <- NULL

      if(length(unique_values) < 6) {
        selected_values <- unique_values
      } else {
        if (input$gliederungsauswahl1_in == "Räumliche Gebiete") {
          selected_values <- c("Deutschland", "Bayern", "Baden-Württemberg", "Nordrhein-Westfalen")
        } else {
          selected_values <- unique_values[1:5]
        }
      }

      updatePickerInput(
        session = session,
        inputId = "kategorieauswahl1_in",
        choices = unique_values,
        selected = selected_values
      )
   })

   observe({
     req(input$gliederungsauswahl1_in)

     if (input$gliederungsauswahl1_in == "Räumliche Gebiete") {
       updateRadioButtons(
         session = session,
         inputId = "darstellungsweise1_in",
         choices = c("Zeitverlauf", "Tabelle", "Karte"),
         selected = input$darstellungsweise1_in
       )
     } else {
       updateRadioButtons(
         session = session,
         inputId = "darstellungsweise1_in",
         choices = c("Zeitverlauf", "Tabelle"),
         selected = ifelse(
           input$darstellungsweise1_in != "Karte",
           input$darstellungsweise1_in,
           "Zeitverlauf")
       )
     }
   })

   observe({
     req(input$darstellungsweise1_in)

     if (input$darstellungsweise1_in == "Karte") {
       updateNumericInput(
         session = session,
         inputId = "karte1_jahr_in",
         value = max(filtered_data_gliederung_kategorie()$Jahr),
         min = min(filtered_data_gliederung_kategorie()$Jahr),
         max = max(filtered_data_gliederung_kategorie()$Jahr)
       )
     } else {
       updateNumericInput(
         session = session,
         inputId = "karte1_jahr_in",
         value = NULL,
         min = NULL,
         max = NULL
       )
     }
   })

    # outputs

    output$output_list <- renderUI({

      if (input$darstellungsweise1_in == "Tabelle" & nrow(plot_data() > 0)) {

        #TODO: Auslagerung Grundfunktionen in svVis

        renderDataTable({
          datatable(
            plot_data(),
            extensions = 'Buttons',
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
        })

      } else if(input$darstellungsweise1_in == "Zeitverlauf" & nrow(plot_data() > 0)) {

        #TODO: Auslagerung Grundfunktionen in svVis

        renderPlotly({
          if(input$gliederungsauswahl1_in == "---") {
            config(
              toImageButtonOptions = list(filename = str_c("SV Monitor Wissenschaft")),
              modeBarButtonsToRemove = c("lasso2d", "zoomIn2d", "zoomOut2d"),
              locale = "de",
              displaylogo = FALSE,
              p = ggplotly(
                ggplot(plot_data(), aes(x = Jahr, y = Wert)) +
                  geom_line() +
                  suppressWarnings(geom_point(aes(
                    text = str_c("Jahr: ", Jahr, "<br>Wert: ",
                                 prettyNum(round(Wert, 1), big.mark = ".", decimal.mark = ","))), color = "#195365")) +
                  theme_pubr() +
                  theme(plot.title = element_text(hjust = -0.45, vjust = 2.12),
                        plot.margin = unit(c(1, 0.5, 1, 1), "cm"),
                        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
                        axis.text.x = element_text(angle = 45, hjust = 1)
                  ) +
                  labs(
                    x = "",
                    y = str_c(plot_data()$Messeinheit[1], "\n"),
                    title = str_c("", "\n")
                  ) +
                  scale_x_continuous(breaks = c(
                    min(plot_data()$Jahr):max(plot_data()$Jahr)
                  )) +
                  scale_color_manual(values = "#195365")
              )
            ) %>%
              layout(
                annotations =
                  list(
                    x = 1,
                    y = -0.3,
                    text = paste0(
                      "Quelle: ",
                      "TODO plot_data()$Quelle[1]."
                    ),
                    showarrow = F,
                    xref = 'paper',
                    yref = 'paper',
                    xanchor = 'right',
                    yanchor = 'auto',
                    xshift = 0,
                    yshift = 0,
                    font = list(size = 10, color = "black")
                  )
              )
          } else if (ncol(plot_data() == 4)) {
            gliederungsauswahl <- input$gliederungsauswahl1_in
            config(
              toImageButtonOptions = list(filename = str_c("SV Monitor Wissenschaft - ", " gegliedert nach ")),
              modeBarButtonsToRemove = c("lasso2d", "zoomIn2d", "zoomOut2d"),
              locale = "de",
              displaylogo = FALSE,
              p = ggplotly(
                ggplot(
                  plot_data(),
                  aes(x = Jahr, y = Wert, col = !!sym(input$gliederungsauswahl1_in))
                ) +
                  geom_line() +
                  suppressWarnings(geom_point(aes(
                    text = str_c("Jahr: ", Jahr, "<br>Kategorie: ", gliederungsauswahl, "<br>Wert: ",
                                 prettyNum(round(Wert, 1), big.mark = ".", decimal.mark = ","))))) +
                  theme_pubr() +
                  theme(plot.title = element_text(hjust = -0.45, vjust = 2.12),
                        plot.margin = unit(c(1, 0.5, 1, 1), "cm"),
                        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
                        axis.text.x = element_text(angle = 45, hjust = 1)
                  ) +
                  labs(
                    x = "",
                    y = str_c(plot_data()$Messeinheit[1], "\n"),
                    title = str_c("", "\n")
                  ) +
                  scale_x_continuous(breaks = c(
                    min(
                      plot_data()$Jahr
                    ):max(
                      plot_data()$Jahr
                    )
                  )) +
                  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",",
                                                                 scientific = FALSE)) +
                  scale_color_manual(
                    values = Farben,
                    name = gliederungsauswahl, drop = FALSE
                  ), tooltip = "text"
              )
            ) %>%
              layout(
                annotations =
                  list(
                    x = 1,
                    y = -0.3,
                    text = paste0(
                      "Quelle: ",
                      "TODOplot_data()$Quelle[1]", "."
                    ),
                    showarrow = F,
                    xref = 'paper',
                    yref = 'paper',
                    xanchor = 'right',
                    yanchor = 'auto',
                    xshift = 0,
                    yshift = 0,
                    font = list(size = 10, color = "black")
                  )
              )
          }
        })

      # } else if (input$darstellungsweise1_in == "Karte" & nrow(plot_data() > 0)) {  #TODO Performantere, nicht brechende Choropleth-Funktion
      #
      #   x <- plot_data() %>%
      #     rename("Region" = `Räumliche Gebiete`)
      #
      #   renderPlot({
      #     create_choropleth_map_germany(x, Wert)
      #   })

      } else {
        renderPlotly({
          ggplotly(ggplot() +
                     annotate("text", x = 2, y = 2, label = "Bitte Daten auswählen.") +
                     labs(x = "", y = "") +
                     theme_pubr() +
                     theme(line = element_blank(),
                           axis.text = element_blank()),
                   tooltip = NULL) %>%
            layout(xaxis = list(fixedrange = TRUE)) %>%
            layout(yaxis = list(fixedrange = TRUE)) %>%
            config(displayModeBar = FALSE)

        })}
    })

  })
}

# test module

# ui <- fluidPage(
#   monitor_indicator_main_content_ui("mein_modul_id", load_table_by_variable_monitor(138))
# )
#
# server <- function(input, output, session) {
#   monitor_indicator_main_content_server("mein_modul_id", load_table_by_variable_monitor(138))
# }
#
# shinyApp(ui, server)
