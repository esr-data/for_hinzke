con <- dbConnect(RSQLite::SQLite(), "data/magpie.sqlite")

get_variable_ids <- function(beschr){
  dbGetQuery(
    con,
    paste(
      "SELECT id FROM tag WHERE beschr IN (",
      paste(
        paste0("'", beschr, "'"),
        collapse = ","
      ),
      ")"
    )
  )[,1]
}

x <- get_variable_ids("Interne FuE-Aufwendungen")


load_table_by_variable <- function(variable){

  daten <-
    dbGetQuery(
      con,
      sprintf(
        "SELECT daten.id, variable.beschr as variable, jahr, wert, wert_einheit.beschr as einheit
         FROM daten
         LEFT JOIN wert_einheit ON daten.wert_einheit_id = wert_einheit.id
         LEFT JOIN variable     ON daten.variable_id = variable.id
         WHERE variable_id = %s",
        variable
      )
    )

  reichweite <-
    "SELECT reichweite.id as id, reichweite.beschr as reichweite, rtyp.beschr as reichweite_typ, rklasse.beschr as reichweite_klasse
     FROM reichweite
     LEFT JOIN reichweite_typ rtyp ON reichweite.reichweite_typ_id = rtyp.id
     LEFT JOIN reichweite_klasse rklasse ON rtyp.reichweite_klasse_id = rklasse.id
     WHERE reichweite.id IN (SELECT reichweite_id FROM daten_reichweite WHERE daten_id IN (SELECT id FROM daten WHERE variable_id = %s))" |>
    sprintf(variable) |>
    dbGetQuery(conn = con)

  daten_reichweite <-
    dbGetQuery(
      con,
      sprintf(
        "SELECT daten_id, reichweite_id
         FROM daten_reichweite
         WHERE daten_id IN (SELECT id FROM daten WHERE variable_id = %s)",
        variable
      )
    )

  reichweite$gruppe <-
    ifelse(
      reichweite$reichweite_klasse == "Räumliche Gebiete",
      reichweite$reichweite_klasse,
      reichweite$reichweite_typ
    )

  daten[,unique(reichweite$gruppe)] <- NA

  for (i in 1:nrow(daten)){
    x <- daten_reichweite[daten_reichweite$daten_id == daten$id[i],]
    for (j in 1:nrow(x)){
      k <- reichweite$id == x$reichweite_id[j]
      l <- c(daten[i, reichweite$gruppe[k]], reichweite$reichweite[k])
      l <- l[!is.na(l)]
      daten[i, reichweite$gruppe[k]] <- paste(l, collapse = ", ")
    }
  }

  for (i in unique(reichweite$gruppe)){
    daten[,i] <- ifelse(is.na(daten[,i]), "Insgesamt", daten[,i])
  }

  names(daten) <- gsub("jahr", "Zeit", names(daten))

  return(list(daten = daten, gruppe = unique(reichweite$gruppe)))
}


var_table <- load_table_by_variable(138)

ui <- function() {
    fluidPage(
      uiOutput("input_list"),
      uiOutput("output_list")
    )
}

server <- function(input, output, session) {

  output$input_list <- renderUI({

    show_elements <-
      radioButtons(
        inputId = "show",
        label = "Gewünschte Form der Darstellung:",
        choices = c(
          "Tabelle",
          "Zeitreihe",
          "Karte"
        )
      )

    range_elements <-
      map(
        1:length(var_table$gruppe),
        ~ selectInput(
          inputId = paste0("i", .x),
          label = var_table$gruppe[.x],
          choices = unique(var_table[["daten"]][[var_table$gruppe[.x]]])
        )
      )

      tagList(show_elements, range_elements)

  })

  filtered_data <- reactive({
    filtered <- var_table[["daten"]]
    for(i in 1:length(var_table$gruppe)) {
      input_id <- paste0("i", i)
      selected_value <- input[[input_id]]
      filtered <- filtered[filtered[[var_table$gruppe[i]]] == selected_value, ]
    }
    filtered %>%
      select(-id)
  })

  filtered_data_map <- reactive({
    req(input$map_year)
    filtered_data() %>%
      filter(time == input$map_year)
  })

  output$output_list <- renderUI({

    if(nrow(filtered_data()) == 0) {
      return(HTML("<p>Leider Keine Daten in dieser Kombination verfügbar, bitte ändern Sie die Eingabe/n.</p>"))
    } else if (input$show == "Tabelle") {
      renderTable(filtered_data())
    }

  })

}

shinyApp(ui, server)
# profvis::profvis(shinyApp(ui, server)) #
