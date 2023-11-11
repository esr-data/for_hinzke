,
tags$div(
  id = "messageContainer",
  style = "position: fixed; bottom: -150px; left: 0; right: 0; text-align: center; padding: 10px; transition: bottom 0.5s; z-index: 1000;"
)


      observeEvent(input$circle_clicked, {
        cat("circle_clicked:", input$monitor_circle_clicked, "\n")
        clicked_circle_id <- input$circle_clicked

        # Führen Sie eine Aktion durch, wie zum Beispiel das Drucken der ID der geklickten Ellipse
        print(paste("Ellipse mit der ID", clicked_circle_id, "wurde geklickt."))

        color <- switch(input$circle_clicked,
                        "monitor_circle_mint" = "#afd700",
                        "monitor_circle_mint_stem" = "#afd700",
                        "monitor_circle_mint_minternational" = "#afd700",
                        "monitor_circle_mint_teilhabe" = "#afd700",
                        "monitor_circle_lehramt" = "#aea68b",
                        "monitor_circle_lehramt_fs" = "#aea68b",
                        "monitor_circle_lehramt_bedingungen" = "#aea68b",
                        "monitor_circle_lehramt_flex" = "#aea68b",
                        "monitor_circle_fs" = "#e73f0c",
                        "monitor_circle_fs_ki" = "#e73f0c",
                        "monitor_circle_fs_curriculum" = "#e73f0c",
                        "monitor_circle_bildung" = "#91bea0",
                        "monitor_circle_bildung_ganztag" = "#91bea0",
                        "monitor_circle_bildung_gerechtigkeit" = "#91bea0",
                        "monitor_circle_bildung_berufsorientierung" = "#91bea0",
                        "#dddddd"
        )
        print(paste("color value:", color))


        msg <- switch(input$circle_clicked,
                      "monitor_circle_mint" = "MINT",
                      "monitor_circle_mint_stem" = "MINT_STEAM",
                      "monitor_circle_mint_minternational" = "MINT_MINTernation",
                      "monitor_circle_mint_teilhabe" = "MINT_Teilhabe",
                      "monitor_circle_lehramt" = "Lehramt",
                      "monitor_circle_lehramt_fs" = "Lehramt Future Skills",
                      "monitor_circle_lehramt_bedingungen" = "Lehramt Bedingungen",
                      "monitor_circle_lehramt_flex" = "Lehramt Flexibilisierung",
                      "monitor_circle_fs" = "Future Skills",
                      "monitor_circle_fs_ki" = "Future Skills KI-Bildung",
                      "monitor_circle_fs_curriculum" = "Future Skills Curriculum",
                      "monitor_circle_bildung" = "Bildung",
                      "monitor_circle_bildung_ganztag" = "Bildung Ganztag",
                      "monitor_circle_bildung_gerechtigkeit" = "Bildung Gerechtigkeit",
                      "monitor_circle_bildung_berufsorientierung" = "Bildung Berufsorientierung",
                      "#dddddd"
        )
        print(paste("msg value:", msg))

        shinyjs::runjs(sprintf("$('#triangle').css('border-bottom-color', '%s');", color))

        shinyjs::html("messageContainer", msg)

        shinyjs::runjs(sprintf("$('#messageContainer').css('background-color', '%s');", color))

        shinyjs::runjs("setTimeout(function() { $('#messageContainer').css('bottom', '-150px'); }, 3000);")
      })


observeEvent(input$circle_clicked, {
  cat("circle_clicked:", input$monitor_circle_clicked, "\n")
  clicked_circle_id <- input$circle_clicked

  # Führen Sie eine Aktion durch, wie zum Beispiel das Drucken der ID der geklickten Ellipse
  print(paste("Ellipse mit der ID", clicked_circle_id, "wurde geklickt."))

  color <- switch(input$circle_clicked,
                  "monitor_circle_mint" = "#afd700",
                  "monitor_circle_mint_stem" = "#afd700",
                  "monitor_circle_mint_minternational" = "#afd700",
                  "monitor_circle_mint_teilhabe" = "#afd700",
                  "monitor_circle_lehramt" = "#aea68b",
                  "monitor_circle_lehramt_fs" = "#aea68b",
                  "monitor_circle_lehramt_bedingungen" = "#aea68b",
                  "monitor_circle_lehramt_flex" = "#aea68b",
                  "monitor_circle_fs" = "#e73f0c",
                  "monitor_circle_fs_ki" = "#e73f0c",
                  "monitor_circle_fs_curriculum" = "#e73f0c",
                  "monitor_circle_bildung" = "#91bea0",
                  "monitor_circle_bildung_ganztag" = "#91bea0",
                  "monitor_circle_bildung_gerechtigkeit" = "#91bea0",
                  "monitor_circle_bildung_berufsorientierung" = "#91bea0",
                  "#dddddd"
  )
  print(paste("color value:", color))


  msg <- switch(input$circle_clicked,
                "monitor_circle_mint" = "MINT",
                "monitor_circle_mint_stem" = "MINT_STEAM",
                "monitor_circle_mint_minternational" = "MINT_MINTernation",
                "monitor_circle_mint_teilhabe" = "MINT_Teilhabe",
                "monitor_circle_lehramt" = "Lehramt",
                "monitor_circle_lehramt_fs" = "Lehramt Future Skills",
                "monitor_circle_lehramt_bedingungen" = "Lehramt Bedingungen",
                "monitor_circle_lehramt_flex" = "Lehramt Flexibilisierung",
                "monitor_circle_fs" = "Future Skills",
                "monitor_circle_fs_ki" = "Future Skills KI-Bildung",
                "monitor_circle_fs_curriculum" = "Future Skills Curriculum",
                "monitor_circle_bildung" = "Bildung",
                "monitor_circle_bildung_ganztag" = "Bildung Ganztag",
                "monitor_circle_bildung_gerechtigkeit" = "Bildung Gerechtigkeit",
                "monitor_circle_bildung_berufsorientierung" = "Bildung Berufsorientierung",
                "#dddddd"
  )
  print(paste("msg value:", msg))

  shinyjs::runjs(sprintf("$('#triangle').css('border-bottom-color', '%s');", color))

  shinyjs::html("messageContainer", msg)

  shinyjs::runjs(sprintf("$('#messageContainer').css('background-color', '%s');", color))

  shinyjs::runjs("setTimeout(function() { $('#messageContainer').css('bottom', '-150px'); }, 3000);")
})


"$(document).on('click', 'ellipse', function() {
             var circle_id = $(this).attr('id');
             Shiny.setInputValue('circle_clicked', circle_id, {priority: 'event'});
           });
          "
