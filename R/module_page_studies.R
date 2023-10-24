#' Missing description
#' @noRd

module_studies_ui <- function(id = "studies", label = "m_studies", type = "all") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",

        # Variabler Titel
        h2(
          style = "text-align: center;",
          ifelse(
            type == "all",
            "Alle Studien",
            ifelse(
              type == "handlung1",
              "Studien zu Bildung & Kompetenz",
              ifelse(
                type == "handlung2",
                "Studien zu Forschung & Innovation",
                "NA"
              )
            )
          )
        ),

        # Informationen

        div(
          style = "background-color: #EAEDEF; padding: 20px; margin: 20px;",
          div(
            style = "color: #195365; margin: 0;",
            shiny::markdown(readLines("md/information_studien.md"))
          )
        ),

        # Box

        div(
          div(
            class = "studies_box",
            div(
              class = "studies_header",
              "FuE-Erhebung"
            ),
            a(
              href = "https://www.google.de",
              img(
                class = "studies_img",
                src = "img/studie_fue.jpg",
                onmouseover = "this.src = 'img/studie_fue_alt.jpg'",
                onmouseout  = "this.src = 'img/studie_fue.jpg'"
              )
            ),
            p(
              class = "studies_text",
              "Zahlen rund um die Forschung und Entwicklung in der Wirtschaft"
            )
          )
        )

      )
    )
  )
}

#' Missing description
#' @noRd

module_studies_server <- function(id = "studies", con, type = "all") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

    }
  )
}
