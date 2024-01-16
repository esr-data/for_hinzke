#' Necessary Packages/Functions

box::use(
  ../../R/utils/database[get_query],
  ../../R/utils/earthworm[read_markdown],
  shiny[
    NS, moduleServer, observeEvent, observe,
    reactiveValues, reactiveValuesToList,
    fluidPage, tagList, h2, div, markdown, p,
    uiOutput, renderUI, HTML,
    actionButton, tags
  ],
  slickR[slickROutput, renderSlickR, slickR, settings]
)

#' Missing description
#' @export

module_home_ui <- function(id = "home", label = "m_home") {
  ns <- NS(id)
  fluidPage(
    div(
      class = "panel-content",
      div(
        style = "display: flex; flex-direction: row; justify-content: center;",
        div(
          style = "width: 500px; padding: 50px;",
          h2(class = "moodie_title", "Bildung und Innovation in Zahlen"),
          uiOutput(ns("willkommen"))
        ),
        div(
          style = "padding: 50px; border: 2px solid var(--grey); border-radius: 8px;",
          slickROutput(ns("slick_output"), width = '500px', height = '500px')
        )
      ),
      div(
        h2("News"),
        p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.")
      ),
      div(
        style = "background-color: #EAE9E3; margin: 40px -40px; padding: 40px",
        h2("Hilfe", style = "text-align: center;"),
        p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.")
      ),
      div(
        h2("Inhalt des SV Datenportals"),
        p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.")
      ),
      div(
        style = "background-color: #8BA791; margin: 40px; padding: 20px 40px",
        h2("SV Data", style = "text-align: center;"),
        p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.")
      ),
      div(
        h2("Weitere Ressourcen"),
        p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.")
      ),
      div(style = "margin-bottom: 100px")
    )
  )
}

#' Missing description
#' @export

module_home_server <- function(id = "home") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$willkommen <- renderUI({read_markdown("willkommen")})

      output$slick_output <- renderSlickR({
        # TODO Platzhalter
        slickR(
          obj = paste0("img/", grep("_alt.svg", list.files("www/img"), value = TRUE)),
          height = 450,
          width = "95%"
        ) +
          settings(
            autoplay = TRUE
          )
      })


    }
  )
}

