#' Necessary Packages/Functions

box::use(
  ../../R/utils/database[get_query],
  ../../R/utils/earthworm[read_markdown],
  shiny[
    NS, moduleServer, observeEvent, observe,
    reactiveValues, reactiveValuesToList,
    fluidPage, tagList, h2, div, markdown, p,
    uiOutput, renderUI, HTML,
    actionButton, tags, img, br
  ],
  slickR[slickROutput, renderSlickR, slickR, settings],
  shiny.router[get_page, get_query_param]
)

#' Missing description
#' @export

module_home_ui <- function(id = "home", label = "m_home") {
  ns <- NS(id)
  fluidPage(
    draw_landing_page(ns)
  )
}

#' Missing description
#' @export

module_home_server <- function(id = "home") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(
        session$clientData$url_hash, {
          if(session$clientData$url_hash == "#!/"){
            #TODO LÖSCHEN? output$startseite <- renderUI({draw_landing_page(ns)})
            output$slick_output <- renderSlickR({
              slickR(
                obj = paste0("img/projects/", grep("_alt.svg", list.files("www/img/projects"), value = TRUE)),
                height = 450,
                width = "95%"
              ) +
                settings(
                  autoplay = TRUE
                )
            })
          }
        }
      )
    }
  )
}

draw_landing_page <- function(ns){
  div(
    class = "panel-content",
    div(
      style = "display: flex; flex-direction: row; justify-content: center;",
      div(
        style = "width: 500px; padding: 50px;",
        h2(class = "moodie_title", "Bildung und Innovation in Zahlen"),
        read_markdown("willkommen")
      ),
      div(
        style = "padding: 70px; border: 2px solid var(--grey); border-radius: 8px;",
        slickROutput(ns("slick_output"), width = '500px', height = '500px')
      )
    ),
    br(),
    div(
      h2("Aktuelles"),
      tags$ul(
        tags$li("23.05.2024: Die Daten des Hochschul-Barometers wurden ins Datenportal eingespielt und können im Daten Explorer erkundet werden"),
        tags$li("17.05.2024: Neue Analyse zum Thema Ganztagesausbau"),
        tags$li("10.05.2024: Erste Einblicke in das Datenprojekt Higher Education Explorer (HEX)")
      )
    ),
    br(),
    div(
      style = "background-color: #EAE9E3; margin: 40px -40px; padding: 40px",
      h2("Daten und Analysen im Stifterverband", style = "text-align: center;"),
      p("Wie muss sich die Hochschulbildung in Deutschland entwickeln, um Nachwuchskräfte mit den für die Zukunft nötigen Kompetenzen zu versorgen? Wie viel investieren deutsche Unternehmen in die eigene Forschung und reicht das, um den Innovationsstandort Deutschland zu sichern? Diese und weitere Fragen analysieren wir regelmäßig in Studien und Erhebungen. Dazu nutzen wir Daten und wissenschaftliche Ergebnisse, die wir selbst erheben, in Auftrag gegeben oder von Dritten bereitgestellt werden, und zeigen klar auf, wo Veränderungen im Bildungs-, Wissenschafts- und Innovationssystem nötig sind. Wir identifizieren Stärken und Schwächen, machen Handlungsbedarfe sichtbar und zeigen, welche Themen verstärkt in den Fokus politischer Debatten rücken müssen. Das Ziel: evidenzbasierte Entscheidungen in Politik und Wissenschaft zu ermöglichen – mit konkreten Handlungsempfehlungen und Zukunftsszenarien.")
    ),
    br(),
    div(
      h2("Unsere Handlungsfelder", style = "text-align: center;"),
    ),
    div(
      style = "display: flex; flex-wrap: wrap;",
      div(
        style = "flex: 1 1 45%; width: 500px; margin: 0px 10px; background-color: #C3DA46; text-align: center; padding: 20px; box-sizing: border-box; display: flex; flex-direction: column; justify-content: space-between; height: 666px;",
        class = "responsive-div",
        div(
          style = "flex: 1;",
          h2("BILDUNG & KOMPETENZEN"),
          br(),
          br(),
          br(),
          p("In diesem Handlungsfeld bündeln wir unsere Aktivitäten im Bildungsbereich.
            Das große Ziel behalten wir immer im Blick: junge Menschen bestmöglich
            auf eine Welt vorzubereiten, in der das einzig Beständige die Veränderung ist.
            Hierzu tragen wir dazu bei schulische Bildung zu stäken, die MINT-Lücke zu schließen,
            Future Skills zu verankern und Innovative Lernorte zu gestalten.")
        ),
        div(
          style = "display: flex; justify-content: center; align-items: flex-end;",
          img(
            src = "img/education.png",
            height = "230px",
            alt = "Symbolbild Handlungsfeld Bildung und Kompetenzen"
          )
        )
      ),
      div(
        style = "flex: 1 1 45%; width: 500px; margin: 0px 10px; background-color: #A96BF1; text-align: center; padding: 20px; box-sizing: border-box; display: flex; flex-direction: column; justify-content: space-between; height: 666px;",
        class = "responsive-div",
        div(
          style = "flex: 1;",
          h2("KOLLABORATIVE FORSCHUNG & INNOVATION"),
          br(),
          p("Damit in Deutschland mehr Innovationen entstehen, vernetzen wir Wirtschaft, Wissenschaft und
            Gesellschaft und fördern die Zusammenarbeit untereinander. Denn Neues entsteht häufig an den
            Schnittstellen von Unternehmen und Wissenschaftseinrichtungen aber auch von
            unterschiedlichen Forschungsgebieten und gesellschaftlichen Perspektiven. Wir haben uns auf die Fahnen geschrieben, den Impact of Science zu stärken, Science Entrepreneurship zu entwickeln und Forschung und Innovation systemisch zu gestalten.")
        ),
        div(
          style = "display: flex; justify-content: center; align-items: flex-end;",
          img(
            src = "img/innovation.png",
            height = "230px",
            alt = "Symbolbild Handlungsfeld kollobarative Forschung und Innovation"
          )
        )
      )
    ),
    # div(
    #   h2("Inhalt des SV Datenportals"),
    #   p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.")
    # ),
    # div(
    #   style = "background-color: #8BA791; margin: 40px; padding: 20px 40px",
    #   h2("SV Data", style = "text-align: center;"),
    #   p("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.")
    # ),
    br(),
    div(
      h2("Mitmachen"),
      p("Ihnen fehlen bestimmte Daten im Bereich Bildung, Wissenschaft und Innovation in unserem Daten-Explorer? Sie erheben selbst Daten in diesem Bereich oder führen Analysen durch und würden diese gerne in unserem Datenportal abgebildet sehen? Schreiben Sie uns! Wir würden uns sehr freuen, Ihre Datenbestände und -analysen mit in unsere Datenbank aufnehmen zu können und somit, zum einen Ihren Daten weitere Reichweite geben und zum anderen Transparenz in unseren Handlungsfeldern erhöhen. Auch wenn Sie an gemeinsamen Studien und/oder anderen Datenporjekten interessiert sind. Melden Sie sich gerne!")
    ),
    div(style = "margin-bottom: 100px")
  )
}
