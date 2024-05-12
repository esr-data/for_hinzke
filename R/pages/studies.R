#' Necessary Packages/Functions

box::use(
  shiny[
    NS, moduleServer, observeEvent,
    fluidPage, tagList,
    markdown, HTML, tags,
    h2, a, p, div, img,
    uiOutput, renderUI
  ],
  bsplus[bs_embed_tooltip],
  shiny.router[get_query_param, get_page]
)

#' Missing description
#' @export

module_studies_ui <- function(id = "studies", label = "m_studies") {
  ns <- NS(id)
  tagList(
    fluidPage(
      div(
        class = "panel-content",

        # Variabler Titel
        uiOutput(ns("titel")),

        # Informationen

        div(
          style = "background-color: #EAEDEF; padding: 20px; margin: 20px;",
          div(
            style = "color: #195365; margin: 0;",
            markdown(readLines("md/information_studien.md"))
          )
        ),

        # Box

        uiOutput(ns("studies")),
        uiOutput(ns("foreign_studies")) #ToDo: Design-Frage: Betreiben wir dieses Auslagern aus den eigentlichen UIs in renderUIs in der Form so stark weiter oder sollten wir das nur machen, wenn auch tatsächlich was berechnet wird? Ich suche die Sachen immer eher direkt in der UI, aber ich glaube es gibt auch den Hang bei dem Projekt das alles in renderUis auszulagern

      )
    )
  )
}

#' Missing description
#' @export

module_studies_server <- function(id = "studies", con) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      observeEvent(
        get_query_param(), {
          if (get_page() == "studies"){

            param_hf <- get_query_param("hf")
            if (is.null(param_hf)) param_hf <- 0

            if (param_hf == 1){
              ui_titel <- "Studienprojekte zu Bildung & Kompetenz"
              output$studies <- renderUI({create_all_boxes_studies("handlung1")})
            } else if (param_hf == 2){
              ui_titel <- "Studienprojekte zu Forschung & Innovation"
              output$studies <- renderUI({create_all_boxes_studies("handlung2")})
            } else {
              ui_titel <- "Alle Studienprojekte"
              output$studies <- renderUI({create_all_boxes_studies("all")})
              output$foreign_studies <- renderUI({create_linklist_foreign_projekts()})
            }

            output$titel <- renderUI({h2(style = "text-align: center;", ui_titel)})
          }
        }, ignoreNULL = FALSE
      )




    }
  )
}

#' Missing description
#' @noRd

create_box_studies <-
  function(title, infos, img_front, img_back, tooltip = NULL, path, typ){
    element <-
      a(
        href =
          sprintf(
            "https://stifterverband.shinyapps.io/%s/", path
          ),
        target = "_blank",
        div(
          class = sprintf("studies_box %s", tolower(typ)),
          div(
            class = "studies_header",
            title
          ),
          img(
            class  = "studies_img",
            src    = sprintf("img/projects/%s", img_front),
            onmouseover = sprintf("this.src = 'img/%s'", img_back),
            onmouseout  = sprintf("this.src = 'img/%s'", img_front)
          ),
          p(
            class = "studies_text",
            infos
          )
        )
      )


    if (!is.null(tooltip)){
      return(bs_embed_tooltip(element, tooltip, placement = "bottom"))
    }

    return(element)
  }

#' Missing description
#' @noRd

create_all_boxes_studies <- function(type){

  box_daten <-
    data.frame(
      rbind(
        c(
          "Innovation",
          "FuE-Erhebung",
          "Zahlen rund um die Erhebung zu Forschung und Entwicklung in der Wirtschaft.",
          "studie_fue.jpg",
          "studie_fue_alt.svg",
          "Die Erhebung zur Forschung und Entwicklung (FuE) im Wirtschaftssektor wird jährlich vom Stifterverband im Auftrag des BMBF durchgeführt. Unter anderem werden hierbei die Aufwendungen und das Personal erfasst, welches Unternehmen in Deutschland in die Forschung und Entwicklung investieren. Details zu den aktuellen Ergebnisse finden sich auf dem Dateportal der FuE-Erhebung.",
          "FuE_Daten"
        ),
        c(
          "Innovation",
          "Gründungsradar",
          "Der Gründungsradar untersucht die Gründungsförderung an deutschen Hochschulen.",
          "studie_gruendungsradar.png",
          "TODO",
          "Der Gründungsradar untersucht die Güte der Gründungsförderung an deutschen Hochschulen und stellt diese vergleichend dar. Er unterstreicht dabei die Bedeutung der Hochschulen für das Innovationsgeschehen und zeigt deren Anstrengungen für eine wirkungsvolle Gründungskultur. Die vorliegende Ausgabe nutzt die gleiche Methodik und das gleiche Indikatorenset mit 46 Indikatoren wie die vorhergehende Befragung 2020. Somit sind Zeitvergleiche zwischen den Erhebungen möglich. An der Befragung nahmen 196 Hochschulen teil , für die das Thema Gründungsförderung eine Rolle spielt (184 im Jahr 2020). Der Gründungsradar bietet damit eine valide Datengrundlage für das Gründungsgeschehen und die Gründungsförderung an deutschen Hochschulen.",
          "Gruendungsradar"
        ),
        c(
          "Innovation",
          "Drittmittel an Hochschulen",
          "Drittmittelstatistik der Hochschulen - nach Bundesland, Hochschule, Hochschultyp und -trägerschaft.",
          "TODO",
          "TODO",
          "Drittmittel an Hochschulen sind finanzielle Zuwendungen von externen Quellen, die im Idealfall innovative Forschung und Lehre fördern, neue Projekte ermöglichen und den wissenschaftlichen Fortschritt beschleunigen. Es gibt verschiedene Drittmittelgeber (u. a. DFG, Bund, EU, Unternehmen und Stiftungen), wobei gerade die Drittmittel aus der Wirtschaft immer wieder Diskussionsgegenstand sind. Auf der Seite des Stifterverbandes sind die Dittmittel - zurückgehend bis 2006 - detailiert und interaktiv filterbar. Durchforsten Sie, wie viele Drittmittel von welchem Mittelgeber wann in welchem Bereich eingeworben/vergeben worden.",
          "DrittmittelAnHochschulen/"
        ),
        c(
          "Sonstige",
          "Engagement-Barometer",
          "Untersuchung der Zivilgesellschaft in der Covid19-Pademie.",
          "studie_engagement.jpg",
          "TODO",
          "In der Panelbefragung von Führungskräfte aus Infrastruktureinrichtungen, Landes- und Bundesverbänden, und gemeinnützigen Organisationen geht es um die Auswirkungen der Covid-19-Pandemie auf die Zivilgesellschaft und wie diese auch zu Problemlösungen beitragn kann.",
          "Engagement-Barometer"
        ),
        c(
          "Bildung",
          "Hochschul-Barometer",
          "Einschätzungen zu Hochschulpolitik durch die Hochschulleitungen.",
          "studie_hochschulbarometer.png",
          "TODO",
          "Die Ergebnisse des Hochschul-Barometers spiegeln die Einschätzungen eines großen Teils der deutschen Hochschullandschaft wider. An den Befragungen des Hochschul-Barometers nehmen immer etwa 160 Hochschulleitungen und damit über 40 Prozent der angeschriebenen Hochschulen teil. Die Erhebung gibt es seit 2011 und entsprechend sind Längsschnitteinschätzungen möglich. Neben einem allgemienen Fragenset gibt es wechselnde Schwerpunktthemen/Frageblöcke.",
          "Hochschul-Barometer"
        ),
        c(
          "Bildung",
          "Hochschul-Bildungs-Report",
          "Monitoring für das Hochschulsystem von 2010 bis 2020",
          "studie_hbr.svg",
          "TODO",
          "Der Hochschul-Bildungs-Report ist die zentrale Publikation der Bildungsinitiative \"Zukunft machen\". Darin haben der Stifterverband und McKinsey seit 2013 jährlich auf sechs Handlungsfeldern die deutsche Hochschulbildung analysiert. Der Report schließt mit der im Frühjahr 2022 erschienenen Ausgabe die Beobachtung einer Dekade ab. Er lieferte messbare Ziele für das Jahr 2020, die im Dialog mit Experten aus den Stifterverbands-Mitgliedsunternehmen, Wissenschaftsorganisationen und Vertretern der Zivilgesellschaft formuliert wurden. Und er gab Empfehlungen, wie diese Ziele zu erreichen waren.",
          "Hochschul-Bildungs-Report"
        ),
        c(
          "Bildung",
          "Länderchecks",
          "Bundeslandsvergleiche mit verschiedenen Themen",
          "studie_laenderchecks.jpg",
          "TODO",
          "Mit dem Ländercheck überprüft der Stifterverband den Stand und die Wirkungen des föderalen Wettbewerbs auf unterschiedlichen Feldern der akademischen Bildungs- und Innovationspolitik und zeichnet Landkarten Deutschlands, die Orientierung bieten für politische Standortdebatten.",
          "Laenderchecks"
        ),
        c(
          "Bildung",
          "Primus Preis",
          "Fakten und Analysen zu den Preisträgerinitiativen",
          "studie_primus.jpg",
          "studie_primus_alt.svg",
          "Der Primus-Preis zeichnet zivilgesellschaftliche Initiativen mit Vorbildcharakter aus. Die Datenportalseite umfasst sowohl Daten und Informationen zum Preis, z.B. zur Fördersumme und zur Anzahl der Projekte, als auch die Evaluation.", # , welche von Dezember 2019 bis Januar 2020 als eine Online-Befragung der gewählten Primus-Preisträgerinitiativen durchgeführt wurde
          "PrimusPreis"
        ),
        c(
          "Bildung",
          "Private Hochschulen",
          "Fakten und Analysen zu privaten Hochschulen",
          "studie_privat.jpg",
          "studie_private_alt.PNG",
          "In der Studienreihe wirft der Stifterverband einen Blick auf den Sektor privater Hochschulen und untersucht, wie sich der Sektor auch unter Berücksichtigung von Transformationsprozessen im gesamten Hochschulwesen, in der Gesellschaft und in der Arbeitswelt entwickelt.",
          "PrivateHochschulen"
        ),
        c(
          "Bildung",
          "Stiftungsprofessuren",
          "Übersicht und Daten zu Stiftungsprofessuren, u.a. im Rahmen einer interaktiven Deutschlandkarte",
          "studie_professur.svg",
          "studie_professur_alt.svg",
          "Der Stifterverband fördert seit Mitte der 1980er-Jahre Stiftungsprofessuren an deutschen Hochschulen. Die Anzahl und die Bedeutung von Stiftungsprofessuren haben über die Jahre zugenommen, denn Wirtschaft und Wissenschaft profitieren gleichermaßen von ihnen.",
          "Stiftungsprofessuren"
        )
      )
    )
  names(box_daten) <- c("typ", "titel", "text", "bild_front", "bild_hinten", "tooltip", "path")

  if (type == "handlung1"){
    box_daten <- box_daten[box_daten$typ == "Bildung",]
  } else if (type == "handlung2"){
    box_daten <- box_daten[box_daten$typ == "Innovation",]
  }

  div(
    style = "padding: 20px; margin: 20px; display: flex; flex-wrap: wrap;",
    tagList(
      apply(box_daten, 1, \(x) create_box_studies(x["titel"], x["text"], x["bild_front"], x["bild_hinten"], x["tooltip"], x["path"], x["typ"]))
    )
  )

}

#' Missing description
#' @noRd

create_linklist_foreign_projekts <- function(){
  div(
    HTML('<hr style="width: 50px; text-align: left; margin-left: 0; margin-top: 7px; margin-bottom: 25px; border-top: 3px solid #e73f0c;">'),
    p("Datenportale anderer Organisationen zu den Bereichen Bildung, Wissenschaft und Innovation:"),
    tags$ul(
      tags$li(tags$a("www.hsi.de")),
      tags$li(tags$a("www.che.de")),
      tags$li(tags$a("www.heads.de")),
      tags$li(tags$a("www.bmbf.de")),
      tags$li(tags$a("www.destatis.de")),
      tags$li(tags$a("www.eurostat.de")),
      tags$li(tags$a("www.weltbank.de")),
      tags$li(tags$a("www.oecd.de")),
      tags$li(tags$a("www.statista.de"))
    )
  )
}


