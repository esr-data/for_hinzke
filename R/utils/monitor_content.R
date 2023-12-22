
box::use(
  ../../R/utils/monitor_content_module[
    monitor_indicator_main_content_server,
  ],
  DBI[
    dbGetQuery
  ],
  dplyr[
    rename
  ],
  purrr[
    map2
  ],
  shiny[
    div, fluidRow, h4, tags
  ]
)

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
#'
load_table_by_variable_monitor <- function(variable, con){

  # browser()

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

  daten <- rename(daten, "Jahr" = Zeit, "Wert" = wert, "Messeinheit" = einheit)

  return(list(daten = daten, gruppe = unique(reichweite$gruppe)))
}

#' Missing description
#' @noRd

manage_explorer_data_monitor <- function(werte, gruppe = NULL, unterscheiden = NULL, filtern = NULL){

  # browser()

  unterscheiden_string <- deparse(substitute(unterscheiden))

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

  names(werte)[names(werte) == "einheit"] <- "Messeinheit"
  names(werte)[names(werte) == "wert"] <- "Wert"
  names(werte)[names(werte) == "Zeit"] <- "Jahr"
  werte$Wert <- as.numeric(werte$Wert)

  neue_gruppe <- gruppe[!(gruppe %in% nicht_unterscheiden)]
  return(werte[,c("Jahr", "Wert", "Messeinheit", neue_gruppe)])
}

#' Missing description
#' @noRd

get_content_monitor_bildung <- function(){
  list(
    "circle_bildung_ganztag" = list(
      "ID" = paste0("box_", 1:7),
      "Titel" = "Ganztag als Bildungszeit",
      "Untertitel" = "Wie steht es um den Ausbau der Ganztagsschule?",
      "Einfuehrungstext" = "Ganztagsschulen können dazu beitragen, Bildungsungleichheiten zu verringern, indem sie allen Kindern, unabhängig von ihrem familiären Hintergrund, Zugang zu zusätzlichen Bildungsressourcen und Unterstützung bieten. Ganztagsschulen ermöglichen eine bessere Vereinbarkeit von Beruf und Familie, da Eltern nicht für die Betreuung ihrer Kinder am Nachmittag sorgen müssen. Zudem bieten sie Raum für innovative pädagogische Konzepte, die über den traditionellen Unterricht hinausgehen und die Persönlichkeitsentwicklung sowie soziale Kompetenzen der Schüler fördern. Ganztagsangebote sind eine Chance Schule neu zu denken und auch andere gesellschaftliche Akteure sinnstiftend einzubinden.",
      "Indikator_Inhalt_IDs" = c(
        "ganztag_quantitativ",
        "ganztag_vielfalt",
        "ganztag_kooperation",
        "ganztag_sozial",
        "ganztag_multiprofessionel",
        "ganztag_digital",
        "ganztag_lage"
      ),
      "Indikator_Inhalt_UI" = c(
        function() monitor_indicator_main_content_ui("ganztag_quantitativ", var_table = load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("ganztag_vielfalt", var_table =load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("ganztag_kooperation", var_table =load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("ganztag_sozial", var_table =load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("ganztag_multiprofessionel", var_table =load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("ganztag_digital", var_table =load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("ganztag_lage", var_table = load_table_by_variable_monitor(138))
      ),
      "Indikator_Inhalt_Server" = c(
        function() monitor_indicator_main_content_server("ganztag_quantitativ", load_table_by_variable_monitor(138, con = con)),
        function() monitor_indicator_main_content_server("ganztag_vielfalt", load_table_by_variable_monitor(122, con = con)),
        function() monitor_indicator_main_content_server("ganztag_kooperation", load_table_by_variable_monitor(138, con = con)),
        function() monitor_indicator_main_content_server("ganztag_sozial", load_table_by_variable_monitor(138, con = con)),
        function() monitor_indicator_main_content_server("ganztag_multiprofessionel", load_table_by_variable_monitor(138, con = con)),
        function() monitor_indicator_main_content_server("ganztag_digital", load_table_by_variable_monitor(138, con = con)),
        function() monitor_indicator_main_content_server("ganztag_lage", load_table_by_variable_monitor(138, con = con))
      ),
      "Ueberschriften" = c(
        "Ausbau der Ganztagsangebote",
        "Vielfalt der Ganztagsangebote",
        "Kooperationen zwischen Schule und Zivilgesellschaft im Ganztag",
        "Sozialer Ausgleich bei Ausbau der Ganztagsangebote",
        "Multiprofessionele Teams an Schulen",
        "IT-Infrastruktur an Schulen",
        "Die Lage an den Schulen aus Sicht der Schulleitungen"
      ),
      "Fragen" = c(
        "Schaffen wir quantitativ genügend Ganztagsangebote?",
        "Schaffen wir vielfältige Ganztagsangebote?",
        "Schaffen wir Kooperationen zwischen Schule und Zivilgesellschaft?",
        "Schaffen wir es Ganztagsschule sozial einzuführen?",
        "Schaffen wir es multiprofessionelle Teams in den Schulen zu verankern?",
        "Schaffen wir die Voraussetzungen für digitale Bildungsangebote?",
        "Schaffen wir es die wahrgenomme Lage an den Schulen zu verbessern?"
      ),
      "Ziele" = c(
        "Schulen in Deutschland sind Ganztagsschulen",
        "Ganztagsunterricht wird vielseitig gestaltet",
        "Zivilgesellschaftlicher Akteure sind in den Ganztag eingebunden",
        "Insbesondere in Schulen mit niedrigem Sozialindex sind zivilgesellschaftliche Akteure eingebunden",
        "Multiprofessionelle Teams an Schulen",
        "Schulen haben WLAN",
        "Die Lage an Schulen wird als gut eingeschätzt"
      ),
      "Indikatoren" = c(
        "Anteil der Ganztagsschulen in Deutschland",
        "Vielfalt der Angebote im Rahmen der Ganztagsbetreuung",
        "Einbindung zivilgesellschaftlicher Akteure",
        "Einbindung zivilgesellschaftlicher Akteure nach Sozialindex",
        "Anteil Schulpersonal außerhalb der Lehrkräfte an Schulpersonal",
        "Anteil Schulen mit WLAN über 100 Mbit",
        "Anteil Schulleitungen die Lage an ihrer Schule insgesamt als gut bezeichnen"
      ),
      "Aktivitaeten" = c(
        "Studie: Vereine, Stiftungen und Co - die neuen Bildungspartner?",
        "Studie: Hochschul-Bildungs-Report",
        "Das Programm von Bildung und Begabung"
      ),
      "Aktivitaeten_Link" = c(
        "https://www.stifterverband.org/download/file/fid/6014",
        "https://www.hochschulbildungsreport.de/sites/hsbr/files/hochschul-bildungs-report_abschlussbericht_2022.pdf",
        "https://www.bildung-und-begabung.de/"
      ),
      "Datenbasis" = c(
        "KMK Schulen in Ganztagsform",
        "Studie zur Entwicklung von Ganztagsschulen (SteG)",
        "ZNL MINT-Angebote im schulischen Ganztag"
      ),
      "Datenbasis_Link" = c(
        "https://www.kmk.org/dokumentation-statistik/statistik/schulstatistik/allgemeinbildende-schulen-in-ganztagsform.html",
        "https://steg.dipf.de/de",
        "https://wp.znl-ulm.de/"
      ),
      "Links" = c(
        "Temenseite Ganztag der Bertelsmann-Stiftung",
        "Sammlung des Forschungsstandes zum Ganztag vom DJI",
        "Themenseite Ganztag auf Bildungsserver",
        "BMBF-Portal zur Ganztagsschule",
        "Bertelsmann 2",
        "Themenseite Ganztag der KMK"
      ),
      "Links_Link" = c(
        "https://www.bertelsmann-stiftung.de/de/unsere-projekte/in-vielfalt-besser-lernen/projektthemen/ganztag",
        "https://www.dipf.de/de/forschung/pdf-forschung/steubis/gts-bilanz_broschuere",
        "https://www.bildungsserver.de/ganztagsschule-1801-de.html#Studien_und_Untersuchungen_",
        "https://www.ganztagsschulen.org/de/forschung/einfuehrung/ganztagsschulforschung/ganztagsschulforschung-als-empirische-bildungsforschung_node.html",
        "https://www.bertelsmann-stiftung.de//de/publikationen/publikation/did/die-landesseitige-ausstattung-gebundener-ganztagsschulen-mit-personellen-ressourcen/",
        "https://www.kmk.org/themen/allgemeinbildende-schulen/bildungswege-und-abschluesse/ganztagsschulen-in-deutschland.html"
      )
    ),
    "circle_bildung_berufsorientierung" = list(
      "ID" = paste0("box_", 1:7),
      "Titel" = "Berufsorientierung fördern",
      "Untertitel" = "",
      "Einfuehrungstext" = "Die Förderung der Berufsorientierung ist essentiell, um Jugendlichen bei der Erkennung ihrer Stärken und Interessen zu helfen, was für eine erfüllende Karriereentscheidung wichtig ist. Sie trägt dazu bei, Fehlentscheidungen zu minimieren und bildet eine Brücke zwischen dem Bedarf des Arbeitsmarktes und qualifizierten Arbeitskräften. Zudem unterstützt eine effektive Berufsorientierung Chancengleichheit, indem sie allen Jugendlichen, unabhängig von ihrem Hintergrund, gleiche Zugangsmöglichkeiten bietet. Kurz gesagt, sie ist ein Schlüsselfaktor für die persönliche Entwicklung und hat weitreichende soziale und wirtschaftliche Vorteile.",
      "Indikator_Inhalt_IDs" = c(
        "bo1",
        "bo2",
        "bo3",
        "bo4",
        "bo5",
        "bo6",
        "bo7"
      ),
      "Indikator_Inhalt_UI" = c(
        function() monitor_indicator_main_content_ui("bo1", var_table = load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("bo2", var_table =load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("bo3", var_table =load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("bo4", var_table =load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("bo5", var_table =load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("bo6", var_table =load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_ui("bo7", var_table = load_table_by_variable_monitor(138))
      ),
      "Indikator_Inhalt_Server" = c(
        function() monitor_indicator_main_content_server("bo1", load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_server("bo2", load_table_by_variable_monitor(122)),
        function() monitor_indicator_main_content_server("bo3", load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_server("bo4", load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_server("bo5", load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_server("bo6", load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_server("bo7", load_table_by_variable_monitor(138))
      ),
      "Ueberschriften" = c(
        "Ausbau der Ganztagsangebote",
        "Vielfalt der Ganztagsangebote",
        "Kooperationen zwischen Schule und Zivilgesellschaft im Ganztag",
        "Sozialer Ausgleich bei Ausbau der Ganztagsangebote",
        "Multiprofessionele Teams an Schulen",
        "IT-Infrastruktur an Schulen",
        "Die Lage an den Schulen aus Sicht der Schulleitungen"
      ),
      "Fragen" = c(
        "Schaffen wir quantitativ genügend Ganztagsangebote?",
        "Schaffen wir vielfältige Ganztagsangebote?",
        "Schaffen wir Kooperationen zwischen Schule und Zivilgesellschaft?",
        "Schaffen wir es Ganztagsschule sozial einzuführen?",
        "Schaffen wir es multiprofessionelle Teams in den Schulen zu verankern?",
        "Schaffen wir die Voraussetzungen für digitale Bildungsangebote?",
        "Schaffen wir es die wahrgenomme Lage an den Schulen zu verbessern?"
      ),
      "Ziele" = c(
        "Schulen in Deutschland sind Ganztagsschulen",
        "Ganztagsunterricht wird vielseitig gestaltet",
        "Zivilgesellschaftlicher Akteure sind in den Ganztag eingebunden",
        "Insbesondere in Schulen mit niedrigem Sozialindex sind zivilgesellschaftliche Akteure eingebunden",
        "Multiprofessionelle Teams an Schulen",
        "Schulen haben WLAN",
        "Die Lage an Schulen wird als gut eingeschätzt"
      ),
      "Indikatoren" = c(
        "Anteil der Ganztagsschulen in Deutschland",
        "Vielfalt der Angebote im Rahmen der Ganztagsbetreuung",
        "Einbindung zivilgesellschaftlicher Akteure",
        "Einbindung zivilgesellschaftlicher Akteure nach Sozialindex",
        "Anteil Schulpersonal außerhalb der Lehrkräfte an Schulpersonal",
        "Anteil Schulen mit WLAN über 100 Mbit",
        "Anteil Schulleitungen die Lage an ihrer Schule insgesamt als gut bezeichnen"
      ),
      "Aktivitaeten" = c(
        "Studie: Vereine, Stiftungen und Co - die neuen Bildungspartner?",
        "Studie: Hochschul-Bildungs-Report",
        "Das Programm von Bildung und Begabung"
      ),
      "Aktivitaeten_Link" = c(
        "https://www.stifterverband.org/download/file/fid/6014",
        "https://www.hochschulbildungsreport.de/sites/hsbr/files/hochschul-bildungs-report_abschlussbericht_2022.pdf",
        "https://www.bildung-und-begabung.de/"
      ),
      "Datenbasis" = c(
        "KMK Schulen in Ganztagsform",
        "Studie zur Entwicklung von Ganztagsschulen (SteG)",
        "ZNL MINT-Angebote im schulischen Ganztag"
      ),
      "Datenbasis_Link" = c(
        "https://www.kmk.org/dokumentation-statistik/statistik/schulstatistik/allgemeinbildende-schulen-in-ganztagsform.html",
        "https://steg.dipf.de/de",
        "https://wp.znl-ulm.de/"
      ),
      "Links" = c(
        "Temenseite Ganztag der Bertelsmann-Stiftung",
        "Sammlung des Forschungsstandes zum Ganztag vom DJI",
        "Themenseite Ganztag auf Bildungsserver",
        "BMBF-Portal zur Ganztagsschule",
        "Bertelsmann 2",
        "Themenseite Ganztag der KMK"
      ),
      "Links_Link" = c(
        "https://www.bertelsmann-stiftung.de/de/unsere-projekte/in-vielfalt-besser-lernen/projektthemen/ganztag",
        "https://www.dipf.de/de/forschung/pdf-forschung/steubis/gts-bilanz_broschuere",
        "https://www.bildungsserver.de/ganztagsschule-1801-de.html#Studien_und_Untersuchungen_",
        "https://www.ganztagsschulen.org/de/forschung/einfuehrung/ganztagsschulforschung/ganztagsschulforschung-als-empirische-bildungsforschung_node.html",
        "https://www.bertelsmann-stiftung.de//de/publikationen/publikation/did/die-landesseitige-ausstattung-gebundener-ganztagsschulen-mit-personellen-ressourcen/",
        "https://www.kmk.org/themen/allgemeinbildende-schulen/bildungswege-und-abschluesse/ganztagsschulen-in-deutschland.html"
      )
    )
  )
}
