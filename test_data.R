# Test Data

content_list_monitor_subpages_structure_full <-
  list(
    "bildung_ganztag" = list(
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
        function() monitor_indicator_main_content_server("ganztag_quantitativ", load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_server("ganztag_vielfalt", load_table_by_variable_monitor(122)),
        function() monitor_indicator_main_content_server("ganztag_kooperation", load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_server("ganztag_sozial", load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_server("ganztag_multiprofessionel", load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_server("ganztag_digital", load_table_by_variable_monitor(138)),
        function() monitor_indicator_main_content_server("ganztag_lage", load_table_by_variable_monitor(138))
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
    "bildung_berufsorientierung" = list(
      "Titel" = "Berufsorientierung stärken",
      "Ueberschriften" = c(
        "Berufsorientierung 1",
        "Berufsorientierung 2",
        "Berufsorientierung 3"
      ),
      "Fragen" = c(
        "Frage 1",
        "Frage 2",
        "Frage 3"
      ),
      "Ziele" = c(
        "Ziel 1",
        "Ziel 2",
        "Ziel 3"
      ),
      "Indikatoren" = c(
        "Indikator 1",
        "Indikator 2",
        "Indikator 3"
      ),
      "Aktivitäten" = c(
        "Berufsorientierung Aktivität 1",
        "Berufsorientierung Aktivität 2",
        "Berufsorientierung Aktivität 3"
      ),
      "Datenbasis" = c(
        "Berufsorientierung Datenbasis 1",
        "Berufsorientierung Datenbasis 2"
      ),
      "Links" = c(
        "Berufsorientierung Link 1",
        "Berufsorientierung Link 2",
        "Berufsorientierung Link 3"
      )
    )
  )
