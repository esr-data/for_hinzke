box::use(
  tibble[
    tibble, tribble
  ],
  stringr[
    str_replace_all
  ],
  dplyr[
    select, mutate, case_when
  ],
  tidyr[
    pivot_longer
  ],
  magrittr[
    `%>%`
  ]
)

# Indikator ganztag_vielfalt
#Quelle: https://www.dipf.de/de/forschung/pdf-forschung/llib/bericht-ganztagsschulen-2017-2018

#' Missing description
#' @export

give_df_ganztag_vielfalt_primar <- function() {
  tribble(
    ~Angebote, ~`2012`, ~`2015`, ~`2018`,
    "Hausaufgabenbetreuung", 92.80, 88.50, 87.80,
    "Aufgabenbezogene Lernzeiten", 38.80, 42.50, 44.10,
    "Förderunterricht/ Fördergruppen", 87.20, 81.20, 76.10,
    "Spezifische Fördermaßnahmen", 72.70, 72.50, 71.90,
    "Mathematische Angebote", 40.00, 32.60, 30.30,
    "Naturwissenschaftliche Angebote", 65.00, 58.20, 51.10,
    "Technische Angebote", 31.70, 32.20, 33.30,
    "Angebote zu neuen Medien", 67.70, 57.30, 47.60,
    "Deutsch, Literatur, Lesen", 77.40, 74.50, 71.20,
    "Fremdsprachen Angebote", 32.50, 30.60, 26.60,
    "Angebote zu Geschichte, Politik und Heimatkunde", 15.60, 10.30, 13.30,
    "Handwerkliche/ Hauswirtschaftliche Angebote", 80.90, 74.60, 76.70,
    "Musisch-künstlerische Angebote", 95.10, 93.00, 89.70,
    "Angebote zum sozialen Lernen", 73.80, 73.60, 70.70,
    "Sportliche Angebote", 98.70, 95.00, 94.00,
    "Spiel- und Denksportangebote", 73.80, 68.60, 67.60,
    "Angebote zu Gesundheit und Ernährung", 70.20, 68.90, 64.30
  )
}

#' Missing description
#' @export

give_df_ganztag_vielfalt_sek_I <- function() {
  tribble(
    ~Angebote, ~`2012`, ~`2015`, ~`2018`,
    "Hausaufgabenbetreuung", 81.10, 75.20, 68.90,
    "Aufgabenbezogene Lernzeiten", 64.50, 66.40, 66.10,
    "Förderunterricht/ Fördergruppen", 94.20, 94.70, 90.40,
    "Spezifische Fördermaßnahmen", 83.00, 81.50, 83.80,
    "Mathematische Angebote", 60.60, 56.40, 51.70,
    "Naturwissenschaftliche Angebote", 57.80, 59.50, 55.80,
    "Technische Angebote", 64.00, 59.40, 59.50,
    "Angebote zu neuen Medien", 73.00, 61.20, 59.70,
    "Deutsch, Literatur, Lesen", 71.40, 69.30, 66.60,
    "Fremdsprachen Angebote", 55.10, 51.40, 48.60,
    "Angebote zu Geschichte, Politik und Heimatkunde", 18.50, 15.40, 18.00,
    "Angebote zur Berufsorientierung", 68.90, 63.90, 58.80,
    "Handwerkliche/ Hauswirtschaftliche Angebote", 81.50, 79.60, 80.80,
    "Musisch-künstlerische Angebote", 91.40, 90.80, 92.60,
    "Angebote zum sozialen Lernen", 76.60, 76.30, 74.40,
    "Sportliche Angebote", 97.60, 97.10, 96.60,
    "Spiel- und Denksportangebote", 62.70, 56.30, 55.70,
    "Angebote zu Gesundheit und Ernährung", 62.00, 66.50, 59.80
  )
}

#' Missing description
#' @export

give_df_ganztag_vielfalt_gym <- function() {
  tribble(
    ~Angebote, ~`2012`, ~`2015`, ~`2018`,
    "Hausaufgabenbetreuung", 92.60, 89.70, 81.40,
    "Aufgabenbezogene Lernzeiten", 55.80, 53.90, 45.90,
    "Förderunterricht/ Fördergruppen", 95.20, 95.50, 89.20,
    "Spezifische Fördermaßnahmen", 73.90, 73.40, 76.00,
    "Mathematische Angebote", 80.20, 75.40, 73.30,
    "Naturwissenschaftliche Angebote", 87.00, 82.60, 81.70,
    "Technische Angebote", 47.30, 57.80, 59.70,
    "Angebote zu neuen Medien", 80.20, 71.50, 67.80,
    "Deutsch, Literatur, Lesen", 82.40, 76.60, 77.40,
    "Fremdsprachen Angebote", 80.20, 76.70, 70.00,
    "Angebote zu Geschichte, Politik und Heimatkunde", 33.30, 30.90, 30.00,
    "Angebote zur Berufsorientierung", 49.00, 43.10, 46.30,
    "Handwerkliche/ Hauswirtschaftliche Angebote", 44.50, 49.20, 53.10,
    "Musisch-künstlerische Angebote", 97.90, 97.80, 95.20,
    "Angebote zum sozialen Lernen", 66.40, 67.20, 60.80,
    "Sportliche Angebote", 97.50, 97.40, 98.80,
    "Spiel- und Denksportangebote", 64.70, 64.60, 60.30,
    "Angebote zu Gesundheit und Ernährung", 51.30, 40.90, 48.60
  )
}

# Indikator ganztag_kooperation
# Quelle:

#' Missing description
#' @export

give_df_ganztag_kooperation <- function() {
  tribble(
    ~Schulform, ~ja, ~nein,
    "insgesamt", 86, 12,
    "Grundschule", 85, 13,
    "Haupt-/ Real-/ Gesamtschule", 89, 9,
    "Gymnasium", 83, 16,
    "Förder-/ Sonderschule", 81, 19
  ) %>%
  pivot_longer(cols = c(ja, nein), names_to = "Auspraegung")
}

# Indikator ganztag_multiprofessionell
# Quelle: https://www.telekom-stiftung.de/sites/default/files/files/umfrage_multiprofessionalitaet_ergebnisbericht.pdf

#' Missing description
#' @export

give_df_ganztag_multiprofessionell <- function() {
  tibble::tribble(
    ~Bereich, ~Beschäftigungsgruppe, ~insgesamt, ~Grundschule, ~`Haupt- / Real-/ Gesamtschule`, ~Gymnasium, ~`Förder-/ Sonderschule`,
    "beschäftigte Lehrkräfte", "grundständig ausgebildete Lehrkräfte", 99, 99, 100, 100, 99,
    "beschäftigte Lehrkräfte", "Lehrkräfte im Vorbereitungsdienst", 70, 64, 76, 86, 71,
    "beschäftigte Lehrkräfte", "Quer-/Seiteneinsteiger außerhalb des MINT-Bereichs", 46, 36, 61, 45, 55,
    "beschäftigte Lehrkräfte", "Quer-/Seiteneinsteiger im MINT-Bereich", 22, 7, 44, 43, 7,
    "beschäftigte Lehrkräfte", "Assistenzlehrkräfte, wie etwa von Teach First", 15, 13, 17, 7, 25,
    "Beschäftigung weiterer pädagogischer Fachkräfte", "Sozialpädagog/innen, Sozialarbeiter/innen", 73, 66, 91, 69, 66,
    "Beschäftigung weiterer pädagogischer Fachkräfte", "Schulbegleiter/innen, Integrationshelfer/innen, Inklusionsassistenten o.ä.", 65, 67, 70, 36, 83,
    "Beschäftigung weiterer pädagogischer Fachkräfte", "Sonderpädagog/innen, Lerntherapeut/innen", 43, 44, 48, 12, 55,
    "Beschäftigung weiterer pädagogischer Fachkräfte", "Erzieher/innen", 37, 44, 28, 11, 59,
    "Beschäftigung weiterer pädagogischer Fachkräfte", "Ergo-, Physiotherapeuten/innen, Logopäden/innen o.ä.", 8, 3, 6, 2, 46,
    "Beschäftigung weiterer pädagogischer Fachkräfte", "Psychologen/innen", 6, 3, 9, 12, 7,
    "Beschäftigung weiterer pädagogischer Fachkräfte", "keine Beschäftigten aus dem Bereich weitere pädagogische Fachkräfte", 9, 12, 4, 15, 3,
    "Beschäftigung nicht-pädagogischer Fachkräfte", "Verwaltungskräfte, Sekretariat", 94, 94, 93, 93, 96,
    "Beschäftigung nicht-pädagogischer Fachkräfte", "Hausmeister/in", 91, 88, 94, 94, 93,
    "Beschäftigung nicht-pädagogischer Fachkräfte", "Ganztagsschulkoordinator/in, Kooperationskoordinator/in", 14, 14, 18, 9, 8,
    "Beschäftigung nicht-pädagogischer Fachkräfte", "IT-Fachkraft", 11, 7, 19, 16, 6,
    "Beschäftigung nicht-pädagogischer Fachkräfte", "(Schul-)Bibliothekar/Teacher Librarian", 7, 2, 10, 28, 2,
    "Beschäftigung nicht-pädagogischer Fachkräfte", "Verwaltungsleitung/Geschäftsführung (zusätzlich zur Schulleitung)", 6, 3, 10, 9, 6,
    "Beschäftigung nicht-pädagogischer Fachkräfte", "Zusätzliche Verwaltungsassistenz", 5, 3, 7, 13, 4,
    "Beschäftigung nicht-pädagogischer Fachkräfte", "Bildungstechnologe bzw. Educational Technologist", 0, 0, 1, 0, 0,
    "Beschäftigung von Fachkräften im MINT-Bereich", "Techniker, etwa für Aufbau/Pflege von Sammlungen", 1, 1, 0, 2, 0,
    "Beschäftigung von Fachkräften im MINT-Bereich", "MINT-Assistenten", 1, 0, 0, 3, 0,
    "Beschäftigung von Fachkräften im MINT-Bereich", "Laboranten", 0, 0, 1, 0, 0,
    "Beschäftigung von Fachkräften im MINT-Bereich", "keine Beschäftigten aus dem Bereich Fachkräfte im MINT-Bereich", 97, 97, 96, 95, 98
  ) %>%
  mutate(
    is_header = !duplicated(Bereich),
    Bereich = as.factor(Bereich)
  ) %>%
  select(-is_header)
}

# Indikator ganztag_lage Arbeitsmotivation Schulleitung
# Quelle: https://deutscher-schulleitungskongress.de/wp-content/uploads/2023/11/2023-11-21_VOe-Nov_Bericht_Deutschland.pdf

#' Missing description
#' @export

give_df_ganztag_lage_arbeitsmotivation_schulleitungen <- function() {
  tibble(
    Jahr = c("2019  ", "Feb 2020  ", "Nov 2020  ", "2021  ", "2022  ", "2023  "),
    sehr_gern = c(58, 42, 24, 30, 30, 37),
    eher_gern = c(38, 47, 48, 45, 49, 46),
    ungern = c(4, 11, 27, 25, 20, 16),
    weiß_nicht = c(0, 0, 1, 0, 1, 1)
  ) %>%
  mutate(gern = sehr_gern + eher_gern) %>%
  select(-c(sehr_gern, eher_gern)) %>%
  pivot_longer(cols = -Jahr, names_to = "Kategorie", values_to = "Prozent") %>%
  mutate(
    Kategorie = case_when(
      Kategorie == "gern" ~ "sehr/eher gern",
      Kategorie == "ungern" ~ "sehr/eher ungern",
      Kategorie == "weiß_nicht" ~ "weiß nicht"
    ),
    Jahr = factor(Jahr, levels = c("2023  ", "2022  ", "2021  ", "Nov 2020  ", "Feb 2020  ", "2019  ")),
    Kategorie = factor(Kategorie, levels = c("sehr/eher gern", "weiß nicht", "sehr/eher ungern"))
  )
}

# Indikator ganztag_lage Weiterempfehlung Schulleitungsberuf
# Quelle: https://deutscher-schulleitungskongress.de/wp-content/uploads/2023/11/2023-11-21_VOe-Nov_Bericht_Deutschland.pdf

#' Missing description
#' @export

give_df_ganztag_lage_weiterempfehlung_schulleiterberuf <- function() {
  tibble(
    Jahr = c("2018  ", "2019  ", "2020  ", "2021  ", "2022  ", "2023  "),
    auf_jeden_Fall = c(24, 20, 14, 11, 10, 11),
    wahrscheinlich = c(49, 50, 46, 40, 36, 38),
    wahrscheinlich_nicht = c(22, 22, 30, 36, 41, 38),
    auf_keinen_Fall = c(1, 5, 6, 10, 9, 8),
    weiß_nicht = c(4, 3, 4, 3, 4, 5)
  ) %>%
  pivot_longer(cols = -Jahr, names_to = "Kategorie", values_to = "Prozent") %>%
  mutate(
    Kategorie = str_replace_all(Kategorie, "_", " "),
    Jahr = factor(Jahr, levels = c("2023  ", "2022  ", "2021  ", "2020  ", "2019  ", "2018  ")),
    Kategorie = factor(Kategorie, levels = c("auf jeden Fall", "wahrscheinlich", "weiß nicht", "wahrscheinlich nicht", "auf keinen Fall"))
  )
}

# Indikator gerechtigkeit Gymansiumsahrscheinlichkeit
# Quelle: https://www.ifo.de/DocDL/sd-2024-05-ungleiche-bildungschancen-woessmann-etal-.pdf

#' Missing description
#' @export

give_df_gerechtigkeit_trichter_gymnasiumswahrscheinlichkeit <- function() {
  tibble(
    Land = c(
      "Berlin", "Brandenburg", "Rheinland-Pfalz", "Saarland",
      "Mecklenburg-Vorpommern", "Hamburg", "Baden-Württemberg",
      "Niedersachsen", "Thüringen", "Nordrhein-Westfalen",
      "Schleswig-Holstein", "Sachsen-Anhalt", "Hessen", "Bremen",
      "Sachsen", "Bayern", "Deutschland"
    ),
    Chancenverhaeltnis = c(
      53.8, 52.8, 52.2, 50.8, 50.1, 47.1, 47.1,
      45.7, 44.4, 44.1, 43.6, 43.3, 42.1, 41.5,
      40.1, 38.1, 44.6
    )
  )
}
