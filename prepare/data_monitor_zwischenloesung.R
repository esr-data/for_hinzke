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
  ],
  ../R/pkgs/svVis/give_ger_federal_states[give_ger_federal_states],
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

give_df_bildung_ganztag_kooperation <- function() {
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

# Indikator minternational_wimis Anteil ausländischer WiMis in MINT
# Quelle: Destatis - Wissenschaftliches und künstlerisches Personal

#' Missing description
#' @export

give_df_minternational_wimis <- function() {
  data <- tribble(
    ~Fachbereich, ~`2008_gesamt`, ~`2009_gesamt`, ~`2010_gesamt`, ~`2011_gesamt`, ~`2012_gesamt`, ~`2013_gesamt`, ~`2014_gesamt`, ~`2015_gesamt`, ~`2016_gesamt`, ~`2017_gesamt`, ~`2018_gesamt`, ~`2019_gesamt`, ~`2020_gesamt`, ~`2021_gesamt`, ~`2022_gesamt`, ~`2023_gesamt`, ~`2008_ausl`, ~`2009_ausl`, ~`2010_ausl`, ~`2011_ausl`, ~`2012_ausl`, ~`2013_ausl`, ~`2014_ausl`, ~`2015_ausl`, ~`2016_ausl`, ~`2017_ausl`, ~`2018_ausl`, ~`2019_ausl`, ~`2020_ausl`, ~`2021_ausl`, ~`2022_ausl`, ~`2023_ausl`, ~`2008_Anteil`, ~`2009_Anteil`, ~`2010_Anteil`, ~`2011_Anteil`, ~`2012_Anteil`, ~`2013_Anteil`, ~`2014_Anteil`, ~`2015_Anteil`, ~`2016_Anteil`, ~`2017_Anteil`, ~`2018_Anteil`, ~`2019_Anteil`, ~`2020_Anteil`, ~`2021_Anteil`, ~`2022_Anteil`, ~`2023_Anteil`,
    "Wiss./künstl. Personal insgesamt (ohne zentrale Einrichtungen)", 243360, 256296, 280850, 302293, 313766, 327566, 342288, 352431, 355544, 356049, 365250, 371520, 376634, 384170, 394914, 395720, 22787, 23738, 26929, 29350, 30851, 32493, 34454, 36787, 38534, 41181, 42817, 44689, 46816, 49997, 53431, 57055, 9.36, 9.26, 9.59, 9.71, 9.83, 9.92, 10.07, 10.44, 10.84, 11.57, 11.72, 12.03, 12.43, 13.01, 13.53, 14.42,
    "MINT", 87832, 92771, 102947, 112309, 117204, 123381, 128663, 132266, 132924, 132203, 136600, 137820, 139465, 141009, 142874, 142426, 9662, 10670, 12451, 13420, 14150, 14985, 15852, 16789, 17314, 17741, 18858, 19945, 21096, 22552, 24184, 25835, 11.00, 11.50, 12.09, 11.95, 12.07, 12.15, 12.32, 12.69, 13.03, 13.42, 13.81, 14.47, 15.13, 15.99, 16.93, 18.14,
    "Alle Nicht MINT-Fächer", 155528, 163525, 177903, 189984, 196562, 204185, 213625, 220165, 222620, 223846, 228650, 233700, 237169, 243161, 252040, 253294, 13125, 13068, 14478, 15930, 16701, 17508, 18602, 19998, 21220, 23440, 23959, 24744, 25720, 27445, 29247, 31220, 8.44, 7.99, 8.14, 8.38, 8.50, 8.57, 8.71, 9.08, 9.53, 10.47, 10.48, 10.59, 10.84, 11.29, 11.60, 12.33,
    "Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin", 6841, 7066, 7340, 7747, 8080, 8291, 8555, 8512, 8677, 8634, 9198, 9346, 9299, 9372, 9655, 9601, 490, 488, 536, 589, 641, 710, 765, 758, 841, 815, 968, 1071, 1139, 1249, 1370, 1443, 7.16, 6.91, 7.30, 7.60, 7.93, 8.56, 8.94, 8.91, 9.69, 9.44, 10.52, 11.46, 12.25, 13.33, 14.19, 15.03,
    "Geisteswissenschaften", 38260, 39304, 41101, 43784, 45226, 47188, 49586, 50936, 35771, 34832, 35179, 34694, 34565, 34604, 35123, 34123, 4195, 4271, 4541, 4984, 5198, 5347, 5709, 5956, 5517, 5378, 5648, 5691, 5733, 5891, 6130, 6383, 10.96, 10.87, 11.05, 11.38, 11.49, 11.33, 11.51, 11.69, 15.42, 15.44, 16.05, 16.40, 16.59, 17.02, 17.45, 18.71,
    "Humanmedizin/Gesundheitswissenschaften", 51060, 53838, 55576, 58189, 59902, 61798, 64218, 65272, 66572, 67777, 70220, 72842, 74936, 79405, 81816, 83962, 4771, 4413, 4802, 5337, 5580, 5837, 6343, 6839, 7352, 8045, 8678, 9278, 9864, 10876, 11492, 12247, 9.34, 8.20, 8.64, 9.17, 9.31, 9.45, 9.87, 10.48, 11.04, 11.87, 12.36, 12.74, 13.16, 13.70, 14.05, 14.59,
    "Ingenieurwissenschaften", 37540, 39703, 44919, 50222, 52164, 55865, 58824, 61140, 76628, 76040, 79042, 80511, 81528, 83506, 85051, 85122, 3328, 3730, 4557, 4909, 5107, 5553, 5893, 6309, 8349, 8371, 8915, 9468, 10075, 10970, 11825, 12758, 8.87, 9.39, 10.14, 9.77, 9.79, 9.94, 10.02, 10.32, 10.90, 11.01, 11.28, 11.76, 12.36, 13.14, 13.90, 14.99,
    "Kunst, Kunstwissenschaft", 15891, 16188, 16801, 17165, 17754, 17977, 18905, 19226, 19300, 19557, 19564, 20135, 20091, 20586, 21056, 21172, 1638, 1728, 1796, 1983, 2090, 2114, 2235, 2327, 2430, 2571, 2706, 2839, 2928, 3041, 3177, 3454, 10.31, 10.67, 10.69, 11.55, 11.77, 11.76, 11.82, 12.10, 12.59, 13.15, 13.83, 14.10, 14.57, 14.77, 15.09, 16.31,
    "Mathematik, Naturwissenschaften", 50292, 53068, 58028, 62087, 65040, 67516, 69839, 71126, 56296, 56163, 57558, 57309, 57937, 57503, 57823, 57304, 6334, 6940, 7894, 8511, 9043, 9432, 9959, 10480, 8965, 9370, 9943, 10477, 11021, 11582, 12359, 13077, 12.59, 13.08, 13.60, 13.71, 13.90, 13.97, 14.26, 14.73, 15.92, 16.68, 17.27, 18.28, 19.02, 20.14, 21.37, 22.82,
    "Rechts-, Wirtschafts- und Sozialwissenschaften", 40928, 44523, 54273, 60134, 62608, 65821, 69121, 72858, 88766, 89424, 90870, 92940, 94427, 95445, 100282, 100387, 1979, 2124, 2736, 2959, 3086, 3406, 3443, 3988, 4918, 6473, 5791, 5692, 5856, 6194, 6890, 7489, 4.84, 4.77, 5.04, 4.92, 4.93, 5.17, 4.98, 5.47, 5.54, 7.24, 6.37, 6.12, 6.20, 6.49, 6.87, 7.46,
    "Sport", 2548, 2606, 2812, 2965, 2992, 3110, 3240, 3361, 3534, 3622, 3619, 3743, 3851, 3749, 4108, 4049, 52, 44, 67, 78, 106, 94, 107, 130, 162, 158, 168, 173, 200, 194, 188, 204, 2.04, 1.69, 2.38, 2.63, 3.54, 3.02, 3.30, 3.87, 4.58, 4.36, 4.64, 4.62, 5.19, 5.17, 4.58, 5.04,
    "Wiss./künstl. Personal insgesamt (inkl. zentrale Einrichtungen)", 260064, 274769, 301042, 324367, 337102, 353690, 369847, 381269, 385311, 386752, 394878, 402058, 406659, 414832, 427698, 428457, 24904, 26107, 29661, 32431, 34212, 36116, 38474, 41010, 43129, 45858, 47537, 49601, 51828, 55176, 59337, 63078, 9.58, 9.50, 9.85, 9.99, 10.15, 10.21, 10.40, 10.76, 11.19, 11.86, 12.04, 12.34, 12.74, 13.30, 13.87, 14.72
  )

  data_long <- data %>%
    pivot_longer(
      cols = starts_with("20"),
      names_to = c("jahr", "kategorie"),
      names_sep = "_",
      values_to = "value"
  )

  data_long_filtered <- dplyr::filter(data_long, kategorie == "Anteil")

  return(data_long_filtered) # memo, hab hier schonmal auch die realen Werte für Magpi mitgenommen, wenngleich im Monitor nur die Anteile benötigt werden, Zwischenlösung ließe sich hier hins. Performence also noch verbessern, für Übertragung in Mapie so aber vermutlich einfacher, die VArs sind Wissenschaftliches und Künstlerisches Personal an Hochschulen insgesamt, ausländisches Wissenschaftliches und Künstlerisches Personal an Hochschulen und Anteil ausländisches wissenschftlcihes künsterisches Perosnal an wissenschaftlichem und Künstlerischem Personal an Hochschulen insgesamt
}

# Indikator teilhabe_mint_stundentafeln Anteil MINT in Stundentafeln
# Quelle: Informatik-Monitor, Landesschulgesetze, eigene Berechnungen

#' Missing description
#' @export

give_df_teilhabe_mint_stundentafeln <- function() {
  tribble(
    ~Region, ~wochenstunden_mint, ~wochenstunden_insgesamt, ~anteil_wochenstunden_mint,
    "Brandenburg",	50,	195,	25.6,
    "Berlin",	54,	195,	27.7,
    "Baden-Württemberg",	50,	199,	25.1,
    "Bayern",	47,	184,	25.5,
    "Bremen",	49,	196,	25.0,
    "Hessen",	45,	179,	25.1,
    "Hamburg",	43,	197,	21.8,
    "Mecklenburg-Vorpommern",	48,	195,	24.6,
    "Niedersachsen",	48,	181,	26.5,
    "Nordrhein-Westfalen",	47,	182,	25.8,
    "Rheinland-Pfalz",	49,	180,	27.2,
    "Schleswig-Holstein",	46,	176,	26.1,
    "Saarland",	48,	178,	27.0,
    "Sachsen",	57,	200,	28.5,
    "Sachsen-Anhalt",	55,	196,	28.1,
    "Thüringen",	52,	197,	26.4,
    "DE_G8",	458,	1770,	25.9,
    "DE_G9",	285,	1081,	26.4,
    "Zusammen",	743,	2851,	26.1
  ) %>%
    select(Region, anteil_wochenstunden_mint) # performance in zwischenlösung geht besser, weil mehr infos geladen werden als benötigt, aber für Magpie sicher schön alles drin zu haben
}


# Indikator teilhabe_mint_lehrkraefte Anteil Lehrkräfte in Informatik
# Quelle: Informatik-Monitor

#' Missing description
#' @export

give_df_teilhabe_mint_lehrkraefte <- function() {
  tibble(
    Region = c(give_ger_federal_states("alphabet"), "Deutschland"),
    `Anteil Informatiklehrkraefte` = c("2 %", "2 %", "3 %", "3 %", "1 %", "3 %", "—", "7 %", "1 %", "2 %", "3 %", "—", "9 %", "3 %", "2 %", "2 %", "2 %"),
    `davon weiblich` = c("38 %", "32 %", "31 %", "33 %", "36 %", "—", "—", "47 %", "24 %", "31 %", "26 %", "—", "37 %", "48 %", "25 %", "33 %", "-")
  )
}

# Indikator teilhabe_mint_pflichtfach_inf
# Quelle: Informatik-Monitor

#' Missing description
#' @export

give_df_teilhabe_mint_pflichtfach_inf <- function() {
  tibble(
    Region = give_ger_federal_states("alphabet"),
    kategorie = c(
      "1-2 Pflichtstunden Informatik an allen Schulformen",
      "1-2 Pflichtstunden Informatik an allen Schulformen",
      "Informatikangebote an einzelnen Schulformen oder in einzelnen Jahrgangsstufen",
      "Informatikangebote an einzelnen Schulformen oder in einzelnen Jahrgangsstufen",
      "kein Angebot",
      "Informatikangebote an einzelnen Schulformen oder in einzelnen Jahrgangsstufen",
      "Informatikangebote an einzelnen Schulformen oder in einzelnen Jahrgangsstufen",
      "5—6 Pflichtstunden Informatik an allen Schulformen",
      "1-2 Pflichtstunden Informatik an allen Schulformen",
      "1-2 Pflichtstunden Informatik an allen Schulformen",
      "Informatikangebote an einzelnen Schulformen oder in einzelnen Jahrgangsstufen",
      "5—6 Pflichtstunden Informatik an allen Schulformen",
      "3—4 Pflichtstunden Informatik an allen Schulformen",
      "Informatikangebote an einzelnen Schulformen oder in einzelnen Jahrgangsstufen",
      "Informatikangebote an einzelnen Schulformen oder in einzelnen Jahrgangsstufen",
      "Informatikangebote an einzelnen Schulformen oder in einzelnen Jahrgangsstufen"
    )
  )
}

# Indikator lehramt_fs_medienkompetenz
# Quelle: Lehrkräfte Monitor

#' Missing description
#' @export

give_df_lehrkraefte_fs_medienkompetenz <- function() {
  tibble(
    Schulform = rep(c("Grundschule/Primarstufe", "Sekundarstufe I", "Sekundarstufe II/allgemeinbildend", "Sekundarstufe II/beruflich", "Sonderpädagogik"), times = 2),
    Jahr = factor(c(rep(2017, times = 5), rep(2022, times = 5))),
    Wert = c(13.2, 14.6, 13.2, 16.7, 0, 63.6, 60.5, 50, 56.5, 52)
  )
}
