give_ger_federal_states <- function(order = c("alphabet", "destatis")) {

  if (!order %in% c("alphabet", "destatis")) {
    stop("Invalid order-choice. Please choose 'alphabet' or 'destatis'.")
  }

  alphabet <- c(
    "Baden-W\u00fcrttemberg",
    "Bayern",
    "Berlin",
    "Brandenburg",
    "Bremen",
    "Hamburg",
    "Hessen",
    "Mecklenburg-Vorpommern",
    "Niedersachsen",
    "Nordrhein-Westfalen",
    "Rheinland-Pfalz",
    "Saarland",
    "Sachsen",
    "Sachsen-Anhalt",
    "Schleswig-Holstein",
    "Th\u00fcringen"
  )

  destatis <- c(
    "Schleswig-Holstein",
    "Hamburg",
    "Niedersachsen",
    "Bremen",
    "Nordrhein-Westfalen",
    "Hessen",
    "Rheinland-Pfalz",
    "Baden-W\u00fcrttemberg",
    "Bayern",
    "Saarland",
    "Berlin",
    "Brandenburg",
    "Mecklenburg-Vorpommern",
    "Sachsen",
    "Sachsen-Anhalt",
    "Th\u00fcringen"
  )

  if(order == "alphabet") {
    return(alphabet)
  } else if(order == "destatis") {
    return(destatis)
  }

}
