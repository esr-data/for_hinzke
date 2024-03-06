
roundFive <- function(x, digits = 0){
  if (!is.numeric(x) | !is.vector(x))
    stop("Cannot round because x is not a numeric vector!")
  if (!is.numeric(digits) & digits < 0)
    stop("Cannot round because digits is not a positive number!")
  digits <- trunc(digits)
  vorzeichen <- sign(x)
  x <- abs(x) * 10^digits
  x <- x + 0.5
  x <- trunc(x)
  x <- x/10^digits
  return(x * vorzeichen)
}

formatNumeric <- function(x, digits = 2){
  format(roundFive(x, digits), decimal.mark = ",", big.mark = ".")
}

convert_zeit <- function(x, to_warn = TRUE){
  x$zeit <- NA
  x$zeit <- ifelse(x$zeit_einheit == "Jahr", substr(x$zeit_ende, 1, 4), x$zeit)
  if (any(is.na(x$zeit)) & to_warn) warning("Konvertierung des Zeitformates: Unbekannte Zeiteinheit!")
  x$zeit_start <- x$zeit
  x$zeit_ende  <- NULL
  x$zeit       <- NULL
  names(x)[names(x) == "zeit_start"] <- "zeit"
  return(x)
}
