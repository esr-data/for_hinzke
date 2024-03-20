
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
  format(roundFive(x, digits), decimal.mark = ",", big.mark = ".", nsmall = 0)
}

convert_zeit <- function(x, to_warn = TRUE, return_data_frame = FALSE){
  x$zeit <- NA
  x$zeit <- ifelse(x$zeit_einheit %in% "Jahr",  substr(x$zeit_ende, 1, 4), x$zeit)
  x$zeit <- ifelse(x$zeit_einheit %in% "Jahre", paste(substr(x$zeit_start, 1, 4), substr(x$zeit_ende, 1, 4), sep = "-"), x$zeit)
  if (any(is.na(x$zeit)) & to_warn) warning("Konvertierung des Zeitformates: Unbekannte Zeiteinheit!")

  if (!return_data_frame) return(x$zeit)

  x$zeit_start <- x$zeit
  x$zeit_ende  <- NULL
  x$zeit       <- NULL
  names(x)[names(x) == "zeit_start"] <- "zeit"
  return(x)
}

format_wert_if_numeric <- function(x){
  checkmate::assert_atomic_vector(x)
  lapply(
    x,
    \(.){
      if (!is.na(suppressWarnings(as.numeric(.)))){
        formatNumeric(as.numeric(.))
      } else {
        .
      }
    }
  ) |>
    unlist() |>
    trimws()
}
