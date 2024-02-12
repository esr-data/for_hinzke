
#' Missing description
#' @export

write_log <- function(x, warn = FALSE){
  type <- "LOG"
  if (warn) type <- "WARNUNG"
  message(sprintf("%s [%s]: %s", type, format(Sys.time(), "%a %d %b %Y %X"), x))
  return(invisible(NULL))
}
