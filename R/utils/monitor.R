
box::use(
  rlist[list.append, list.rbind],
  yaml[read_yaml]
)


#' Missing description
#' @noRd

get_content_monitor <- function(){
  output <- list()
  yml_path <- "yml/monitor"
  for (i in list.files(yml_path)){
    new_output <- read_yaml(file.path(yml_path, i))
    for (j in c("Inhalt", "Aktivitaet", "Datenbasis", "Link")){
      new_output[[j]] <-
        new_output[[j]] |>
        list.rbind() |>
        as.data.frame()
    }
    output <- list.append(output, new_output)
  }
  names(output) <- unlist(lapply(output, \(x) x$ID))
  return(output)
}
