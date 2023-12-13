
box::use(
  cicerone[Cicerone]
)

#' Missing description
#' @noRd

plan_tutorial_tour <- function(){
  guide <- Cicerone$
    new()$
    step(
      el = "sb_home",
      title = "Text Input",
      description = "This is where you enter the text you want to print."
    )$
    step(
      "sb_handlung1",
      "Send the Text",
      "Send the text to the server for printing"
    )
  return(guide)
}
