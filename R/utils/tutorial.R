
box::use(
  cicerone[Cicerone]
)

#' Missing description
#' @noRd

plan_tutorial_tour <- function(){
  guide <- Cicerone$
    new(
      done_btn_text  = "Fertig",
      close_btn_text = "Schließen",
      next_btn_text  = "weiter",
      prev_btn_text  = "zurück",
    )$
    step(
      el          = "sb_handlung1",
      title       = "Handlungsfelder",
      description = "Alle Inhalte können nach zwei Handlungsfeldern gefiltert werden. Ein einfacher Klick auf das Feld reicht hierfür aus. Ein zweiter Klick hebt den Filter wieder auf. Das erste Handlungsfeld ist <b><a style = 'color: black' href='https://www.stifterverband.org/bildung-kompetenzen' target='_blank'>Bildung & Kompetenzen</a></b>. Ist kein Handlungsfeld ausgewählt, werden alle Inhalte angezeigt."
    )$
    step(
      el          = "sb_handlung2",
      title       = "Handlungsfelder",
      description = "Das zweite Handlungsfeld ist <b><a style = 'color: black' href='https://www.stifterverband.org/forschung-innovation' target='_blank'>kollaborative Forschung & Innovation</a></b>."
    )$
    step(
      el          = "sb_stories",
      title       = "Stories",
      description = "Stories sind kuratierte Inhalte ... work in progress."
    )
  return(guide)
}


