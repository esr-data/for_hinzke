
box::use(
  shiny[div, icon, p],
)


#' Missing description
#' @export

draw_under_construction <- function(){
  div(
    style = "display: flex; color: var(--red); font-size: 30px; margin-top: 30px;",
    style = "display: flex; color: var(--red); font-size: 30px; margin-top: 30px;",
    icon(
      style = "margin-right: 10px;",
      "screwdriver-wrench"
    ),
    p("Under construction ...")
  )
}
