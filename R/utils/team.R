box::use(
  shiny[
    div, a, img, p, h3
  ]
)

#' Missing description
#' @export

# ToDo: Designentscheidung: In Funktionen auf css-Classes im Style.css zugreifen oder
# an dieser Stelle lieber Inline-CSS, damit man die Funktionen flexibler zwischen
# Projekten kopieren kann? Derzeit sind Klassen in das Style.css integriert.

create_team_member <- function(name, email, img_source, position, website) {
  div(class = "col-md-4 team-member",
      a(href = website, target = "_blank",
        div(class = "team-card",
            img(src = img_source, alt = paste("Teammitglied", name)),
            h3(name),
            p(position),
            a(href = sprintf("mailto:%s", email), class = "mail-link", email)
        )
      )
  )
}

# .team-member img {
#   width: 100px;
#   height: 100px;
#   border-radius: 50%;
#   object-fit: cover;
# }
#
# .team-member {
#   text-align: center;
#   margin: 20px;
# }
#
# .team-card {
#   padding: 20px;
#   border: 1px solid #ddd;
#   border-radius: 8px;
#   transition: box-shadow .3s;
#   cursor: pointer;
# }
#
# .team-card:hover {
#   box-shadow: 0 0 11px rgba(33,33,33,.2);
# }
#
# .mail-link {
#   color: #007bff;
#     text-decoration: none;
# }
#
# .mail-link:hover {
#   text-decoration: underline;
# }
