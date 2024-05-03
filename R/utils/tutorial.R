
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
      el          = "sb_explorer",
      title       = "Format: Daten Explorer",
      description = "In unserem SV Datenportal bieten wir Datenzugang über verschiedene
                    Datenprodukte und -formate. Insgesamt stehen derzeit vier Formate
                    zur Auswahl: Daten Explorer, Monitoring, Analysen und Projekte.
                    Ein einfacher Klick auf das Feld reicht aus, um ein Format auszuwählen.
                    <br><br>Schauen wir uns zunächst den <b>Daten Explorer</b> an:
                    <br><br>Der Daten Explorer richtet sich besonders an Menschen, die schnell und
                    unkompliziert auf spezifische Daten zugreifen möchten. Egal, ob Sie
                    gezielt nach Zahlen suchen oder Grafiken für Ihre Präsentation benötigen –
                    der Daten Explorer ist Ihre zentrale Anlaufstelle. Aber er bietet mehr als
                    nur schnellen Zugriff: Durch seine intuitive Bedienung und die intelligente
                    Verknüpfung von Datenpunkten werden Sie nicht nur schnell fündig, sondern
                    entdecken auch verwandte Inhalte, die für Ihre Recherche oder Ihr Projekt
                    von Interesse sein könnten.
                    <br><br>Nutzen Sie den Daten Explorer, um:<br>
                    <ul>
                    <li>Direkten Zugriff auf unsere umfangreiche Datenbank in den Themen Bildung, Wissenschaft, Innovation und Zivilgesellschaft zu erhalten.</li>
                    <li>Visualisierungen und Grafiken zu erstellen, die Daten auf einfache und verständliche Weise darstellen.</li>
                    <li>Neue Einsichten zu gewinnen, indem Sie verwandte Daten erkunden.</li>
                    </ul>
                    Ihnen steht dazu die direkte Suche nach Daten zur Verfügung, die Möglichkeit einzelne Indikatoren direkt zu
                    auszuwählen und Variablen miteinander in Beziehung zu setzen. Auch können Sie sich den passenden Datenpunkten vom
                    Datensatz nähren oder - wenn die Daten geografisch aufzuschlüsseln sind - sich entsprechend Karten ausgeben lassen."
    )$
    step(
      el          = "sb_monitor",
      title       = "Format Monitor",
      description = "In unserem <b>Monitoring</b> betrachten wir, wie sich ausgewählte Indikatoren in den
                    Handlungsfeldern des Stifterverbandes entwicklen. Ausgehend von unseren Fragen an
                    Politik, Bildungs-, Wissenschafts- und Innovationssystem sowie uns als Gesellschaft
                    versuchen wir Ziele und Indikatoren abzuleiten und zu monitoren.
                    Wir werden unseren Teil für ein Mehr an guter Bildung, exzellenter Wissenschaft und
                    Innovationsfreude beitragen, dennoch handelt es sich bei dem Monitoring nicht um
                    unseren eigenen Ziel- und Indikatorenkatalog, sondern um eine allgemeine Betrachtung zur
                    Entwicklung der Handlungsfelder. Das Monitoring ist vor allem für Menschen interessant, die
                    sich über die großen Entwicklungslinien im Bildungs- Wissenschafts und Innovationssystem informieren
                    möchten."
    )$
    step(
      el          = "sb_stories",
      title       = "Format Analysen",
      description = "Unter <b>Analysen</b> verstehen wir kuratierte Geschichten und Insights aus
                    unserer Datenarbeit. Filterbar nach unseren Handlungsfeldern und einem Taggingsystem
                    finden Sie hier eine Einordnungen unserer Zahlen sowie sich daraus ergebende Handlungsempfehlungen."
    )$
    step(
      el          = "sb_studies",
      title       = "Format Projekte",
      description = "Daten enstehen im Stifterverband meist projektbezogen: Eine Umfrage zu Gründungsförderung an
                    Hochschulen, unsere Studie zu Forschung und Entwicklung oder ein WebScraping-Projekt zu Vorlesungsverzeichnissen und vieles mehr.
                    Wenn Sie in Informationen zu einem spezifischen Projekt möchten, finden Sie unter dem Format <b>Projekte</b> Links zu unseren
                    einzelnen Studien und Datenseite.<br><br>Hier endet unser kleines Tutorial, der Rest sollte relativ selbsterklärend sein.
                    Wenn nicht: Zögern Sie nicht uns darauf hinzuweisen mit einer Mail an <a href='mailto:daten@stifterverband.de'> daten@stifterverband.de </a>.
                    Dann bauen wir die Erklärtexte und Hinweise entsprechend aus."
    )
  return(guide)
}
