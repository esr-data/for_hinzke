
library(ggplot2)
library(cowplot)

# --- FUE-Erhebung ---------------------------------------------------------------------------------

df <-
  data.frame(
    Sektor = c("Staat", "Hochschulen", "Wirtschaft", "Private Org."),
    Wert   = c(.37,.57, 2.11, .07)
  )
df$Sektor <- factor(df$Sektor, levels = df$Sektor[c(4,1,2,3)])

g <-
  ggplot(df, aes(x = 1, y = Wert)) +
  geom_bar(aes(fill = Sektor), stat = "identity", position = "stack", width = 3.5) +
  scale_x_discrete(labels = df$Sektor) +
  scale_fill_manual(values = c(keaVis::col_grey(), keaVis::col_greyblue(), keaVis::col_beige(), keaVis::col_blue())) +
  scale_y_continuous(
    limits = c(-1, 3.6),
    breaks = c(0:4),
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
  ) +
  svVis:::get_theme() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(hjust = 1, size = 20, family = "Calibri"),
    axis.text.x = element_text(vjust = 0, size = 18, family = "Calibri")
  ) +
  geom_hline(yintercept = 3.5, size = 1.5, color = keaVis::col_orange(), linetype = "dotted")

g <-
  ggdraw() +
  draw_plot(g, -.1, -.38, .8, 1.5) +
  # draw_line(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0), color = "blue", size = 2) +
  draw_text(
    "Wirtschaft",
    0.65, 0.12,
    colour = keaVis::col_blue(),
    size = 18,
    fontface = "bold", family = "Calibri", hjust = 0
  )  +
  draw_text(
    "2,11%",
    0.65, 0.06,
    colour = keaVis::col_blue(),
    size = 18,
    fontface = "bold", family = "Calibri", hjust = 0
  ) +
  draw_text(
    "Hochschulen",
    0.65, 0.67,
    colour = keaVis::col_beige(),
    size = 18,
    fontface = "bold", family = "Calibri", hjust = 0
  ) +
  draw_text(
    "0,57%",
    0.65, 0.61,
    colour = keaVis::col_beige(),
    size = 18,
    fontface = "bold", family = "Calibri", hjust = 0
  )  +
  draw_text(
    "Staat 0,37%",
    0.65, 0.76,
    colour = keaVis::col_greyblue(),
    size = 18,
    fontface = "bold", family = "Calibri", hjust = 0
  )  +
  draw_text(
    "PNP 0,07%",
    0.65, 0.85,
    colour = keaVis::col_grey(),
    size = 18,
    fontface = "bold", family = "Calibri", hjust = 0
  ) +
  draw_text(
    "3,5%-Ziel",
    0.65, 0.94,
    colour = keaVis::col_orange(),
    size = 18,
    fontface = "bold", family = "Calibri", hjust = 0
  )

ggsave(
  "www/img/studie_fue_alt.svg",
  g,
  width  = 1200,
  height = 1200,
  units  = "px",
  dpi    = 300,
  scale  = 1
)

rm(df, g)


# --- PRIMUS-PREIS ---------------------------------------------------------------------------------

df <-
  data.frame(
    Antwort = c("Ja", "Nein", "k.A."),
    Anzahl = c(11, 12, 4)
  )

df$Antwort <- factor(df$Antwort, levels = df$Antwort)

g <-
  ggplot(df, aes(x = Antwort, y = Anzahl)) +
  geom_col(aes(fill = Antwort)) +
  scale_x_discrete(labels = df$Antwort) +
  scale_fill_manual(values = c(keaVis::col_blue(), keaVis::col_orange(), keaVis::col_grey())) +
  scale_y_continuous(
    limits = c(-1, 12.5),
    breaks = c(0:3 * 4),
    labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
  ) +
  svVis:::get_theme() +
    theme(
      legend.position = "none",
      axis.text.y = element_text(hjust = 1, size = 18, family = "Calibri"),
      axis.text.x = element_text(vjust = 0, size = 18, family = "Calibri")
    )

g <-
  ggdraw() +
  draw_plot(g, -.1, -.01, 1.2, 1.15) +
  draw_text(
    "Neue Partner gewonnen?",
    0.5, 0.05,
    colour = keaVis::col_blue(),
    size = 18,
    fontface = "bold", family = "Calibri"
  )

ggsave(
  "www/img/studie_primus_alt.svg",
  g,
  width  = 1200,
  height = 1200,
  units  = "px",
  dpi    = 300,
  scale  = .7
)

rm(df, g)

# --- Stiftungsprofessoren -------------------------------------------------------------------------

df <-
  data.frame(
    Jahr   = c(2016:2021),
    Anzahl = c(488, 478, 492, 463, 428, 386)
  )

g <-
  ggplot(df[df$Jahr > 2018,], aes(x = Jahr, y = Anzahl)) +
  geom_col(fill = "#C3DA46") +
  geom_text(aes(label = Anzahl, y = Anzahl - 40), family = "Calibri", size = 5, color = "white") +
  scale_fill_manual(values = c(keaVis::col_blue(), keaVis::col_orange(), keaVis::col_grey())) +
  scale_y_continuous(
    limits = c(-30,520)#,
    # breaks = c(0:3 * 4)
  ) +
  svVis:::get_theme() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),# element_text(hjust = 1, size = 14, family = "Calibri"),
    axis.text.x = element_text(vjust = 0, size = 16, family = "Calibri"),
    axis.title.y = element_blank()
  )

g <-
  ggdraw() +
  draw_plot(g, -.1, .075, 1.2, 1.15) +
  draw_text(
    "Professuren\naus der Wirtschaft",
    0.5, 0.1,
    colour = keaVis::col_blue(),
    size = 18,
    fontface = "bold",
    family = "Calibri",
    lineheight = .75
  )

ggsave(
  "www/img/studie_professur_alt.svg",
  g,
  width  = 1200,
  height = 1200,
  units  = "px",
  dpi    = 300,
  scale  = .7
)
# --- DRITTMITTEL ----------------------------------------------------------------------------------

df <-
  svMagpie::get_query(
    "SELECT variable, EXTRACT(YEAR FROM zeit_start) as jahr, CAST(wert AS numeric) FROM view_daten
     WHERE
     zeit_start = '2021-01-01' AND
     region IS NOT NULL AND
     region IN ('Deutschland') AND
     reichweite IS NULL AND
     variable IN ('Drittmittel von andere internationalen Organisationen',
                  'Drittmittel vom Bund',
                  'Drittmittel von der Bundesanstalt für Arbeit',
                  'Drittmittel von der DFG',
                  'Drittmittel insgesamt',
                  'Drittmittel von der EU',
                  'Drittmittel von Gemeinden und Zweckverbänden',
                  'Drittmittel von Hochschulfördergesellschaften',
                  'Drittmittel der Länder (ohne Trägermittel)',
                  'Drittmittel des sonstigen öffentlichen Bereichs',
                  'Drittmittel von Stiftungen',
                  'Drittmittel von der Wirtschaft')"
  )

# 'Drittmittel von DFG-Einzelförderungen',
# 'Drittmittel von DFG-Koordinierten Programmen',
# 'Drittmittel von von der DFG-Exzellenzstrategie',
# 'Drittmittel von sonstigen DFG-Förderungen'

df$wert_relativ <- df$wert / df$wert[df$variable == "Drittmittel insgesamt"] * 100
df <- df[df$variable != "Drittmittel insgesamt",]

df$count <- df$wert_relativ
df$fraction = df$count / sum(df$count)
df$ymax = cumsum(df$fraction)
df$ymin = c(0, head(df$ymax, n=-1))
library(ggplot2)
ggplot(
  df,
  aes(
    fill = variable,
    ymax = ymax,
    ymin = ymin,
    xmax = 3.5, xmin = 3
  )
) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(0, 3.5)) +
  labs(
    title = str_c(stri_wrap(plot_title, width = 160), collapse = "\n"), subtitle = paste(plot_subtitle,
                                                                                                  "\n\n"), caption = str_c("N = ", sum(donut_plot_data[["n"]]),
                                                                                                                           ". ", "Quelle: ", source, "."), fill = "") + theme_pubr() +
  theme(text = element_text(size = 8, color = "#195365",
                            family = "Trebuchet MS"), axis.text = element_blank(),
        axis.ticks = element_blank(), axis.line = element_blank(),
        legend.position = c(0.5, 0.5), legend.background = element_rect(fill = alpha("white",
                                                                                     1)), plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "italic")) +
  scale_fill_manual(values = color_set)
