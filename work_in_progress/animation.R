



df <-
  svWrangling::get_data(
    variable = "Anteil internat. Studiengänge",
    time_period_start = "2017",
    time_period_end = "2020"
  )

df <-
  svWrangling::get_data(
    variable = "Interne FuE-Aufwendungen",
    group = "Sektor",
    time_period = c("1983", "2021")
  )

library(ggplot2)
library(gganimate)

df$wert <- as.numeric(df$wert)
p <-
  ggplot(df, aes(y = wert, x = zeit, color = Sektor)) +
  geom_line() +
  scale_y_continuous(breaks = c(0,20,40,60) * 1000000, labels = c(0,20,40,60)) +
  theme(legend.position = "none", axis.title = element_blank()) +
  transition_reveal(zeit) +
  ease_aes('linear')
animate(
  p,
  duration = 15,
  fps = 20, width = 1400, height = 865, res = 500,
  renderer = gifski_renderer(loop = FALSE), rewind = FALSE)
anim_save("work_in_progress/filenamehere.gif")



p <-
  ggplot(df, aes(y = wert, x = Sektor, color = Sektor)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = c(0,20,40,60) * 1000000, labels = c(0,20,40,60)) +
  theme(legend.position = "none", axis.title = element_blank()) +
  transition_states(
    year,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out')
animate(
  p,
  duration = 15,
  fps = 20, width = 1400, height = 865, res = 500,
  renderer = gifski_renderer(loop = FALSE), rewind = FALSE)
anim_save("work_in_progress/filenamehere.gif")



a <- data.frame(group=c("A","B","C"), values=c(3,2,4), frame=rep('a',3))
b <- data.frame(group=c("A","B","C"), values=c(5,3,7), frame=rep('b',3))
data <- rbind(a,b)

# Basic barplot:
ggplot(a, aes(x=group, y=values, fill=group)) +
  geom_bar(stat='identity')

# Make a ggplot, but add frame=year: one image per year
p <- ggplot(data, aes(x=group, y=values, fill=group)) +
  geom_bar(stat='identity') +
  theme_bw() +
  # gganimate specific bits:
  transition_states(
    frame,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('sine-in-out')
animate(
  p,
  duration = 15,
  fps = 20, width = 1400, height = 865, res = 500,
  renderer = gifski_renderer(loop = FALSE), rewind = FALSE)
# Save at gif:
anim_save("288-animated-barplot-transition.gif")
