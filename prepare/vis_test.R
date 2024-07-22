
# library(DBI, quietly = TRUE)
# library(visNetwork, quietly = TRUE)
# con <- dbConnect(RSQLite::SQLite(), "data/magpie.sqlite")


daten <- get_network_data(con)
# 1 30 47
i <- i + 1
x <-
  draw_network(daten) |>
  visLayout(
    randomSeed = i
  )
x



nodes <- data.frame(id = 1:10, label = paste("Label", 1:10),
                    group = sample(c("A", "B"), 10, replace = TRUE))
edges <- data.frame(from = c(2,5,10), to = c(1,2,10))

visNetwork(nodes, edges) %>%
  visGroups(groupname = "A", color = "red", shape = "database") %>%
  visGroups(groupname = "B", color = "yellow", shape = "triangle") %>%
  visClusteringByGroup(groups = c("B"), label = "Group : ",
                       shape = "ellipse", color = "blue", force = TRUE) %>%
  visLegend()


visStartSimulation(draw_network(daten))
