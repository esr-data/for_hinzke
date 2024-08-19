rm(list = ls())
box::purge_cache()
box::use(
  ../R/utils/network[get_network_data, draw_network],
  shiny[shinyApp, renderDataTable, observeEvent],
  visNetwork[visNetworkOutput, renderVisNetwork, visNetworkProxy, visGetEdges, visGetNodes],
)

server <- function(input, output, session) {
  nodes <- data.frame(id = 1:3,
                      name = c("first", "second", "third"),
                      extra = c("info1", "info2", "info3"))
  edges <- data.frame(from = c(1,2), to = c(1,3), id= 1:2)

  output$network_proxy <- renderVisNetwork({
    draw_network(get_network_data())
  })


  output$nodes_data_from_shiny <- renderDataTable({
    if(!is.null(input$network_proxy_nodes)){
      print(unlist(input$network_proxy_nodes))
      test2 <<- unlist(input$network_proxy_nodes)
      info <- data.frame(matrix(unlist(input$network_proxy_nodes), ncol = dim(nodes)[1],
                                byrow=T),stringsAsFactors=FALSE)
      colnames(info) <- colnames(nodes)
      info <<- info
      info
    }
  })
  output$edges_data_from_shiny <- renderPrint({
    if(!is.null(input$network_proxy_edges)){
      input$network_proxy_edges
    }
  })

  observeEvent(input$getNodes,{
    visNetworkProxy("network_proxy") %>%
      visGetNodes()

    # visNetworkProxy("network_proxy") %>%
    #   visGetPositions()
    # # vals$coords <- if (!is.null(input$network_positions))
    #  test <<- input$network_positions
#
#     visNetworkProxy("network_proxy") %>%
#       visGet
  })

  observeEvent(input$getEdges, {
    visNetworkProxy("network_proxy") %>%
      visGetEdges()
  })
}

ui <- fluidPage(
  visNetworkOutput("network_proxy", height = "400px"),
  verbatimTextOutput("edges_data_from_shiny"),
  dataTableOutput("nodes_data_from_shiny"),
  actionButton("getEdges", "Edges"),
  actionButton("getNodes", "Nodes")
)

shinyApp(ui = ui, server = server)
