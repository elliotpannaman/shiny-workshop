require(shiny)
require(shinydashboard)
require(dplyr)
require(plotly)
require(leaflet)
require(igraph)
require(DT)
require(visNetwork)
require(shinyjs)
require(prophet)
require(lubridate)
require(shinycssloaders)

boroughs <- readRDS("boroughs.rds")
msoa <- readRDS("msoa.rds")
pal <- colorNumeric("viridis", NULL)
NASDAQ <- readRDS("NASDAQ.rds")
stock_data <- readRDS('stockdata.rds')

getStockData <- function(ticker,
                         from,
                         to) {
  
  dt <- tq_get(ticker,
               from = from,
               to = to)
  
  return(dt)
  
}

makeGraph <- function(n, children) {
  
  make_tree(n,
            children) %>% 
    set_vertex_attr("group", 
                    value = sample(c("x", "y", "z"), 
                                   60,
                                   replace = TRUE)) %>% 
    set_vertex_attr("size", 
                    value = sample(1:3*30, 
                                   60,
                                   replace = TRUE))
  
  
}

plotGraph <- function(graph) {
  
  visIgraph(graph,
            layout = "layout_as_tree",
            circular = TRUE) %>% 
    visLegend()
  
}