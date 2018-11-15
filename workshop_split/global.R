require(shiny)
require(shinydashboard)
require(dplyr)
require(plotly)
# require(tidyquant)
require(leaflet)
require(igraph)
require(DT)
require(visNetwork)
require(shinyjs)
require(prophet)
require(lubridate)
require(shinycssloaders)

boroughs <- readRDS("source-files/boroughs.rds")
msoa <- readRDS("source-files/msoa.rds")
pal <- colorNumeric("viridis", NULL)
# symbolData <- read.csv("ftp://ftp.nasdaqtrader.com/SymbolDirectory/nasdaqlisted.txt",
#                        sep="|") %>% 
#   mutate(Company.Name = trimws(gsub(" Inc| - |Class A|Common Stock|Series A|[[:punct:]]", "", word(Security.Name, 1, 3)), which = "both"))
# 
# NYSE <- tq_exchange("NYSE") %>%
#   filter(sector %in% c("Technology",
#                        "Consumer Services",
#                        "Health Care",
#                        "Finance",
#                        "Public Utilities",
#                        "Transportation",
#                        "Energing")) %>%
#   mutate(company_value = case_when(grepl("M", market.cap) ~ as.numeric(gsub("\\$|M", "", market.cap)) * 1000000,
#                                    grepl("B", market.cap) ~ as.numeric(gsub("\\$|B", "", market.cap)) * 1000000000)) %>%
#   group_by(sector) %>%
#   mutate(sector_value = sum(na.omit(company_value))) %>%
#   top_n(15, wt = company_value) %>%
#   ungroup() %>%
#   arrange(desc(company_value))
# 
# NASDAQ <- tq_exchange("NASDAQ") %>%
#   filter(!is.na(ipo.year),
#          sector %in% c("Technology",
#                        "Consumer Services",
#                        "Health Care",
#                        "Finance",
#                        "Public Utilities",
#                        "Transportation",
#                        "Energing")) %>%
#   mutate(company_value = case_when(grepl("M", market.cap) ~ as.numeric(gsub("\\$|M", "", market.cap)) * 1000000,
#                                    grepl("B", market.cap) ~ as.numeric(gsub("\\$|B", "", market.cap)) * 1000000000)) %>%
#   group_by(sector) %>%
#   mutate(sector_value = sum(na.omit(company_value))) %>%
#   top_n(15, wt = company_value) %>%
#   ungroup() %>%
#   arrange(desc(company_value))

# saveRDS(NASDAQ, "source-files/NASDAQ.rds")
NASDAQ <- readRDS("source-files/NASDAQ.rds")

getStockData <- function(ticker,
                         from,
                         to) {
  
  dt <- tq_get(ticker,
               from = from,
               to = to)
  
  return(dt)
  
}

# NAS <- getStockData("AAPL",
#                     from = Sys.Date() - 730,
#                     to = Sys.Date()) %>% 
#   mutate(symbol = "AAPL",
#          company = "Apple Inc.")

# lapply(2:nrow(NASDAQ), function(x) {
#   
#   row <- NASDAQ[x,]
#   
#   symbol <- row['symbol'][[1]]
#   company <- row['company'][[1]]
#   
#   print(paste(x, 'of 90 -', company))
#   
#   int <- getStockData(symbol,
#                       from = Sys.Date() - 730,
#                       to = Sys.Date()) %>% 
#     mutate(symbol = row['symbol'][[1]],
#            company = row['company'][[1]])
#   
#   NAS <<- rbind(NAS,
#                 int)
#   
# })

# saveRDS(NAS, 'source-files/stockdata.rds')
stock_data <- readRDS('source-files/stockdata.rds')

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

# time <- getStockData(ticker = "AMZN",
#                      from = Sys.Date()-30,
#                      to = Sys.Date()) %>% 
#   mutate(average = (high + low) / 2)
