---
title: "Workshop Documentation"
output: 
  html_document:
    keep_md: TRUE
editor_options: 
  chunk_output_type: console
---



***

### Agenda

#### Objective 1: Build your own visualisation (keep it simple or go nuts)
#### Objective 2: Deploy your app on Shinyapps.io
  
**1) Basics (15 mins)**  

- Why did RStudio create Shiny?
- [HMRC example](https://dataexploitation.shinyapps.io/localversion/)
- get something running with mtcars and ggplot
- run through sample dashboard code

**2) Visualisations (30 mins)**

- leaflet (need boundaries for london with a sample deprivation set)
- plotly (need tidy_quant working)
- igraph (need basic data structure)
- pick one and add it to your site

**3) Advanced (15 mins)**

- reactive function
- observeEvent example
- shinyjs change css on tab change

**4) Deploy (10 mins)**

- what's needed to deploy
- options
- get something on shared shinyapps

**5) Common problems (10 mins)**

- file structures
- too much reactivity
- feeding data in parallel
- package versions
- tracking usage with Google Analytics

**Potential Open Datasets**

- [IMDB Open Data](https://www.imdb.com/interfaces/)
- [UK Open Geography Portal](http://geoportal.statistics.gov.uk/datasets?q=Census%20Boundaries&sort=name)
- [Google Dataset Search](https://toolbox.google.com/datasetsearch)
- [Inspiration](https://shiny.rstudio.com/gallery/)

***

### Shiny Basics

Taken from: [Rstudio Tutorial](https://shiny.rstudio.com/articles/basics.html)
<br>

**Single App File**  
Shiny apps have two main components, the *user interface* (file saved as ui.R) and the *reactive server* (file saved as server.R).


```r
library(shiny)

# Build the frontend UI

ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Build the backend server

server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(from = min(x), 
                to = max(x), 
                length.out = input$bins + 1)
    
    hist(x, breaks = bins, 
         col = "#75AADB", 
         border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

# Run it!

shinyApp(ui = ui, 
         server = server)
```
  
**ui.R**  
The user interface script holds all information on the static (permanent) design and layout of the app.

There are a variety of different packages and pre-set designs available to use - to name a few:

- navBarPage - *good for tabs*
- fluidPage - *good when using on different device types*
- dashboardpage - *general purpose*

Each of these designs allows you to easily create a sidebar/mainPanel layout for easy dashboard use.

This is where you tell Shiny where to position all visible elements:

- An *input* is a reference for `Shiny` to take the value chosen by the user to change another element on the page (usually added on the ui.R side)
- An *output* is a widget, plot, table, map, chart or input box (tip of the iceberg!) that can change depending on any other input on the page
- The server script (see below) handles all *interactive elements* (inputs and outputs), updating them on user changes (AKA reactivity)

*Note: It is possible to make pretty much any ui element reactive by adding an Output function (with an id) on the ui side, and then building it out on the server side.*


```r
header <- dashboardHeader(title = "Shiny Workshop")

sidebar <- dashboardSidebar(
  
  sidebarMenu(id = "main_menu",
              
              menuItem("London Deprivation",
                       tabName = "leaflet",
                       icon = icon("map")),
              
              menuItem("Graph Network",
                       tabName = "visnetwork",
                       icon = icon("link")),
              
              menuItem("Stocks Prediction",
                       tabName = "tidyquant",
                       icon = icon("university")),
              
              br(),
              uiOutput("filters")
              
  )
  
)

body <- dashboardBody(
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  tabItems(
    
    tabItem(tabName = "leaflet",
            
            fluidRow(
              box(status = "primary",
                  width = 12,
                  leafletOutput("map") %>% withSpinner()
              )),
            
            fluidRow(
              box(status = "primary",
                  width = 12,
                  DT::dataTableOutput("maptable")
              ))
            
    ),
    
    tabItem(tabName = "visnetwork",
            
            fluidRow(
              box(status = "primary",
                  width = 12,
                  column(width = 9,
                         visNetworkOutput("network") %>% withSpinner()
                  ),
                  column(width = 3,
                         sliderInput("visn",
                                     "Number of nodes",
                                     min = 5,
                                     max = 100,
                                     value = 20),
                         sliderInput("vischildren",
                                     "Number of children",
                                     min = 1,
                                     max = 10,
                                     value = 2))
              )),
            
            fluidRow(
              box(status = "primary",
                  width = 12,
                  column(width = 12,
                         DT::dataTableOutput("vistable"))
              ))
    ),
    
    tabItem(tabName = "tidyquant",
            
            fluidRow(
              box(status = "primary",
                  width = 12,
                  uiOutput("quant_viz"),
                  uiOutput("quant_predict_options")
              )),
            
            fluidRow(
              box(status = "primary",
                  width = 12,
                  DT::dataTableOutput("quant_table"))
            )
    )
  )
  
)

ui <- dashboardPage(header, sidebar, body)
```
   
**server.R**  
As mentioned above, the server element of the Shiny app exists to make it *interactive*.

In order to reference *clicks/choices/hovers/selections* from other elements in the app, use *input\$id* (e.g. "input\$bins" as  below)

You can prevent elements from updating each time or make them wait for dependent inputs before rendering by using the `isolate()` and `req()` functions


```r
server <- function(input, output, session) {
  
  # sidebar -----------------------------------------------------------------
  
  output$filters <- renderUI({
    
    if (input$main_menu == "leaflet") {
      
      output = tagList()
      
      output[[length(output) + 1]] = selectizeInput(inputId = "map_indicator",
                                                    label = "Deprivation Index",
                                                    choices = c("Income" = "income",
                                                                "Population" = "population"),
                                                    selected = "population")
      
      output[[length(output) + 1]] = sliderInput(inputId = "map_timeline",
                                                 label = "Deprivation Timeline",
                                                 min = 0,
                                                 max = 10,
                                                 value = 2)
      
    } else if (input$main_menu == "visnetwork") {
      
      output = tagList()
      
      output[[length(output) + 1]] = selectizeInput("vistype",
                                                    label = "Complexity:",
                                                    choices = c("Just Nodes & Edges" = "simple",
                                                                "Colors & Groups Too" = "complex"),
                                                    selected = NULL)
      
    } else if (input$main_menu == "tidyquant") {
      
      output = tagList()
      
      symbols <- setNames(as.character(NASDAQ$symbol),
                          as.character(NASDAQ$company))
      
      output[[length(output) + 1]] = selectizeInput(inputId = "quant_sector",
                                                    label = "Choose a sector:",
                                                    choices = list(Sectors = unique(NASDAQ$sector)),
                                                    selected = "Technology")
      
      output[[length(output) + 1]] = selectizeInput(inputId = "quant_choose",
                                                    label = "Choose a company:",
                                                    choices = list(Stocks = symbols),
                                                    selected = symbols[1])
      
      output[[length(output) + 1]] = dateRangeInput("quant_timeline",
                                                    "Date Range",
                                                    min = today() - 365,
                                                    max = today(),
                                                    start = today() - 90,
                                                    end = today())
      
      output[[length(output) + 1]] = actionButton("quant_predict",
                                                  label = textOutput("quant_type"))
      
    }
    
    output
    
  })
  
  
  # leaflet -----------------------------------------------------------------
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB) %>%
      setView(lat = 51.527179,
              lng = -0.127713,
              zoom = 9.25) %>%
      addGeoJSON(boroughs,
                 layerId = "feature.properties.cartodb_id")

    
  })
  
  observeEvent(input$map_click, {
    
    print(input$map_click)
    
  })
  
  output$maptable <- DT::renderDataTable({
    
    search <- ifelse(is.null(input$map_shape_click),
                     "",
                     gsub("\\s{1}[[:digit:]]+", "", input$map_shape_click))
    
    DT::datatable(msoa@data[,c("msoa11nm",
                               "RGN17NM",
                               "country",
                               "totalMales",
                               "totalFemales",
                               "FemRatio")],
                  colnames = c('MSOA',
                               'Region',
                               'Country',
                               'Males',
                               'Females',
                               'Gender Ratio'),
                  rownames = FALSE,
                  options = list(dom = 'ft',
                                 search = list(regex = TRUE,
                                               search = search)))
    
  })
  
  
  # igraph ------------------------------------------------------------------
  
  output$network <- renderVisNetwork({
    
    if (input$vistype == "simple") {
      
      make_tree(input$visn,
                input$vischildren) %>%
        visIgraph(layout = "layout_as_tree",
                  circular = TRUE)
      
    } else if (input$vistype == "complex") {
      
      makeGraph(input$visn,
                input$vischildren) %>%
        plotGraph()
      
    }
    
  })
  
  output$vistable <- DT::renderDT({
    
    makeGraph(input$visn,
              input$vischildren) %>%
      as_long_data_frame() %>%
      datatable()
    
  })
  
  
  # tidyquant ---------------------------------------------------------------
  
  stockData <- reactive({
    
    # return(getStockData(input$quant_choose,
    #                     input$quant_timeline[1],
    #                     input$quant_timeline[2]))
    
    return(stock_data %>% 
             filter(symbol == input$quant_choose,
                    date >= input$quant_timeline[1],
                    date <= input$quant_timeline[2]))
    
  })
  
  stockCompany <- reactive({
    
    return(NASDAQ[NASDAQ$symbol == input$quant_choose, "company"])
    
  })
  
  quant_range <- reactive({
    
    return(input$timeline[2] - input$timeline[1])
    
  })
  
  observeEvent(input$quant_sector, {
    
    symbols <- setNames(NASDAQ[NASDAQ$sector %in% input$quant_sector,]$symbol,
                        NASDAQ[NASDAQ$sector %in% input$quant_sector,]$company)
    
    updateSelectizeInput(session,
                         inputId = "quant_choose",
                         label = "Choose a company:",
                         choices = list(Stocks = symbols),
                         selected = symbols[1])
    
  })
  
  output$quant_predict_options <- renderUI({
    
    if (input$quant_predict[1] %% 2 == 1) {
      
      sliderInput(inputId = "quant_predict_time",
                  label = "Prediction timeline:",
                  min = 7,
                  max = quant_range() * 2,
                  value = quant_range() / 3,
                  step = 15,
                  width = "100%",
                  post = "days")
      
    }
    
  })
  
  output$quant_type <- renderText({
    
    if (input$quant_predict[1] %% 2 == 0) {
      
      "Make a prediction"
      
    } else {
      
      "View stock changes"
      
    }
    
  })
  
  output$quant_candles <- renderPlotly({
    
    if (length(input$quant_choose) > 1) {
      
      g <- lapply(input$quant_choose, function(x) {
        
        stockData() %>%
          plot_ly(type = "candlestick",
                  x = ~date,
                  open = ~open,
                  close = ~close,
                  high = ~high,
                  low = ~low,
                  height = 600) %>%
          layout(title = "",
                 showlegend = FALSE,
                 annotations = list(x = 0.5,
                                    y = 1.2,
                                    text = x,
                                    showarrow = FALSE,
                                    xref = 'paper',
                                    yref = 'paper'),
                 margin = list(t = 20,
                               b = 20)) %>%
          config(displayModeBar = FALSE)
        
      })
      
      g %>%
        subplot(nrows = round(length(input$quant_choose) / 2, 0)) %>%
        config(displayModeBar = FALSE)
      
    } else {
      
      stockData() %>%
        plot_ly(type = "candlestick",
                x = ~date,
                open = ~open,
                close = ~close,
                high = ~high,
                low = ~low) %>%
        layout(title = paste(stockCompany(), "Stock Movements"),
               showlegend = FALSE) %>%
        config(displayModeBar = FALSE)
      
    }
    
  })
  
  output$quant_timeseries <- renderPlotly({
    
    st <- stockData() %>%
      rename(ds = date,
             y = adjusted) %>%
      mutate(yhat_lower = NA,
             yhat_upper = NA) %>%
      select(c('ds', 'y', 'yhat_lower', 'yhat_upper')) %>%
      mutate(ds = as.Date(ds))
    
    m <- prophet(st,
                 yearly.seasonality = TRUE,
                 weekly.seasonality = TRUE)
    
    future <- make_future_dataframe(m, periods = input$quant_predict_time)
    
    forecast <- predict(m, future)
    
    forecast <- forecast %>%
      select(c('ds', 'yhat', 'yhat_lower', 'yhat_upper')) %>%
      filter(ds > Sys.Date()) %>%
      rename(y = yhat)
    
    st <- st %>%
      rbind(forecast) %>%
      mutate(color = ifelse(ds <= Sys.Date(),
                            'rgb(22, 96, 167)',
                            'rgb(220,41,13)'))
    
    plot_ly(data = st[st$ds <= Sys.Date(),],
            x = ~ds,
            y = ~y,
            type = 'scatter',
            name = "Existing",
            mode = 'lines',
            line = list(color = ~color,
                        width = 2)) %>%
      add_trace(data = st[st$ds > Sys.Date(),],
                y = ~yhat_upper,
                name = 'Upper',
                line = list(color = 'rgba(0,100,80,0.2)',
                            width = 2)) %>%
      add_trace(data = st[st$ds > Sys.Date(),],
                x = ~ds,
                y = ~y,
                type = 'scatter',
                name = "Prediction",
                mode = 'lines',
                line = list(color = ~color,
                            width = 4),
                fill = 'tonexty',
                fillcolor = 'rgba(0,100,80,0.2)') %>%
      add_trace(data = st[st$ds > Sys.Date(),],
                y = ~yhat_lower,
                name = 'Lower',
                line = list(color = 'rgba(0,100,80,0.2)',
                            width = 2),
                fill = 'tonexty',
                fillcolor = 'rgba(0,100,80,0.2)') %>%
      layout(showlegend = FALSE,
             title = paste(stockCompany(), "Stock Prediction"),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Adjusted Price ($)")) %>%
      config(displayModeBar = FALSE)
    
  })
  
  output$quant_table <- DT::renderDT({
    
    multiple_stocks <- lapply(input$quant_choose, function(company) {
      
      stockData()
      
    })
    
    do.call("rbind", multiple_stocks) %>%
      arrange(desc(date))
    
  })
  
  observeEvent(input$quant_predict, {
    
    output$quant_viz <- renderUI({
      
      if (input$quant_predict[1] %% 2 == 0) {
        
        plotlyOutput("quant_candles")
        
      } else {
        
        plotlyOutput("quant_timeseries")
        
      }
      
    })
    
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
}
```
   
**global.R**  
Objects defined in global.R are similar to those defined in app.R outside of the server function definition, with one important difference: they are also visible to the code in the ui object. This is because they are loaded into the global environment of the R session; all R code in a Shiny app is run in the global environment or a child of it.

In practice, there aren’t many times where it’s necessary to share variables between server and ui. The code in ui is run once, when the Shiny app is started and it generates an HTML file which is cached and sent to each web browser that connects. This may be useful for setting some shared configuration options.


```r
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

boroughs <- readRDS("source-files/boroughs.rds")
msoa <- readRDS("source-files/msoa.rds")
pal <- colorNumeric("viridis", NULL)
NASDAQ <- readRDS("source-files/NASDAQ.rds")
stock_data <- readRDS('source-files/stockdata.rds')

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
```
<br>
     
**app.R**  
Now it's time to call the app.


```r
# If running in a single file:

# - Copy and paste all three elements above into one file (put the global stuff at the top)
# - Run the script with this at the bottom:

shinyApp(ui, server)

# If running with a split file structure (use this option for deploying to shinyapps.io!):

# - Save the ui.R, server.R and global.R scripts along with all dependences in a folder
# - Use runApp() anywhere or in an app.R file:

runApp("path_to_folder_name")
```

***

### Leaflet (Maps)

Taken from: [Leaflet Documentation](https://rstudio.github.io/leaflet/shiny.html)

The Leaflet package includes powerful and convenient features for integrating with Shiny applications.

Most Shiny output widgets are incorporated into an app by including an output (e.g. plotOutput) for the widget in the UI definition, and using a render function (e.g. renderPlot) in the server function. Leaflet maps are no different; in the UI you call leafletOutput, and on the server side you assign a renderLeaflet call to the output. Inside the renderLeaflet expression, you return a Leaflet map object.

**Markers**
Use markers to call out points on the map. Marker locations are expressed in latitude/longitude coordinates, and can either appear as icons or as circles.


```r
# global -----------------------------------------------------------------

library(leaflet)

# ui -----------------------------------------------------------------

leafletOutput("mymap"),
p(),
actionButton("recalc", "New points")

# server -----------------------------------------------------------------

# Generate random points on click
points <- eventReactive(input$recalc, {
  cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
}, ignoreNULL = FALSE)

output$mymap <- renderLeaflet({
  
  # Open leaflet
  leaflet() %>%
    
    # Change background with provider tiles
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    
    # Add icons
    addMarkers(data = points())
})
```
<br>

**Shapes**
Leaflet makes it easy to take spatial lines and shapes from R and add them to maps.


```r
# global -----------------------------------------------------------------

library(rgdal)

# From https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
states <- readOGR("shp/cb_2013_us_state_20m.shp",
                  layer = "cb_2013_us_state_20m", GDAL1_integer64_policy = TRUE)

# server -----------------------------------------------------------------

neStates <- subset(states, states$STUSPS %in% c(
  "CT","ME","MA","NH","RI","VT","NY","NJ","PA"
))

leaflet(neStates) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))
```
<br>

**Modifying Existing Maps with leafletProxy**

This works, but reactive inputs and expressions that affect the renderLeaflet expression will cause the entire map to be redrawn from scratch and reset the map position and zoom level.

For some situations that may be acceptable or desirable behavior. But in other situations, you may want finer-grained control over the map, such as changing the color of a single polygon or adding a marker at the point of a click – without redrawing the entire map.

To modify a map that’s already running in the page, you use the leafletProxy() function in place of the leaflet() call, but otherwise use Leaflet function calls as normal.


```r
  observe({
    proxy <- leafletProxy("map", data = quakes)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    
    if (input$legend) {
      
      pal <- colorpal()
      
      proxy %>% addLegend(position = "bottomright",
        pal = pal, values = ~mag
        
      )
    }
  })
```
<br>

**Inputs/Events**

The map itself also has a few input values/events.

- **input$MAPID_click** is an event that is sent when the map background or basemap is clicked. The value is a list with lat and lng.

- **input$MAPID_bounds** provides the latitude/longitude bounds of the currently visible map area; the value is a list() that has named elements north, east, south, and west.

- **input$MAPID_zoom** is an integer that indicates the zoom level.

- **input$MAPID_center** provides the latitude/longtitude of the center of the currently visible map area; the value is a list() that has named elements lat and lng.

***

### iGraph

Taken from: [iGraph Documentation](https://datastorm-open.github.io/visNetwork/)

**Graph Dataset Structure**

```r
library(visNetwork)

nodes <- data.frame(id = 1:15)
edges <- data.frame(from = sample(1:15,10), to = sample(1:15,10))

visNetwork(nodes, edges)
```

<!--html_preserve--><div id="htmlwidget-9c9f8bec8b78c5bd6369" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-9c9f8bec8b78c5bd6369">{"x":{"nodes":{"id":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]},"edges":{"from":[12,13,5,4,15,2,3,9,6,11],"to":[9,8,12,3,2,15,6,1,4,7]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

**Nodes & Edges**

Nodes and edges must be in separate dataframes, with at least one column id. The edges dataset needs from and to columns, which make the link with id of nodes. You can add properties simply by adding variables on data.frame.


```r
# global ------------------------------------------------------------------

library(visNetwork)
library(igraph)
library(shiny)

# ui ------------------------------------------------------------------

actionButton("network-recalc",
             label = "Refresh")

visNetworkOutput("network")

# server ------------------------------------------------------------------

nnodes <- eventReactive(input$network-recalc, { sample(50:1000, 1) })
nnedges <- reactive({ sample(nnodes():1000, 1) })

renderVisNetwork({
  
  nodes <- data.frame(id = 1:nnodes)
  edges <- data.frame(from = sample(1:nnodes, nnedges, replace = T),
                      to = sample(1:nnodes, nnedges, replace = T))
  
  visNetwork(nodes, edges, height = "500px") %>%
    visIgraphLayout() %>%
    visNodes(size = 10)
  
})
```
<br>

**Classification Tree**

```r
library(rpart)
library(sparkline)

data("solder")

res <- rpart(Opening~., data = solder, control = rpart.control(cp = 0.00005))
visTree(res, height = "800px", nodesPopSize = TRUE, minNodeSize = 10, 
        maxNodeSize = 30, width = "100%")
```

<!--html_preserve--><div id="htmlwidget-e914e498abb8ad58f2d8" style="width:100%;height:800px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-e914e498abb8ad58f2d8">{"x":{"nodes":{"id":[1,2,4,8,16,17,34,35,70,71,9,18,36,72,73,37,19,38,76,77,39,78,156,157,79,5,10,20,40,80,81,162,324,325,163,41,82,164,165,330,331,83,21,42,84,168,169,85,43,11,22,44,45,90,91,23,46,92,184,185,370,371,93,47,3,6,12,24,48,49,25,13,26,52,53,27,7],"label":["skips","skips","PadType","Mask","L","PadType","L","Solder","L","M","Mask","Panel","Panel","L","M","M","Solder","Mask","L","M","PadType","Panel","L","M","L","Solder","skips","PadType","skips","L","PadType","Mask","L","M","S","PadType","Panel","L","Panel","L","M","M","Mask","PadType","Mask","L","M","M","S","Mask","PadType","M","skips","L","S","PadType","Mask","PadType","M","skips","M","S","S","S","Mask","skips","Solder","skips","L","M","S","skips","PadType","L","S","S","S"],"level":[1,2,3,4,5,5,6,6,7,7,4,5,6,7,7,6,5,6,7,7,6,7,8,8,7,3,4,5,6,7,7,8,9,9,8,6,7,8,8,9,9,7,5,6,7,8,8,7,6,4,5,6,6,7,7,5,6,7,8,8,9,9,7,6,2,3,4,5,6,6,5,4,5,6,6,5,3],"color":["#F1B8C2","#F1B8C2","#DAC49C","#A9D1A5","#7D91B6","#DAC49C","#7D91B6","#8AD3D0","#7D91B6","#AC83AE","#A9D1A5","#B4C7ED","#B4C7ED","#7D91B6","#AC83AE","#AC83AE","#8AD3D0","#A9D1A5","#7D91B6","#AC83AE","#DAC49C","#B4C7ED","#7D91B6","#AC83AE","#7D91B6","#8AD3D0","#F1B8C2","#DAC49C","#F1B8C2","#7D91B6","#DAC49C","#A9D1A5","#7D91B6","#AC83AE","#B9828C","#DAC49C","#B4C7ED","#7D91B6","#B4C7ED","#7D91B6","#AC83AE","#AC83AE","#A9D1A5","#DAC49C","#A9D1A5","#7D91B6","#AC83AE","#AC83AE","#B9828C","#A9D1A5","#DAC49C","#AC83AE","#F1B8C2","#7D91B6","#B9828C","#DAC49C","#A9D1A5","#DAC49C","#AC83AE","#F1B8C2","#AC83AE","#B9828C","#B9828C","#B9828C","#A9D1A5","#F1B8C2","#8AD3D0","#F1B8C2","#7D91B6","#AC83AE","#B9828C","#F1B8C2","#DAC49C","#7D91B6","#B9828C","#B9828C","#B9828C"],"value":[720,529,242,125,19,106,60,46,33,13,117,39,27,10,17,12,78,25,12,13,53,22,14,8,31,287,150,106,56,25,31,24,11,13,7,50,32,9,23,12,11,18,44,34,25,8,17,9,10,137,71,50,21,8,13,66,53,31,10,21,11,10,22,13,191,96,41,27,9,18,14,55,22,10,12,33,95],"shape":["dot","dot","dot","dot","square","dot","square","dot","square","square","dot","dot","dot","square","square","square","dot","dot","square","square","dot","dot","square","square","square","dot","dot","dot","dot","square","dot","dot","square","square","square","dot","dot","square","dot","square","square","square","dot","dot","dot","square","square","square","square","dot","dot","square","dot","square","square","dot","dot","dot","square","dot","square","square","square","square","dot","dot","dot","dot","square","square","square","dot","dot","square","square","square","square"],"title":["<div style=\"text-align:center;\">N : <b>100%<\/b> (720)<br>Complexity : <b>0.254<\/b><br>L : <b>33.3%<\/b> (240)<br>M : <b>33.3%<\/b> (240)<br>S : <b>33.3%<\/b> (240)<\/div>","<div style=\"text-align:center;\">N : <b>73.5%<\/b> (529)<br>Complexity : <b>0.062<\/b><br>L : <b>41.2%<\/b> (218)<br>M : <b>40.6%<\/b> (215)<br>S : <b>18.1%<\/b> (96)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 5.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>33.6%<\/b> (242)<br>Complexity : <b>0.008<\/b><br>L : <b>52.1%<\/b> (126)<br>M : <b>38.4%<\/b> (93)<br>S : <b>9.5%<\/b> (23)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>17.4%<\/b> (125)<br>Complexity : <b>0.002<\/b><br>L : <b>61.6%<\/b> (77)<br>M : <b>32%<\/b> (40)<br>S : <b>6.4%<\/b> (8)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D7, L4, L6, L7, L8, L9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>2.6%<\/b> (19)<br>Complexity : <b>0<\/b><br>L : <b>84.2%<\/b> (16)<br>M : <b>15.8%<\/b> (3)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D7, L4, L6, L7, L8, L9<br><b> Mask <\/b> B6<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>14.7%<\/b> (106)<br>Complexity : <b>0.002<\/b><br>L : <b>57.5%<\/b> (61)<br>M : <b>34.9%<\/b> (37)<br>S : <b>7.5%<\/b> (8)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D7, L4, L6, L7, L8, L9<br><b> Mask <\/b> A1.5, A3, B3<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>8.3%<\/b> (60)<br>Complexity : <b>0<\/b><br>L : <b>63.3%<\/b> (38)<br>M : <b>30%<\/b> (18)<br>S : <b>6.7%<\/b> (4)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D7, L8, L9<br><b> Mask <\/b> A1.5, A3, B3<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>6.4%<\/b> (46)<br>Complexity : <b>0.002<\/b><br>L : <b>50%<\/b> (23)<br>M : <b>41.3%<\/b> (19)<br>S : <b>8.7%<\/b> (4)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> L4, L6, L7<br><b> Mask <\/b> A1.5, A3, B3<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>4.6%<\/b> (33)<br>Complexity : <b>0<\/b><br>L : <b>54.5%<\/b> (18)<br>M : <b>33.3%<\/b> (11)<br>S : <b>12.1%<\/b> (4)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> L4, L6, L7<br><b> Mask <\/b> A1.5, A3, B3<br><b> Solder <\/b> Thick<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.8%<\/b> (13)<br>Complexity : <b>0<\/b><br>L : <b>38.5%<\/b> (5)<br>M : <b>61.5%<\/b> (8)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> L4, L6, L7<br><b> Mask <\/b> A1.5, A3, B3<br><b> Solder <\/b> Thin<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>16.2%<\/b> (117)<br>Complexity : <b>0.003<\/b><br>L : <b>41.9%<\/b> (49)<br>M : <b>45.3%<\/b> (53)<br>S : <b>12.8%<\/b> (15)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, D6, W4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>5.4%<\/b> (39)<br>Complexity : <b>0.002<\/b><br>L : <b>48.7%<\/b> (19)<br>M : <b>51.3%<\/b> (20)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, D6, W4, W9<br><b> Mask <\/b> B3, B6<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>3.8%<\/b> (27)<br>Complexity : <b>0.002<\/b><br>L : <b>51.9%<\/b> (14)<br>M : <b>48.1%<\/b> (13)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, D6, W4, W9<br><b> Mask <\/b> B3, B6<br><b> Panel <\/b> < 2.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.4%<\/b> (10)<br>Complexity : <b>0<\/b><br>L : <b>60%<\/b> (6)<br>M : <b>40%<\/b> (4)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, D6, W4, W9<br><b> Mask <\/b> B3, B6<br><b>  <\/b> 1.5 <= <b>Panel<\/b> < 2.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>2.4%<\/b> (17)<br>Complexity : <b>0<\/b><br>L : <b>47.1%<\/b> (8)<br>M : <b>52.9%<\/b> (9)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, D6, W4, W9<br><b> Mask <\/b> B3, B6<br><b> Panel <\/b> < 1.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.7%<\/b> (12)<br>Complexity : <b>0<\/b><br>L : <b>41.7%<\/b> (5)<br>M : <b>58.3%<\/b> (7)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, D6, W4, W9<br><b> Mask <\/b> B3, B6<br><b> Panel <\/b> >= 2.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>10.8%<\/b> (78)<br>Complexity : <b>0.003<\/b><br>L : <b>38.5%<\/b> (30)<br>M : <b>42.3%<\/b> (33)<br>S : <b>19.2%<\/b> (15)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, D6, W4, W9<br><b> Mask <\/b> A1.5, A3<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>3.5%<\/b> (25)<br>Complexity : <b>0.003<\/b><br>L : <b>32%<\/b> (8)<br>M : <b>56%<\/b> (14)<br>S : <b>12%<\/b> (3)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, D6, W4, W9<br><b> Mask <\/b> A1.5, A3<br><b> Solder <\/b> Thin<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.7%<\/b> (12)<br>Complexity : <b>0<\/b><br>L : <b>50%<\/b> (6)<br>M : <b>33.3%<\/b> (4)<br>S : <b>16.7%<\/b> (2)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, D6, W4, W9<br><b> Mask <\/b> A3<br><b> Solder <\/b> Thin<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.8%<\/b> (13)<br>Complexity : <b>0<\/b><br>L : <b>15.4%<\/b> (2)<br>M : <b>76.9%<\/b> (10)<br>S : <b>7.7%<\/b> (1)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, D6, W4, W9<br><b> Mask <\/b> A1.5<br><b> Solder <\/b> Thin<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>7.4%<\/b> (53)<br>Complexity : <b>0.001<\/b><br>L : <b>41.5%<\/b> (22)<br>M : <b>35.8%<\/b> (19)<br>S : <b>22.6%<\/b> (12)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, D6, W4, W9<br><b> Mask <\/b> A1.5, A3<br><b> Solder <\/b> Thick<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>3.1%<\/b> (22)<br>Complexity : <b>0.001<\/b><br>L : <b>45.5%<\/b> (10)<br>M : <b>40.9%<\/b> (9)<br>S : <b>13.6%<\/b> (3)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, W4<br><b> Mask <\/b> A1.5, A3<br><b> Solder <\/b> Thick<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.9%<\/b> (14)<br>Complexity : <b>0<\/b><br>L : <b>50%<\/b> (7)<br>M : <b>35.7%<\/b> (5)<br>S : <b>14.3%<\/b> (2)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, W4<br><b> Mask <\/b> A1.5, A3<br><b> Solder <\/b> Thick<br><b> Panel <\/b> < 2.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.1%<\/b> (8)<br>Complexity : <b>0<\/b><br>L : <b>37.5%<\/b> (3)<br>M : <b>50%<\/b> (4)<br>S : <b>12.5%<\/b> (1)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D4, W4<br><b> Mask <\/b> A1.5, A3<br><b> Solder <\/b> Thick<br><b> Panel <\/b> >= 2.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>4.3%<\/b> (31)<br>Complexity : <b>0<\/b><br>L : <b>38.7%<\/b> (12)<br>M : <b>32.3%<\/b> (10)<br>S : <b>29%<\/b> (9)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> < 0.5<br><b> PadType <\/b> D6, W9<br><b> Mask <\/b> A1.5, A3<br><b> Solder <\/b> Thick<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>39.9%<\/b> (287)<br>Complexity : <b>0.023<\/b><br>L : <b>32.1%<\/b> (92)<br>M : <b>42.5%<\/b> (122)<br>S : <b>25.4%<\/b> (73)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>20.8%<\/b> (150)<br>Complexity : <b>0.018<\/b><br>L : <b>44.7%<\/b> (67)<br>M : <b>42%<\/b> (63)<br>S : <b>13.3%<\/b> (20)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thin<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>14.7%<\/b> (106)<br>Complexity : <b>0.006<\/b><br>L : <b>52.8%<\/b> (56)<br>M : <b>42.5%<\/b> (45)<br>S : <b>4.7%<\/b> (5)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>7.8%<\/b> (56)<br>Complexity : <b>0.002<\/b><br>L : <b>58.9%<\/b> (33)<br>M : <b>33.9%<\/b> (19)<br>S : <b>7.1%<\/b> (4)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> D4, D6, L7, W4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>3.5%<\/b> (25)<br>Complexity : <b>0<\/b><br>L : <b>72%<\/b> (18)<br>M : <b>28%<\/b> (7)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 1.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> D4, D6, L7, W4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>4.3%<\/b> (31)<br>Complexity : <b>0.002<\/b><br>L : <b>48.4%<\/b> (15)<br>M : <b>38.7%<\/b> (12)<br>S : <b>12.9%<\/b> (4)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 1.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> D4, D6, L7, W4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>3.3%<\/b> (24)<br>Complexity : <b>0.002<\/b><br>L : <b>54.2%<\/b> (13)<br>M : <b>41.7%<\/b> (10)<br>S : <b>4.2%<\/b> (1)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 1.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> D6, L7, W4<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.5%<\/b> (11)<br>Complexity : <b>0<\/b><br>L : <b>72.7%<\/b> (8)<br>M : <b>27.3%<\/b> (3)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 1.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> D6, L7, W4<br><b> Mask <\/b> A3, B6<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.8%<\/b> (13)<br>Complexity : <b>0<\/b><br>L : <b>38.5%<\/b> (5)<br>M : <b>53.8%<\/b> (7)<br>S : <b>7.7%<\/b> (1)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 1.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> D6, L7, W4<br><b> Mask <\/b> A1.5, B3<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1%<\/b> (7)<br>Complexity : <b>0<\/b><br>L : <b>28.6%<\/b> (2)<br>M : <b>28.6%<\/b> (2)<br>S : <b>42.9%<\/b> (3)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 1.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> D4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>6.9%<\/b> (50)<br>Complexity : <b>0.003<\/b><br>L : <b>46%<\/b> (23)<br>M : <b>52%<\/b> (26)<br>S : <b>2%<\/b> (1)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> D7, L4, L6, L8, L9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>4.4%<\/b> (32)<br>Complexity : <b>0.003<\/b><br>L : <b>50%<\/b> (16)<br>M : <b>46.9%<\/b> (15)<br>S : <b>3.1%<\/b> (1)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> L4, L6, L9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.2%<\/b> (9)<br>Complexity : <b>0<\/b><br>L : <b>66.7%<\/b> (6)<br>M : <b>33.3%<\/b> (3)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> L4, L6, L9<br><b> Panel <\/b> < 1.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>3.2%<\/b> (23)<br>Complexity : <b>0.003<\/b><br>L : <b>43.5%<\/b> (10)<br>M : <b>52.2%<\/b> (12)<br>S : <b>4.3%<\/b> (1)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> L4, L6, L9<br><b> Panel <\/b> >= 1.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.7%<\/b> (12)<br>Complexity : <b>0<\/b><br>L : <b>58.3%<\/b> (7)<br>M : <b>41.7%<\/b> (5)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> L4, L6, L9<br><b> Panel <\/b> >= 2.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.5%<\/b> (11)<br>Complexity : <b>0<\/b><br>L : <b>27.3%<\/b> (3)<br>M : <b>63.6%<\/b> (7)<br>S : <b>9.1%<\/b> (1)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> L4, L6, L9<br><b>  <\/b> 1.5 <= <b>Panel<\/b> < 2.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>2.5%<\/b> (18)<br>Complexity : <b>0<\/b><br>L : <b>38.9%<\/b> (7)<br>M : <b>61.1%<\/b> (11)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 3.5<br><b> Solder <\/b> Thin<br><b> PadType <\/b> D7, L8<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>6.1%<\/b> (44)<br>Complexity : <b>0.018<\/b><br>L : <b>25%<\/b> (11)<br>M : <b>40.9%<\/b> (18)<br>S : <b>34.1%<\/b> (15)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 3.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thin<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>4.7%<\/b> (34)<br>Complexity : <b>0.004<\/b><br>L : <b>32.4%<\/b> (11)<br>M : <b>52.9%<\/b> (18)<br>S : <b>14.7%<\/b> (5)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 3.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thin<br><b> Mask <\/b> A3, B3, B6<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>3.5%<\/b> (25)<br>Complexity : <b>0.004<\/b><br>L : <b>44%<\/b> (11)<br>M : <b>48%<\/b> (12)<br>S : <b>8%<\/b> (2)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 3.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thin<br><b> Mask <\/b> A3, B3, B6<br><b> PadType <\/b> D4, D7, L6, L7, L8, L9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.1%<\/b> (8)<br>Complexity : <b>0<\/b><br>L : <b>75%<\/b> (6)<br>M : <b>25%<\/b> (2)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 3.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thin<br><b> Mask <\/b> B3<br><b> PadType <\/b> D4, D7, L6, L7, L8, L9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>2.4%<\/b> (17)<br>Complexity : <b>0<\/b><br>L : <b>29.4%<\/b> (5)<br>M : <b>58.8%<\/b> (10)<br>S : <b>11.8%<\/b> (2)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 3.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thin<br><b> Mask <\/b> A3, B6<br><b> PadType <\/b> D4, D7, L6, L7, L8, L9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.2%<\/b> (9)<br>Complexity : <b>0<\/b><br>L : <b>0%<\/b> (0)<br>M : <b>66.7%<\/b> (6)<br>S : <b>33.3%<\/b> (3)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 3.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thin<br><b> Mask <\/b> A3, B3, B6<br><b> PadType <\/b> D6, L4, W4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.4%<\/b> (10)<br>Complexity : <b>0<\/b><br>L : <b>0%<\/b> (0)<br>M : <b>0%<\/b> (0)<br>S : <b>100%<\/b> (10)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 3.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thin<br><b> Mask <\/b> A1.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>19%<\/b> (137)<br>Complexity : <b>0.023<\/b><br>L : <b>18.2%<\/b> (25)<br>M : <b>43.1%<\/b> (59)<br>S : <b>38.7%<\/b> (53)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>9.9%<\/b> (71)<br>Complexity : <b>0.006<\/b><br>L : <b>29.6%<\/b> (21)<br>M : <b>52.1%<\/b> (37)<br>S : <b>18.3%<\/b> (13)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> B3, B6<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>6.9%<\/b> (50)<br>Complexity : <b>0<\/b><br>L : <b>30%<\/b> (15)<br>M : <b>62%<\/b> (31)<br>S : <b>8%<\/b> (4)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> B3, B6<br><b> PadType <\/b> D4, D7, L4, L6, L7, L8, L9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>2.9%<\/b> (21)<br>Complexity : <b>0.004<\/b><br>L : <b>28.6%<\/b> (6)<br>M : <b>28.6%<\/b> (6)<br>S : <b>42.9%<\/b> (9)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> B3, B6<br><b> PadType <\/b> D6, W4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.1%<\/b> (8)<br>Complexity : <b>0<\/b><br>L : <b>50%<\/b> (4)<br>M : <b>25%<\/b> (2)<br>S : <b>25%<\/b> (2)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 1.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> B3, B6<br><b> PadType <\/b> D6, W4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.8%<\/b> (13)<br>Complexity : <b>0<\/b><br>L : <b>15.4%<\/b> (2)<br>M : <b>30.8%<\/b> (4)<br>S : <b>53.8%<\/b> (7)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 1.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> B3, B6<br><b> PadType <\/b> D6, W4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>9.2%<\/b> (66)<br>Complexity : <b>0.002<\/b><br>L : <b>6.1%<\/b> (4)<br>M : <b>33.3%<\/b> (22)<br>S : <b>60.6%<\/b> (40)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> A1.5, A3<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>7.4%<\/b> (53)<br>Complexity : <b>0.002<\/b><br>L : <b>7.5%<\/b> (4)<br>M : <b>37.7%<\/b> (20)<br>S : <b>54.7%<\/b> (29)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> A1.5, A3<br><b> PadType <\/b> D7, L4, L7, L8, L9, W4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>4.3%<\/b> (31)<br>Complexity : <b>0.002<\/b><br>L : <b>12.9%<\/b> (4)<br>M : <b>41.9%<\/b> (13)<br>S : <b>45.2%<\/b> (14)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> A3<br><b> PadType <\/b> D7, L4, L7, L8, L9, W4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.4%<\/b> (10)<br>Complexity : <b>0<\/b><br>L : <b>10%<\/b> (1)<br>M : <b>60%<\/b> (6)<br>S : <b>30%<\/b> (3)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> A3<br><b> PadType <\/b> L4, L9, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>2.9%<\/b> (21)<br>Complexity : <b>0.002<\/b><br>L : <b>14.3%<\/b> (3)<br>M : <b>33.3%<\/b> (7)<br>S : <b>52.4%<\/b> (11)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> A3<br><b> PadType <\/b> D7, L7, L8, W4<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.5%<\/b> (11)<br>Complexity : <b>0<\/b><br>L : <b>18.2%<\/b> (2)<br>M : <b>45.5%<\/b> (5)<br>S : <b>36.4%<\/b> (4)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 1.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> A3<br><b> PadType <\/b> D7, L7, L8, W4<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.4%<\/b> (10)<br>Complexity : <b>0<\/b><br>L : <b>10%<\/b> (1)<br>M : <b>20%<\/b> (2)<br>S : <b>70%<\/b> (7)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 1.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> A3<br><b> PadType <\/b> D7, L7, L8, W4<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>3.1%<\/b> (22)<br>Complexity : <b>0<\/b><br>L : <b>0%<\/b> (0)<br>M : <b>31.8%<\/b> (7)<br>S : <b>68.2%<\/b> (15)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> A1.5<br><b> PadType <\/b> D7, L4, L7, L8, L9, W4, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.8%<\/b> (13)<br>Complexity : <b>0<\/b><br>L : <b>0%<\/b> (0)<br>M : <b>15.4%<\/b> (2)<br>S : <b>84.6%<\/b> (11)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 0.5 <= <b>skips<\/b> < 5.5<br><b> Solder <\/b> Thick<br><b> Mask <\/b> A1.5, A3<br><b> PadType <\/b> D4, D6, L6<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>26.5%<\/b> (191)<br>Complexity : <b>0.008<\/b><br>L : <b>11.5%<\/b> (22)<br>M : <b>13.1%<\/b> (25)<br>S : <b>75.4%<\/b> (144)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> >= 5.5<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>13.3%<\/b> (96)<br>Complexity : <b>0.008<\/b><br>L : <b>19.8%<\/b> (19)<br>M : <b>20.8%<\/b> (20)<br>S : <b>59.4%<\/b> (57)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> >= 5.5<br><b> Mask <\/b> B6<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>5.7%<\/b> (41)<br>Complexity : <b>0.008<\/b><br>L : <b>36.6%<\/b> (15)<br>M : <b>36.6%<\/b> (15)<br>S : <b>26.8%<\/b> (11)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 5.5 <= <b>skips<\/b> < 12.5<br><b> Mask <\/b> B6<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>3.8%<\/b> (27)<br>Complexity : <b>0.008<\/b><br>L : <b>48.1%<\/b> (13)<br>M : <b>44.4%<\/b> (12)<br>S : <b>7.4%<\/b> (2)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 5.5 <= <b>skips<\/b> < 12.5<br><b> Mask <\/b> B6<br><b> Solder <\/b> Thin<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.2%<\/b> (9)<br>Complexity : <b>0<\/b><br>L : <b>66.7%<\/b> (6)<br>M : <b>11.1%<\/b> (1)<br>S : <b>22.2%<\/b> (2)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 9.5 <= <b>skips<\/b> < 12.5<br><b> Mask <\/b> B6<br><b> Solder <\/b> Thin<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>2.5%<\/b> (18)<br>Complexity : <b>0<\/b><br>L : <b>38.9%<\/b> (7)<br>M : <b>61.1%<\/b> (11)<br>S : <b>0%<\/b> (0)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 5.5 <= <b>skips<\/b> < 9.5<br><b> Mask <\/b> B6<br><b> Solder <\/b> Thin<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.9%<\/b> (14)<br>Complexity : <b>0<\/b><br>L : <b>14.3%<\/b> (2)<br>M : <b>21.4%<\/b> (3)<br>S : <b>64.3%<\/b> (9)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 5.5 <= <b>skips<\/b> < 12.5<br><b> Mask <\/b> B6<br><b> Solder <\/b> Thick<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>7.6%<\/b> (55)<br>Complexity : <b>0.002<\/b><br>L : <b>7.3%<\/b> (4)<br>M : <b>9.1%<\/b> (5)<br>S : <b>83.6%<\/b> (46)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> >= 12.5<br><b> Mask <\/b> B6<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>3.1%<\/b> (22)<br>Complexity : <b>0.002<\/b><br>L : <b>18.2%<\/b> (4)<br>M : <b>18.2%<\/b> (4)<br>S : <b>63.6%<\/b> (14)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 12.5 <= <b>skips<\/b> < 17.5<br><b> Mask <\/b> B6<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.4%<\/b> (10)<br>Complexity : <b>0<\/b><br>L : <b>40%<\/b> (4)<br>M : <b>40%<\/b> (4)<br>S : <b>20%<\/b> (2)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 12.5 <= <b>skips<\/b> < 17.5<br><b> Mask <\/b> B6<br><b> PadType <\/b> D4, D7, L4, W4<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>1.7%<\/b> (12)<br>Complexity : <b>0<\/b><br>L : <b>0%<\/b> (0)<br>M : <b>0%<\/b> (0)<br>S : <b>100%<\/b> (12)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b>  <\/b> 12.5 <= <b>skips<\/b> < 17.5<br><b> Mask <\/b> B6<br><b> PadType <\/b> D6, L6, L7, L8, L9, W9<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>4.6%<\/b> (33)<br>Complexity : <b>0<\/b><br>L : <b>0%<\/b> (0)<br>M : <b>3%<\/b> (1)<br>S : <b>97%<\/b> (32)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> >= 17.5<br><b> Mask <\/b> B6<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>","<div style=\"text-align:center;\">N : <b>13.2%<\/b> (95)<br>Complexity : <b>0<\/b><br>L : <b>3.2%<\/b> (3)<br>M : <b>5.3%<\/b> (5)<br>S : <b>91.6%<\/b> (87)<hr class = \"rPartvisNetwork\">\n<div class =\"showOnMe2\"><div style=\"text-align:center;\"><U style=\"color:blue;\" class = \"classActivePointer\">Rules<\/U><\/div>\n<div class=\"showMeRpartTTp2\" style=\"display:none;\">\n<b> skips <\/b> >= 5.5<br><b> Mask <\/b> A1.5, A3, B3<\/script><script type=\"text/javascript\">$(document).ready(function(){\n$(\".showOnMe2\").click(function(){\n$(\".showMeRpartTTp2\").toggle();\n$.sparkline_display_visible();\n});\n  });<\/script><\/div><\/div>\n\n<\/div>"],"fixed":[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],"colorClust":["#7D91B6","#7D91B6","#7D91B6","#7D91B6","#7D91B6","#7D91B6","#7D91B6","#7D91B6","#7D91B6","#AC83AE","#AC83AE","#AC83AE","#7D91B6","#7D91B6","#AC83AE","#AC83AE","#AC83AE","#AC83AE","#7D91B6","#AC83AE","#7D91B6","#7D91B6","#7D91B6","#AC83AE","#7D91B6","#AC83AE","#7D91B6","#7D91B6","#7D91B6","#7D91B6","#7D91B6","#7D91B6","#7D91B6","#AC83AE","#B9828C","#AC83AE","#7D91B6","#7D91B6","#AC83AE","#7D91B6","#AC83AE","#AC83AE","#AC83AE","#AC83AE","#AC83AE","#7D91B6","#AC83AE","#AC83AE","#B9828C","#AC83AE","#AC83AE","#AC83AE","#B9828C","#7D91B6","#B9828C","#B9828C","#B9828C","#B9828C","#AC83AE","#B9828C","#AC83AE","#B9828C","#B9828C","#B9828C","#B9828C","#B9828C","#7D91B6","#7D91B6","#7D91B6","#AC83AE","#B9828C","#B9828C","#B9828C","#7D91B6","#B9828C","#B9828C","#B9828C"],"labelClust":["L","L","L","L","L","L","L","L","L","M","M","M","L","L","M","M","M","M","L","M","L","L","L","M","L","M","L","L","L","L","L","L","L","M","S","M","L","L","M","L","M","M","M","M","M","L","M","M","S","M","M","M","S","L","S","S","S","S","M","S","M","S","S","S","S","S","L","L","L","M","S","S","S","L","S","S","S"],"Leaf":[0,0,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,0,1,1,0,0,1,1,1,0,0,0,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,0,0,1,1,1,1,0,0,1,0,1,1,0,0,0,1,0,1,1,1,1,0,0,0,0,1,1,1,0,0,1,1,1,1],"font.size":[16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16],"scaling.min":[10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10],"scaling.max":[30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30]},"edges":{"id":["edge1","edge2","edge3","edge4","edge5","edge6","edge7","edge8","edge9","edge10","edge11","edge12","edge13","edge14","edge15","edge16","edge17","edge18","edge19","edge20","edge21","edge22","edge23","edge24","edge25","edge26","edge27","edge28","edge29","edge30","edge31","edge32","edge33","edge34","edge35","edge36","edge37","edge38","edge39","edge40","edge41","edge42","edge43","edge44","edge45","edge46","edge47","edge48","edge49","edge50","edge51","edge52","edge53","edge54","edge55","edge56","edge57","edge58","edge59","edge60","edge61","edge62","edge63","edge64","edge65","edge66","edge67","edge68","edge69","edge70","edge71","edge72","edge73","edge74","edge75","edge76"],"from":[1,2,4,8,8,17,17,35,35,4,9,18,36,36,18,9,19,38,38,19,39,78,78,39,2,5,10,20,40,40,81,162,162,81,20,41,82,82,165,165,41,10,21,42,84,84,42,21,5,11,22,22,45,45,11,23,46,92,92,185,185,46,23,1,3,6,12,24,24,12,6,13,26,26,13,3],"to":[2,4,8,16,17,34,35,70,71,9,18,36,72,73,37,19,38,76,77,39,78,156,157,79,5,10,20,40,80,81,162,324,325,163,41,82,164,165,330,331,83,21,42,84,168,169,85,43,11,22,44,45,90,91,23,46,92,184,185,370,371,93,47,3,6,12,24,48,49,25,13,26,52,53,27,7],"label":["< 5.5","< 0.5","D7, L4,...","B6","A1.5, A...","D7, L8, L9","L4, L6, L7","Thick","Thin","D4, D6,...","B3, B6","< 2.5",">= 1.5","< 1.5",">= 2.5","A1.5, A3","Thin","A3","A1.5","Thick","D4, W4","< 2.5",">= 2.5","D6, W9",">= 0.5","Thin","< 3.5","D4, D6,...","< 1.5",">= 1.5","D6, L7, W4","A3, B6","A1.5, B3","D4, W9","D7, L4,...","L4, L6, L9","< 1.5",">= 1.5",">= 2.5","< 2.5","D7, L8",">= 3.5","A3, B3, B6","D4, D7,...","B3","A3, B6","D6, L4,...","A1.5","Thick","B3, B6","D4, D7,...","D6, W4, W9","< 1.5",">= 1.5","A1.5, A3","D7, L4,...","A3","L4, L9, W9","D7, L7,...","< 1.5",">= 1.5","A1.5","D4, D6, L6",">= 5.5","B6","< 12.5","Thin",">= 9.5","< 9.5","Thick",">= 12.5","< 17.5","D4, D7,...","D6, L6,...",">= 17.5","A1.5, A..."],"value":[529,242,125,19,106,60,46,33,13,117,39,27,10,17,12,78,25,12,13,53,22,14,8,31,287,150,106,56,25,31,24,11,13,7,50,32,9,23,12,11,18,44,34,25,8,17,9,10,137,71,50,21,8,13,66,53,31,10,21,11,10,22,13,191,96,41,27,9,18,14,55,22,10,12,33,95],"title":["<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\"><5.5<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\"><0.5<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D7<\/div><div style=\"text-align:center;\">L4<\/div><div style=\"text-align:center;\">L6<\/div><div style=\"text-align:center;\">L7<\/div><div style=\"text-align:center;\">L8<\/div><div style=\"text-align:center;\">L9<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">B6<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A1.5<\/div><div style=\"text-align:center;\">A3<\/div><div style=\"text-align:center;\">B3<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D7<\/div><div style=\"text-align:center;\">L8<\/div><div style=\"text-align:center;\">L9<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">L4<\/div><div style=\"text-align:center;\">L6<\/div><div style=\"text-align:center;\">L7<\/div>","<div style=\"text-align:center;\"><b>Solder<\/b><\/div><div style=\"text-align:center;\">Thick<\/div>","<div style=\"text-align:center;\"><b>Solder<\/b><\/div><div style=\"text-align:center;\">Thin<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D4<\/div><div style=\"text-align:center;\">D6<\/div><div style=\"text-align:center;\">W4<\/div><div style=\"text-align:center;\">W9<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">B3<\/div><div style=\"text-align:center;\">B6<\/div>","<div style=\"text-align:center;\"><b>Panel<\/b><\/div><div style=\"text-align:center;\"><2.5<\/div>","<div style=\"text-align:center;\"><b>Panel<\/b><\/div><div style=\"text-align:center;\">>=1.5<\/div>","<div style=\"text-align:center;\"><b>Panel<\/b><\/div><div style=\"text-align:center;\"><1.5<\/div>","<div style=\"text-align:center;\"><b>Panel<\/b><\/div><div style=\"text-align:center;\">>=2.5<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A1.5<\/div><div style=\"text-align:center;\">A3<\/div>","<div style=\"text-align:center;\"><b>Solder<\/b><\/div><div style=\"text-align:center;\">Thin<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A3<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A1.5<\/div>","<div style=\"text-align:center;\"><b>Solder<\/b><\/div><div style=\"text-align:center;\">Thick<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D4<\/div><div style=\"text-align:center;\">W4<\/div>","<div style=\"text-align:center;\"><b>Panel<\/b><\/div><div style=\"text-align:center;\"><2.5<\/div>","<div style=\"text-align:center;\"><b>Panel<\/b><\/div><div style=\"text-align:center;\">>=2.5<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D6<\/div><div style=\"text-align:center;\">W9<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\">>=0.5<\/div>","<div style=\"text-align:center;\"><b>Solder<\/b><\/div><div style=\"text-align:center;\">Thin<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\"><3.5<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D4<\/div><div style=\"text-align:center;\">D6<\/div><div style=\"text-align:center;\">L7<\/div><div style=\"text-align:center;\">W4<\/div><div style=\"text-align:center;\">W9<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\"><1.5<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\">>=1.5<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D6<\/div><div style=\"text-align:center;\">L7<\/div><div style=\"text-align:center;\">W4<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A3<\/div><div style=\"text-align:center;\">B6<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A1.5<\/div><div style=\"text-align:center;\">B3<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D4<\/div><div style=\"text-align:center;\">W9<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D7<\/div><div style=\"text-align:center;\">L4<\/div><div style=\"text-align:center;\">L6<\/div><div style=\"text-align:center;\">L8<\/div><div style=\"text-align:center;\">L9<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">L4<\/div><div style=\"text-align:center;\">L6<\/div><div style=\"text-align:center;\">L9<\/div>","<div style=\"text-align:center;\"><b>Panel<\/b><\/div><div style=\"text-align:center;\"><1.5<\/div>","<div style=\"text-align:center;\"><b>Panel<\/b><\/div><div style=\"text-align:center;\">>=1.5<\/div>","<div style=\"text-align:center;\"><b>Panel<\/b><\/div><div style=\"text-align:center;\">>=2.5<\/div>","<div style=\"text-align:center;\"><b>Panel<\/b><\/div><div style=\"text-align:center;\"><2.5<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D7<\/div><div style=\"text-align:center;\">L8<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\">>=3.5<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A3<\/div><div style=\"text-align:center;\">B3<\/div><div style=\"text-align:center;\">B6<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D4<\/div><div style=\"text-align:center;\">D7<\/div><div style=\"text-align:center;\">L6<\/div><div style=\"text-align:center;\">L7<\/div><div style=\"text-align:center;\">L8<\/div><div style=\"text-align:center;\">L9<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">B3<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A3<\/div><div style=\"text-align:center;\">B6<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D6<\/div><div style=\"text-align:center;\">L4<\/div><div style=\"text-align:center;\">W4<\/div><div style=\"text-align:center;\">W9<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A1.5<\/div>","<div style=\"text-align:center;\"><b>Solder<\/b><\/div><div style=\"text-align:center;\">Thick<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">B3<\/div><div style=\"text-align:center;\">B6<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D4<\/div><div style=\"text-align:center;\">D7<\/div><div style=\"text-align:center;\">L4<\/div><div style=\"text-align:center;\">L6<\/div><div style=\"text-align:center;\">L7<\/div><div style=\"text-align:center;\">L8<\/div><div style=\"text-align:center;\">L9<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D6<\/div><div style=\"text-align:center;\">W4<\/div><div style=\"text-align:center;\">W9<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\"><1.5<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\">>=1.5<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A1.5<\/div><div style=\"text-align:center;\">A3<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D7<\/div><div style=\"text-align:center;\">L4<\/div><div style=\"text-align:center;\">L7<\/div><div style=\"text-align:center;\">L8<\/div><div style=\"text-align:center;\">L9<\/div><div style=\"text-align:center;\">W4<\/div><div style=\"text-align:center;\">W9<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A3<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">L4<\/div><div style=\"text-align:center;\">L9<\/div><div style=\"text-align:center;\">W9<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D7<\/div><div style=\"text-align:center;\">L7<\/div><div style=\"text-align:center;\">L8<\/div><div style=\"text-align:center;\">W4<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\"><1.5<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\">>=1.5<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A1.5<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D4<\/div><div style=\"text-align:center;\">D6<\/div><div style=\"text-align:center;\">L6<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\">>=5.5<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">B6<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\"><12.5<\/div>","<div style=\"text-align:center;\"><b>Solder<\/b><\/div><div style=\"text-align:center;\">Thin<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\">>=9.5<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\"><9.5<\/div>","<div style=\"text-align:center;\"><b>Solder<\/b><\/div><div style=\"text-align:center;\">Thick<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\">>=12.5<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\"><17.5<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D4<\/div><div style=\"text-align:center;\">D7<\/div><div style=\"text-align:center;\">L4<\/div><div style=\"text-align:center;\">W4<\/div>","<div style=\"text-align:center;\"><b>PadType<\/b><\/div><div style=\"text-align:center;\">D6<\/div><div style=\"text-align:center;\">L6<\/div><div style=\"text-align:center;\">L7<\/div><div style=\"text-align:center;\">L8<\/div><div style=\"text-align:center;\">L9<\/div><div style=\"text-align:center;\">W9<\/div>","<div style=\"text-align:center;\"><b>skips<\/b><\/div><div style=\"text-align:center;\">>=17.5<\/div>","<div style=\"text-align:center;\"><b>Mask<\/b><\/div><div style=\"text-align:center;\">A1.5<\/div><div style=\"text-align:center;\">A3<\/div><div style=\"text-align:center;\">B3<\/div>"],"color":["#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7","#8181F7"],"font.size":[14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14],"font.align":["horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal","horizontal"],"smooth.enabled":[true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true,true],"smooth.type":["cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier","cubicBezier"],"smooth.roundness":[0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false},"layout":{"hierarchical":{"enabled":true,"direction":"UD"}},"interaction":{"dragNodes":false,"selectConnectedEdges":false,"tooltipDelay":500},"edges":{"scaling":{"label":{"enabled":false}}}},"groups":null,"width":"100%","height":"800px","idselection":{"enabled":false,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)"},"main":{"text":"","style":"font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:20px;text-align:center;"},"submain":{"text":"","style":"font-family:Georgia, Times New Roman, Times, serif;font-size:12px;text-align:center;"},"footer":{"text":"","style":"font-family:Georgia, Times New Roman, Times, serif;font-size:12px;text-align:center;"},"background":"rgba(0, 0, 0, 0)","highlight":{"enabled":true,"hoverNearest":false,"degree":{"from":50000,"to":0},"algorithm":"hierarchical","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":true,"fit":true,"resetHighlight":true,"clusterOptions":{"fixed":true,"physics":false}},"tooltipStay":300,"tooltipStyle":"position: fixed;visibility:hidden;padding: 5px;\n                      white-space: nowrap;\n                      font-family: cursive;font-size:12px;font-color:purple;background-color: #E6E6E6;\n                      border-radius: 15px;","OnceEvents":{"stabilized":"function() { \n        this.setOptions({layout:{hierarchical:false}, physics:{solver:'barnesHut', enabled:true, stabilization : false}, nodes : {physics : false, fixed : true}});\n    }"},"legend":{"width":0.1,"useGroups":false,"position":"left","ncol":1,"stepX":100,"stepY":100,"zoom":true,"nodes":{"label":["Mask","PadType","Panel","skips","Solder","L","M","S"],"color":["#A9D1A5","#DAC49C","#B4C7ED","#F1B8C2","#8AD3D0","#7D91B6","#AC83AE","#B9828C"],"shape":["dot","dot","dot","dot","dot","square","square","square"],"size":[22,22,22,22,22,22,22,22],"Leaf":[0,0,0,0,0,1,1,1],"font.size":[16,16,16,16,16,16,16,16],"id":[10000,10001,10002,10003,10004,10005,10006,10007]},"nodesToDataframe":true},"tree":{"updateShape":true,"shapeVar":"dot","shapeY":"square","colorVar":{"variable":["skips","PadType","Mask","Solder","Panel"],"color":["#F1B8C2","#DAC49C","#A9D1A5","#8AD3D0","#B4C7ED"]},"colorY":{"colorY":{"modality":["L","M","S"],"color":["#7D91B6","#AC83AE","#B9828C"]},"vardecidedClust":["L","L","L","L","L","L","L","L","L","M","M","M","L","L","M","M","M","M","L","M","L","L","L","M","L","M","L","L","L","L","L","L","L","M","S","M","L","L","M","L","M","M","M","M","M","L","M","M","S","M","M","M","S","L","S","S","S","S","M","S","M","S","S","S","S","S","L","L","L","M","S","S","S","L","S","S","S"]}},"export":{"type":"png","css":"float:right;-webkit-border-radius: 10;\n                  -moz-border-radius: 10;\n                  border-radius: 10px;\n                  font-family: Arial;\n                  color: #ffffff;\n                  font-size: 12px;\n                  background: #090a0a;\n                  padding: 4px 8px 4px 4px;\n                  text-decoration: none;","background":"#fff","name":"network.png","label":"Export as png"}},"evals":["OnceEvents.stabilized"],"jsHooks":[]}</script><!--/html_preserve-->

***

### tidyQuant

Taken from: [tidyQuant Documentation](https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ00-introduction-to-tidyquant.html)

1) Get a Stock Index, **tq_index()**, or a Stock Exchange, tq_exchange(): Returns the stock symbols and various attributes for every stock in an index or exchange. Eighteen indexes and three exchanges are available.

2) Get Quantitative Data, **tq_get()**: A one-stop shop to get data from various web-sources.

3) Transmute, **tq_transmute()**, and Mutate, **tq_mutate()**, Quantitative Data: Perform and scale financial calculations completely within the tidyverse. These workhorse functions integrate the xts, zoo, quantmod, TTR, and PerformanceAnalytics packages.

4) Performance analysis, **tq_performance()**, and portfolio aggregation, tq_portfolio(): The PerformanceAnalytics integration enables analyzing performance of assets and portfolios. Refer to Performance Analysis with tidyquant.

<br>
**Get Ticker Data***

```r
tq_get(x = "AAPL", 
       get = "stock.prices", 
       from = " 1990-01-01")
```

<br>
**Get market data**

```r
tq_exchange("NASDAQ")
```

<br>
**Visualise candlesticks**

```r
tq_get(x = "AAPL", 
       get = "stock.prices", 
       from = " 2000-01-01") %>%
  plot_ly(type = "candlestick",
          x = ~date,
          open = ~open,
          close = ~close,
          high = ~high,
          low = ~low,
          height = 600) %>%
  layout(title = "",
         showlegend = FALSE,
         # annotations = list(x = 0.5,
         #                    y = 1.2,
         #                    text = x,
         #                    showarrow = FALSE,
         #                    xref = 'paper',
         #                    yref = 'paper'),
         margin = list(t = 20,
                       b = 20))
```
<br>

**Prediction**
Using the [Prophet package by Facebook](https://facebook.github.io/prophet/)

In R, we use the normal model fitting API. We provide a prophet function that performs fitting and returns a model object. You can then call predict and plot on this model object.


```r
stock <- tq_get(x = "AAPL", 
                get = "stock.prices", 
                from = " 2000-01-01") %>%
  rename(ds = date,
         y = adjusted) %>%
  mutate(yhat_lower = NA,
         yhat_upper = NA) %>%
  select(c('ds', 'y', 'yhat_lower', 'yhat_upper')) %>%
  mutate(ds = as.Date(ds))

fcast <- prophet(stock,
                 daily.seasonality = TRUE,
                 yearly.seasonality = TRUE,
                 weekly.seasonality = TRUE)

future <- make_future_dataframe(fcast, periods = 90)

predict(fcast, future) %>% 
  select(c('ds', 'yhat', 'yhat_lower', 'yhat_upper')) %>%
  filter(ds > Sys.Date()) %>%
  rename(y = yhat)
```

***
<br>

### Advanced Shiny

**Reactivity**

Use **observeEvent** whenever you want to perform an action in response to an event. (Note that "recalculate a value" does not generally count as performing an action--see eventReactive for that.) The first argument is the event you want to respond to, and the second argument is a function that should be called whenever the event occurs.

Use **eventReactive** to create a calculated value that only updates in response to an event. This is just like a normal reactive expression except it ignores all the usual invalidations that come from its reactive dependencies; it only invalidates in response to the given event.

Both **observeEvent** and **eventReactive** take an ignoreNULL parameter that affects behavior when the eventExpr evaluates to NULL (or in the special case of an actionButton, 0). In these cases, if ignoreNULL is TRUE, then an observeEvent will not execute and an eventReactive will raise a silent validation error. This is useful behavior if you don't want to do the action or calculation when your app first starts, but wait for the user to initiate the action first (like a "Submit" button); whereas ignoreNULL=FALSE is desirable if you want to initially perform the action/calculation and just let the user re-initiate it (like a "Recalculate" button).

**observe** and **reactive** are the same thing except they are constantly listening and do not wait for an event to occur.

<br>

**Validation**

**validate** tests a condition and returns a validation error if the test fails. Validation errors are designed to interact with the Shiny framework in a pleasing way. Shiny will:

- recognize a validation error
- display a validation error in a neutral grey color
- pass a validation error to any reactive expression or observer object that depends on it
- validate takes one or more specially formatted arguments. You can provide these arguments need, a new function designed to work with validate. You can also provide these arguments with your own functions if you like.

You call **req** with one or more arguments. req will evaluate each argument one at a time, and if it encounters an argument that it considers to be “missing” or “false” (see below for exactly what this means), it will stop.


```r
# ------------ validate example

validate(
  
  need( try( input$data != foo ), 
        "Please select a data set")
  
)

# ------------ req example

library(shiny)

ui <- fluidPage(
  selectInput("datasetName", "Dataset", c("", "pressure", "cars")),
  plotOutput("plot"),
  tableOutput("table")
)

server <- function(input, output, session) {
  dataset <- reactive({
    # Make sure requirements are met
    req(input$datasetName)

    get(input$datasetName, "package:datasets", inherits = FALSE)
  })

  output$plot <- renderPlot({
    plot(dataset())
  })

  output$table <- renderTable({
    head(dataset(), 10)
  })
}

shinyApp(ui, server)
```
<br>

**shinyJS**
Full documentation here: [shinyjs documentation](https://deanattali.com/shinyjs/)

The **shinyjs** package lets you perform common useful JavaScript operations in Shiny apps that will greatly improve your apps.


```r
library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # Include shinyjs

  actionButton("button", "Click me"),
  textInput("text", "Text")
)

server <- function(input, output) {
  observeEvent(input$button, {
    toggle("text")  # toggle is a shinyjs function
  })
}

shinyApp(ui, server)
```
<br>

**shinydashboard**
[Documentation here](https://rstudio.github.io/shinydashboard/get_started.html)

The **shinydashboard** package has three parts: a header, a sidebar, and a body. Here’s the most minimal possible UI for a dashboard page.


```r
## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),

      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
```

***

### Deploying Shiny Apps

[Full RStudio deployment options](https://shiny.rstudio.com/deploy/)

- shinyapps.io (cloud hosting, free if open publicly)
- Shiny Server (on-prem, open-source)
- Shiny Server Pro (on-prem, pay for usage with logins/authentication/load-balancing)
- RSConnect (on-prem, par for users with easy deployment button and added services)

**shinyapps.io**

```r
library(rsconnect)

rsconnect::setAccountInfo(name='devday',
                          token='2FA179ED87E8091FC9A3E39C6B22DC4B',
                          secret='wFXRdhepkahFIYakpc0JvAMlFvplw4DiKIzqBwFx')

rsconnect::deployApp(appDir = '/Users/elliotpannaman/Desktop/workshop_split/',
                     appName = 'aquaman',
                     account = 'devday')
```

**Shiny Server (free)**

- [Building Shiny Server on a Raspberry Pi](https://steemit.com/tutorial/@m4rk.h4nn4/how-to-install-and-run-shiny-server-on-the-raspberry-pi-3-and-raspian-jassie-lite)
- [Install Shiny Server on Raspberry Pi](https://withr.github.io/install-shiny-server-on-raspberry-pi/)

Notes:
- Can't handle split apps (ui.R and server.R) - needs to call the app using the shinyApp(ui, server) function within the app.R file.
- Can handle pretty complex file structures, so use FTP to get the files on there if needed.
- If it's your home server and you want to connect elsewhere, remember to open the box up to the public domain (although be careful as this is probably a catastrophe waiting to happen).

***

### More Advanced Stuff

**User Experience**
  
- [Linking Plotly charts as an input for other elements](https://plot.ly/r/shiny-coupled-events/)  
- [Choosing icons](https://fontawesome.com/icons?from=io)  
- [Running javascript code with shinyJS](https://github.com/daattali/shinyjs)  
- [Improving the user experience with progress indicators](https://shiny.rstudio.com/articles/progress.html)  
- [Using downloadHandler to download tables](https://shiny.rstudio.com/articles/download.html)  
- [Downloading ggplots with ggsave](https://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app)  
- [Showing elements based on conditions](https://shiny.rstudio.com/gallery/conditionalpanel-demo.html)  

**Speed**

- Run all the aggregations, data processing and queries in advance (like a cube), then run `Shiny` from condensed datasets at an individual level.
- Add the data used into a database, then query as an when needed instead of running in memory.
- Use the `req()` function inside `render({})` functions on your server.R script to stop the app trying to draw plots/tables until the required reactive data/filters have already been sorted out.
- Use the `isolate()` function to stop input changes affecting every output they're linked to every time.

**Other considerations**
  
- [Separating code for complex dashboards with Shiny Modules](https://shiny.rstudio.com/articles/modules.html)  
- [Saving the state of a Shiny application with Bookmarking](https://shiny.rstudio.com/articles/bookmarking-state.html)  
- [Tracking client usage with session$clientData](https://shiny.rstudio.com/articles/client-data.html)  
- [Linking to Google Analytics for broader tracking](https://shiny.rstudio.com/articles/google-analytics.html)  
- [Debugging Shiny applications](https://shiny.rstudio.com/articles/debugging.html)  
- [Server-to-client custom messages](https://shiny.rstudio.com/gallery/server-to-client-custom-messages.html)  
- [Generating downloadable reports](https://shiny.rstudio.com/articles/generating-reports.html)

***

### Resources

- [**Shiny Official Site**](https://fontawesome.com/icons?from=io)  
- [**Shiny Cheatsheet**](https://shiny.rstudio.com/articles/cheatsheet.html)  
  
**Tutorials**  
  
- [Shiny Official Tutorial](https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/)  
- [Datacamp Tutorial](https://www.datacamp.com/courses/building-web-applications-in-r-with-shiny)  
- [HMRC Self-Learning Guide to R](https://kai-data-exploitation.github.io/R-selflearn-guide/guide.html)
- [Mango Solutions Tutorial](https://github.com/MangoTheCat/shiny_beyond_the_basics/blob/master/Workshop%202%20-%20Shiny%20-%20Beyond%20the%20Basics.pdf)  

**Visualisation packages**

- [**DT** (best for interactive tables!)](https://rstudio.github.io/DT/shiny.html)  
- [**Plotly** (use ggplotly() function to convert ggplot to interactive charts)](https://plot.ly/r/shiny-tutorial/)  
- [**Leaflet** (maps)](https://rstudio.github.io/leaflet/shiny.html)  
- [**d3**](http://www.htmlwidgets.org/showcase_networkD3.html)  
- [**Google Charts**](https://github.com/mages/googleVis/)
  
**Inspiration**  
  
- [RStudio App Gallery](https://shiny.rstudio.com/gallery/)  
- [Shiny Extended Gallery](https://shiny.rstudio.com/gallery/see-more.html)  
- [Show Me Shiny Gallery](https://www.showmeshiny.com/)  
  
**Influencers**  
  
- [Dean Attali](https://deanattali.com/blog/advanced-shiny-tips/)  
- [Joe Cheng](https://twitter.com/jcheng?lang=en)  
  
**HMRC Examples**  
  
- [shinyTemplate](https://dataexploitation.shinyapps.io/shinytemplate/) 
