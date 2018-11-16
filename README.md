
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
                          token='xxx',
                          secret='xxx')

rsconnect::deployApp(appDir = 'xxx',
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
