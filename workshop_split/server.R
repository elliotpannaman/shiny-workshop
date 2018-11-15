
shinyServer(function(input, output, session) {
  
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
                 layerId = "feature.properties.cartodb_id"#,
                 # fill = ~pal(objectid)
      )
    # addPolygons(data = county,
    #             stroke = FALSE,
    #             smoothFactor = 0.3,
    #             fillColor = ~pal(objectid),
    #             # label = ~paste(LAD16NM, paste0(as.character(input$indicator), ": "), input$indicator),
    #             layerId = ~objectid,
    #             group = "towns")

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

})