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
