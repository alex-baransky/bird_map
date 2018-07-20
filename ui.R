shinyUI(dashboardPage(
  skin = 'blue',
    dashboardHeader(title = "2016 US Bird Range"),
    dashboardSidebar(
            sidebarMenu(
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Time Histogram", tabName = 'hist', icon = icon("time", lib = 'glyphicon')),
            menuItem("Data Table", tabName = "data", icon = icon("database"))
        ),
        selectizeInput("species",
                       "Select species to inspect:",
                       species, selected = 'American Bittern'),
        sliderInput(inputId = "month",
                    label = "Month Range",
                    min = 01,
                    max = 12,
                    value = 1,
                    round = TRUE,
                    sep = '',
                    animate = animationOptions(interval = 2000,
                                               playButton = HTML("<h4>Play</h4>")))
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(tabName = "map",
                    fluidRow(box(plotOutput("map"), width = 12),
                             uiOutput("image"),
                             textOutput("month_num"))),
            tabItem(tabName = 'hist',
                    fluidRow(box(plotOutput("hist"), width = 12))),
            tabItem(tabName = "data",
                    fluidRow(box(DT::dataTableOutput("table"), width = 12, title = 'Observations by State,County')))
        )
    )
))