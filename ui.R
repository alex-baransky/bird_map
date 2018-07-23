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
                       "Select bird species:",
                       species_list, selected = 'American Bittern'),
        checkboxInput("breed", "Show breeding season only", value = FALSE),
        sliderInput(inputId = "month",
                    label = "Select month range:",
                    min = 01,
                    max = 12,
                    value = c(1, 12),
                    round = TRUE,
                    sep = '',
                    animate = animationOptions(interval = 2000,
                                               playButton = HTML("<h4>Play</h4>"))),
        uiOutput("menu_text")
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(tabName = "map",
                    fluidRow(box(plotOutput("map"), width = 12),
                             uiOutput("image"))),
            tabItem(tabName = 'hist',
                    fluidRow(box(plotOutput("hist"), width = 12))),
            tabItem(tabName = "data",
                    fluidRow(box(DT::dataTableOutput("county_table"), width = 12, title = 'Observations by County')),
                    fluidRow(box(DT::dataTableOutput("time_table"), width = 12, title = 'Observations by Time')))
        )
    )
))