# ui.R
dashboardPage(
  dashboardHeader(title = "cran.rstudio.com"),
  dashboardSidebar(
    textInput("firebase_node", "Firebase Node", "test3"),
    textInput("firebase_key", "Firebase Key", "float"),
    textInput("node", "node", "TestEC3"),
    textInput("firebase_test", "Firebase Test", "allDataSensor01"),
    sliderInput("rateThreshold", "Warn when rate exceeds",
                min = 0, max = 50, value = 3, step = 0.1
    ),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(column(12,
                valueBoxOutput(width=3, "airtemp"),              
                valueBoxOutput(width=3, "humidity"),
                valueBoxOutput(width=3, "pressure"),
                valueBoxOutput(width=3, "gas")
                )
              ),
              fluidRow(column(12,
                valueBoxOutput(width=3, "watertemp"),              
                valueBoxOutput(width=3, "waterph"),
                valueBoxOutput(width=3, "turbidity"),
                valueBoxOutput(width=3, "voltage")
                )
              )
              # fluidRow(
              #   column(3,plotlyOutput("tempPlot")),
              #   column(3,plotlyOutput("humidityPlot")),
              #   column(3,plotlyOutput("pressurePlot")),
              #   column(3,plotlyOutput("gasPlot"))
              #   ),
              # fluidRow(
              #   column(3,plotlyOutput("watertempPlot")),
              #   column(3,plotlyOutput("waterphPlot")),
              #   column(3,plotlyOutput("turbidityPlot")),
              #   column(3,plotlyOutput("voltagePlot"))
              #   )
      ),
      tabItem("rawdata",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)

# test