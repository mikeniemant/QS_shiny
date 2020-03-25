# QS shiny - tab - trend
tabPanel("Plot trend", value = "plot.trend",
         textOutput("trend_info"),
         plotlyOutput("trend")
)
