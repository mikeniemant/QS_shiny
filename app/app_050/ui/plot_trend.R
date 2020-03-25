# QS results dashboard - 041 - plot trend tab

tabPanel("Plot trend", value = "plot.trend",
         textOutput("trend_info"),
         plotlyOutput("trend")
)