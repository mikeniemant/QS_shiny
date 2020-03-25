# QS results dashboard - 031 - plot

tabPanel("Plot", value = "plot",
         plotOutput("plot"),
         verbatimTextOutput("plot_info")
)