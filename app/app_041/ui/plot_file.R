# QS results dashboard - 041 - tab plot file

tabPanel("Plot file", value = "plot.file",
         textOutput("title"),
         textOutput("date"),
         textOutput("instrument"),
         plotlyOutput("plot_file"),
         textOutput("plot_info"),
         DT::dataTableOutput("plot_table")
)