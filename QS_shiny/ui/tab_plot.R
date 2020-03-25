# QS shiny - tab - plot
tabPanel("Plot file", value = "plot.file",
         textOutput("title"),
         textOutput("date"),
         textOutput("instrument"),
         plotlyOutput("plot_file"),
         textOutput("plot_info"),
         DT::dataTableOutput("plot_table")
)
