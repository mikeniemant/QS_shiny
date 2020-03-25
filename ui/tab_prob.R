# QS shiny - tab - table
tabPanel("Plot probability x sample", value = "plot.prob",
         plotlyOutput("prob_sample_plot"),
         DT::dataTableOutput("prob_table")
)
