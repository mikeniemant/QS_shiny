# QS shiny - tab - plot
tabPanel("Plot sample x lot", value = "plot.sample.lot",
         conditionalPanel(condition = "output.av_file_uploaded & input.tabs == 'plot.sample.lot'",
                          radioButtons(inputId = "radio_av_samples", 
                                       label = "QS sample", 
                                       choices = "", 
                                       selected = "")),
         
         #plotlyOutput("prob_sample_plot"),
         
         #plotlyOutput("sample_lot_plot")
         plotOutput("sample_lot_plot")
)
