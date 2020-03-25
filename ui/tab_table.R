# QS shiny - tab - table
tabPanel("Table", value = "table",
         # conditionalPanel(condition = "(output.file_uploaded & input.tabs == 'tab_table')",
         #                  textOutput("import_message")),
         conditionalPanel(condition = "input.tabs == 'table' & output.file_uploaded",
                          downloadButton(outputId = 'downloadData', 
                                         label = 'Download xlsx file with all sheets')),
         DT::dataTableOutput("results")
)
