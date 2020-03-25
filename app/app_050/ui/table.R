# QS results dashboard - 041 - tab table

tabPanel("Table", value = "table",
         conditionalPanel(condition = "input.tabs == 'table' & output.file_uploaded",
                          downloadButton(outputId = 'downloadData', 
                                         label = 'Download xlsx file with all sheets')),
         DT::dataTableOutput("results")
)

