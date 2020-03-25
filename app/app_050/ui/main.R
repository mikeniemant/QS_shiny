# QS results dashboard - 041 - tab 1

tabPanel("Imported files", value = "main",
         DT::dataTableOutput("input_files_table")
)