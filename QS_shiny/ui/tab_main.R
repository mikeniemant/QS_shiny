# QS shiny - tab - main
tabPanel("Imported files", value = "main",
         DT::dataTableOutput("input_files_table")
)
