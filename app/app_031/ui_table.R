# QS results dashboard - 031 - table

tabPanel("Table", value = "table",
         DT::dataTableOutput("QS_all_results")
)