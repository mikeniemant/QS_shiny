# QS results dashboard - import_file

pageWithSidebar(
  headerPanel("Import file"),
  
  sidebarPanel(
    fileInput(inputId = 'QS_file', label = 'Choose xlsx file',
              accept = c(".xlsx"), placeholder = "Choose file"),
    
    fluidRow(column(5, radioButtons('type', 'Type',
                                    c(cT = 'cT',
                                      `Cq Conf` = "Cq Conf",
                                      `Positive Control` = "Positive Control",
                                      `Negative Control` = "Negative Control",
                                      MTP = "MTP",
                                      Tm1 = "Tm1"),
                                    'cT')))
    ,width = 3
  ),
  
  mainPanel(
    plotOutput("plot_file"),
    
    textOutput("plot_text"),
    
    tableOutput('missing_df'),
    DT::dataTableOutput("QS_results")
  )
)
