# QS results dashboard - import_file

pageWithSidebar(
  headerPanel("Import file"),
  
  sidebarPanel(
    fileInput('QS_file', 'Choose xlsx file',
              accept = c(".xlsx"), 
              placeholder = "Choose file", 
              multiple = T),
    
    fluidRow(column(5, radioButtons('sheet', 'Tab',
                                    c(Results='Results',
                                      `Melt Curve Result`='Melt Curve Result'),
                                    'Results')),
             column(5, radioButtons('type', 'Type',
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
    #tableOutput('QS_results')
    plotOutput("plot_file"),
    
    textOutput("plot_text"),
    
    tableOutput('missing_df'),
    DT::dataTableOutput("QS_results")
  )
)
