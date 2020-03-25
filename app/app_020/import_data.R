# QS results dashboard - import_data

pageWithSidebar(
  headerPanel("Import multiple files"),
  
  sidebarPanel(
    fileInput(inputId = 'QS_files', label = 'Choose xlsx files', multiple = T,
              accept = c(".xlsx"), placeholder = "Choose files"),
    
    checkboxGroupInput(inputId = "control",
                       label = "Include controls:", 
                       choiceNames = c("Positive", "Negative"),
                       choiceValues = c("Positive Control", "Negative Control"),
                       selected = NULL),
    
    checkboxGroupInput(inputId = "selected_files", 
                       label = "CT files", 
                       choices = ""),
    
    # h4("Selected files"),
    # verbatimTextOutput("selected_files"),
    radioButtons(inputId = "data_type", 
                 label = "Data type", 
                 choices = c("All", "cT", "Cq Conf", "Positive Control", "Negative Control", "MTP", "Tm1"), 
                 selected = "cT"),
    
    downloadLink(outputId = 'downloadData', label = 'Download xlsx file with all sheets'), 
    
    width = 3
  ),
  
  mainPanel(
    dataTableOutput("QS_all_results"),
    plotOutput("plot_trend")
  )
)
