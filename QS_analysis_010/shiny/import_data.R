# QS results dashboard - import_data

pageWithSidebar(
  headerPanel("Import multiple files"),
  
  sidebarPanel(
    shinyDirButton("dir", "Choose directory", "Upload"),
    checkboxGroupInput(inputId = "control",
                       label = "Include controls:", 
                       choiceNames = c("Postive", "Negative"),
                       choiceValues = c("Positive Control", "Negative Control"),
                       selected = NULL),
    # h4("dir"),
    # verbatimTextOutput("dir"),
    # h4("Files in that dir"),
    # verbatimTextOutput("files"),
    checkboxGroupInput("selected_files", label = "CT files found in directory", choices = ""),
    # h4("Checked files"),
    # verbatimTextOutput("checked_files"),
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
