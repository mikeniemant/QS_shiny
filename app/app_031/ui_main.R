# QS results dashboard - 031 - UI

wellPanel(
  fileInput(inputId = 'QS_files', label = 'Choose xlsx files', multiple = T,
            accept = c(".xlsx"), placeholder = "Choose files"),
  
  # (de-)select all imported files action button
  actionButton("select", "Select all files"),
  actionButton("deselect", "De-select all files"),
  
  checkboxGroupInput(inputId = "selected_files", 
                     label = "QS files", 
                     choices = ""),
  
  checkboxGroupInput(inputId = "control",
                     label = "Include controls:", 
                     choiceNames = c("Positive", "Negative"),
                     choiceValues = c("Positive Control", "Negative Control"),
                     selected = NULL),
  
  # h4("Selected files"),
  # verbatimTextOutput("selected_files"),
  radioButtons(inputId = "data_type", 
               label = "Data type", 
               choices = c("All", "cT", "Cq Conf", "Positive Control", "Negative Control", "MTP", "Tm1"), 
               selected = "cT"),
  
  downloadLink(outputId = 'downloadData', label = 'Download xlsx file with all sheets'),
)