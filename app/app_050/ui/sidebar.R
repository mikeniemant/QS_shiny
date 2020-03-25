# QS results dashboard - 041 - UI

wellPanel( 
  fileInput(inputId = 'input_files', label = 'Import Quant Studio files', multiple = T,
            accept = c(".xlsx"), placeholder = "Select xlsx files"),
  
  # Working version  
  # conditionalPanel(condition = "output.file_uploaded & input.tabs == 'main'",
  #                  textOutput("import_message")),
  
  conditionalPanel(condition = "(output.file_uploaded & input.tabs == 'main') | !output.file_uploaded",
                   textOutput("import_message")),

  conditionalPanel(condition = "output.file_uploaded & (input.tabs == 'plot.trend' | input.tabs == 'table')",
                   # Selected files checkbox
                   checkboxGroupInput(inputId = "selected_files",
                                      label = "QS files",
                                      choices = ""),
                   # (de-)select all imported files action button
                   actionButton("select", "Select all files"),
                   actionButton("deselect", "De-select all files")),
  
  # Plot file
  conditionalPanel(condition = "output.file_uploaded & input.tabs == 'plot.file'",
                   radioButtons(inputId = "radio_selected_file", 
                                label = "QS files", 
                                choices = "", 
                                selected = ""),
                   radioButtons(inputId = "data_type", 
                                label = "Data type", 
                                choices = c("cT", "Cq Conf", "MTP", "Tm1"), 
                                selected = "cT"),
                   checkboxGroupInput(inputId = "control",
                                      label = "Include controls:",
                                      choiceNames = c("Positive", "Negative"),
                                      choiceValues = c("positive", "negative"),
                                      selected = NULL)
  )
)
