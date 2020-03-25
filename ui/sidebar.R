# QS shiny - ui - sidebar
wellPanel( 
  # QS results ----
  
  conditionalPanel(condition = "input.tabs != 'plot.sample.lot' & input.tabs != 'plot.prob'",
   fileInput(inputId = 'input_files', label = 'Import Quant Studio output files', multiple = T,
             accept = c(".xlsx"), placeholder = "Select xlsx files")),
  
  # Working version  
  # conditionalPanel(condition = "output.file_uploaded & input.tabs == 'main'",
  #                  textOutput("import_message")),
  
  conditionalPanel(condition = "!output.file_uploaded & (input.tabs != 'plot.sample.lot' & input.tabs != 'plot.prob')",
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
                                choices = c("Ct", "Cq Conf", "MTP", "Tm1"), 
                                selected = "Ct"),
                   checkboxGroupInput(inputId = "control",
                                      label = "Include controls:",
                                      choiceNames = c("Positive", "Negative"),
                                      choiceValues = c("positive", "negative"),
                                      selected = NULL)
  ),
  
  # AV ----
  # Import analytical validation file
  conditionalPanel(condition = "input.tabs == 'plot.sample.lot' | input.tabs == 'plot.prob'",
                   fileInput(inputId = 'av_file', label = 'Import analytical validation file', multiple = F,
                             accept = c(".xlsx"), placeholder = "Select csv files"),
                   textOutput("import_av_message"))
)
