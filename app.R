# QS shiny - app
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(dplyr)
library(tidyr)
library(plotly)

# import global variables and functions
source("./global.R")

# UI ----
ui <- fluidPage(
  titlePanel(title = "QS shiny | V 0.7.8"),
  
  fluidRow(
    column(3,
           source(file.path("ui", "sidebar.R"), local = TRUE)$value),
    
    column(8,
           tabsetPanel(id = "tabs",
                       source(file.path("ui", "tab_main.R"),  local = TRUE)$value,
                       source(file.path("ui", "tab_plot.R"),  local = TRUE)$value,
                       source(file.path("ui", "tab_trend.R"),  local = TRUE)$value,
                       source(file.path("ui", "tab_table.R"),  local = TRUE)$value
           )
    )
  )
)

# All functionality in the back-end
# Server ----
server <- shinyServer(function(input, output, session) {
  # Import files ----
  output$file_uploaded <- reactive({
    output$import_message <- renderText("Please import QS output files")
    # Get imported files
    # 1. Import files
    files.df <- input$input_files # data frame with columns: name, size, type, datapath
    
    # Print files.df if available
    if(!is.null(files.df)) {
      print("files.df")
      print(files.df)
    }
    
    # 2. Check if files.df not empty
    if(is.null(files.df)) return(F)
    
    # 3. Check if a filename is included twice in the vector
    n.files <- sum(unlist(lapply(files.df$name, function(x) {
      length(which(files.df$name %in% x))}
    )))
    
    if(n.files != nrow(files.df)) {
      output$import_message <- renderText("Double file names")
      output$input_files_table <- DT::renderDataTable(NULL)
      return(F)
    }
    
    # Validate files
    # 4. Validate and preprocess selected files (imported files df)
    if.df <<- do.call(rbind, mapply(FUN = function(i){
      i.df <- data.frame(`Exp name` = substr(files.df$name[i], 0, nchar(files.df$name[i])-4),
                         `File name` = files.df$name[i],
                         datapath = files.df$datapath[i],
                         Date = NA,
                         Instrument_ID = NA, 
                         `File empty?` = F,
                         N_missing_values = NA,
                         stringsAsFactors = F, 
                         check.names = F)
      
      pp <- readTxt(i.df$datapath)
      pp$data <- processQSResultsTxt(pp$data)
      
      # Check if file is not empty
      if(nrow(pp$data) == 0) {
        i.df$`File empty?` = "Yes"
        return(i.df)
      }
      
      i.df$Date <- pp$date
      i.df$Instrument_ID <- pp$instr
      i.df$N_missing_values <- sum(is.na(pp$data))
      
      return(i.df)
      
    }, i = 1:nrow(files.df), SIMPLIFY = F))
    rownames(if.df) <- NULL
    print("if.df")
    print(if.df)
    
    # Check for empty files
    empty.files <<- which(if.df$`File empty?` == "Yes")
    if(length(empty.files) > 0) {
      output$import_message <- renderText(paste0(ifelse(length(empty.files) == 1, "File ", "Files "), empty.files, " no data"))
      output$input_files_table <- DT::renderDataTable(if.df)
      return(F)
    }
    
    # Check if all other values are valid
    incomplete.rows <- which(unname(apply(if.df %>% select(-`File empty?`), 2, function(x) sum(x == F)>0)))
    
    if(length(incomplete.rows) > 0) {
      output$import_message <- renderText(paste0(ifelse(length(incomplete.rows) == 1, "File ", "Files "), incomplete.rows, " incomplete"))
      
      output$input_files_table <- DT::renderDataTable(if.df)

      return(F)
    } else {
      output$import_message <- renderText(paste0("Successfully imported and pre-processed ", nrow(if.df), ifelse(nrow(if.df) == 0, " file", " files")))
      output$input_files_table <- DT::renderDataTable(if.df %>% select(-datapath))
      
      # Preprocess data
      data.df <<- preProcessFiles(if.df)
      print(paste("data.df; nrow", as.character(nrow(data.df)), "; ncol", as.character(ncol(data.df))))
      print(head(data.df))
      
      return(T)
    }
  })
  
  outputOptions(output, 'file_uploaded', suspendWhenHidden=FALSE)
  
  # Check boxes imported files ----
  # Check boxes that visualize the imported files
  # - For checked check boxes, get and preprocess data
  observeEvent({
    
    input$input_files
  },
  {
    if(nrow(if.df) > 0) {
      names <- if.df$`Exp name`

      updateCheckboxGroupInput(session, "selected_files",
                               choices = names,
                               selected = names)
    } else {
      updateCheckboxGroupInput(session, "selected_files",
                               choices = "")
    }
  })
  
  # radio button imported files ----
  observeEvent({
    
    input$input_files
  },
  {
    if(nrow(if.df) > 0) {
      names <- if.df$`Exp name`
      
      updateRadioButtons(session, "radio_selected_file",
                         choices = names,
                         selected = names[1])
    } else {
      updateCheckboxGroupInput(session, "radio_selected_file",
                               choices = "")
    }
  })
  
  # radio button extract file ----
  # Extract files that are selected by checkbox
  extractSelectedFile <- reactive({
    return(input$radio_selected_file)
  })
  
  # Button to (de-)select all imported files ----
  # Select all
  observeEvent(input$select, {
    updateCheckboxGroupInput(session, "selected_files",
                             selected = if.df$`Exp name`)
  })
  
  # De-select all
  observeEvent(input$deselect, {
    updateCheckboxGroupInput(session, "selected_files",
                             selected = "")
  })
  
  # Extract selected files
  selectedFiles <- reactive({
    return(input$selected_files)
  })
  
  # Plot file ----
  source(file.path("server", "server_plot.R"),  local = TRUE)$value  
  
  # Plot trend ----
  source(file.path("server", "server_trend.R"),  local = TRUE)$value  
  
  # Table ----
  source(file.path("server", "server_table.R"),  local = TRUE)$value  
})

# Run application 
shinyApp(ui = ui, server = server)
