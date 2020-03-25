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
  # App title
  titlePanel(title = "QS shiny | V 0.6.0"),
  
  fluidRow(
    # Inputs: edit in this file:
    column(3,
           source(file.path("ui", "sidebar.R"), local = TRUE)$value),
    
    # Outputs: edit in these files:
    column(8,
           tabsetPanel(id = "tabs",
                       source(file.path("ui", "tab_main.R"),  local = TRUE)$value,
                       source(file.path("ui", "tab_plot.R"),  local = TRUE)$value,
                       source(file.path("ui", "tab_trend.R"),  local = TRUE)$value,
                       source(file.path("ui", "tab_lot.R"),  local = TRUE)$value,
                       source(file.path("ui", "tab_prob.R"),  local = TRUE)$value,
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
    files.df <<- input$input_files # data frame with columns: name, size, type, datapath
    
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
    if.df <- do.call(rbind, mapply(FUN = function(path){
      i.df <- data.frame(`Results sheet?` = "No",
                         `File empty?` = "No",
                         Name = NA,
                         Date = NA,
                         Instrument_ID = NA, 
                         N_missing_values = NA,
                         stringsAsFactors = F, 
                         check.names = F)
      
      sheets <- readxl::excel_sheets(path)
      if(!"Results" %in% sheets) {
        return(i.df)
      } else {
        i.df$`Results sheet?` = "Yes"
      }
      # Extract raw data
      raw <- suppressMessages(readxl::read_xlsx(path = as.character(path), 
                                                sheet = "Results"))
      
      # Check if file is not empty
      if(nrow(raw) == 0) {
        i.df$`File empty?` = "Yes"
        return(i.df)
      }
      
      pp <- processQSResults(raw)
      
      i.df$Name <- pp$name
      i.df$Date <- pp$date
      i.df$Instrument_ID <- pp$instr
      i.df$N_missing_values <- sum(is.na(pp$data))
      return(i.df)
      
    }, files.df$datapath, SIMPLIFY = F))
    rownames(if.df) <- NULL

    # Check if all files have a results tab
    missing.results.rows <- which(if.df$`Results sheet?` == "No")
    
    if(length(missing.results.rows) > 0) {
      output$import_message <- renderText(paste0(ifelse(length(missing.results.rows), "File ", "Files "), missing.results.rows, " missing `Results` sheet"))
      output$input_files_table <- DT::renderDataTable(if.df)
      return(F)
    }
    
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
      output$input_files_table <- DT::renderDataTable(if.df %>% select(-`Results sheet?`))
      
      # Preprocess data
      data.df <<- preProcessFiles(files.df)
      
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
    if(nrow(files.df) > 0) {
      names <- unlist(lapply(files.df$name, function(x) substr(x, 1, nchar(x)-5)))
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
    if(nrow(files.df) > 0) {
      names <- unlist(lapply(files.df$name, function(x) substr(x, 1, nchar(x)-5)))
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
                             #choices = files$name,
                             selected = unlist(lapply(files.df$name, function(x) substr(x, 1, nchar(x)-5))))
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
  
  # Import av file ----
  output$import_av_message <- renderText("Please import analytical validation file")
  
  output$av_file_uploaded <- reactive({
    # Get imported files
    # 1. Import files
    files.df <<- input$av_file # data frame with columns: name, size, type, datapath
    
    # 2. Check if files.df not empty
    if(is.null(files.df)) return(F)
    
    # 3. Validate and preprocess selected av file (imported files df)
    i.df <- data.frame(SHEETS = NA,
                       nrow = NA,
                       ncol = NA,
                       stringsAsFactors = F)
    
    path <- as.character(files.df$datapath)
    # path <- "/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/test_data/PROT-139 Data processing.xlsx"
    SHEETS <<- readxl::excel_sheets(path)
    NAMES <<- NULL
    
    P.DF <<- data.frame()
    CT.DF <<- data.frame()
    for(s in SHEETS) {
      # Define sample name
      name <- as.character(readxl::read_xlsx(path = path, 
                                             sheet = s, 
                                             range = "A1:A1", 
                                             col_names = "sample_name"))
      name <- strsplit(name, ";")[[1]][[2]]
      name <- substring(name, 2, nchar(name))
      NAMES <<- c(NAMES, name)
      
      # Define output
      i.df <- data.frame(sheet = s,
                         name = name,
                         nrow = NA,
                         ntargets = NA,
                         stringsAsFactors = F)
      
      # Extract raw data
      raw.w <- suppressMessages(readxl::read_xlsx(path = path, sheet = s, skip = 1))
      
      # Preprocess Ct data frame --
      # For now, only include the first 15 columns
      ct.w.df <- raw.w[, c(1:15)]
      
      names(ct.w.df) <- c("day", "instrument", "plate", "lot", "x", "ACTB", "RPLP0", "MLANA", "ITGB3", "PLAT", "IL8", "GDF15", "LOXL4", "TGFBR1", "SERPINE2")
      
      i.df$nrow <- nrow(ct.w.df)
      i.df$ntargets <- ncol(ct.w.df)-5
      
      # Remove X from ct.w.df
      ct.w.df <- ct.w.df %>% select(-x)
      
      ct.l.df <- ct.w.df %>% gather(target, Ct, -day, -instrument, -plate, -lot)
      ct.l.df <- ct.l.df %>% mutate(sample = name) %>% select(sample, everything())
      
      CT.DF <<- rbind(CT.DF, as.data.frame(ct.l.df))
      
      # Preprocess probability df --
      p.df <- raw.w[, c(1:5, 24)]
      names(p.df) <- c("day", "instrument", "plate", "lot", "x", "p")
      p.df <- cbind(sample = i.df$name, p.df)
      
      P.DF <<- rbind(P.DF, p.df)
    }
    
    # Round columns with numeric values
    P.DF <- P.DF %>% mutate_if(is.numeric, function(x) round(x+100*.Machine$double.eps, 3))
    CT.DF <- CT.DF %>% mutate_if(is.numeric, function(x) round(x+100*.Machine$double.eps, 3))
    
    if(nrow(P.DF) == 0) {
      return(F)
    } else {
      return(T)
    }
  })
  
  outputOptions(output, 'av_file_uploaded', suspendWhenHidden=FALSE)
  
  # Plot file ----
  source(file.path("server", "server_plot.R"),  local = TRUE)$value  
  
  # Plot trend ----
  source(file.path("server", "server_trend.R"),  local = TRUE)$value  
  
  # Plot sample x lot ----
  source(file.path("server", "server_plot_sample_lot.R"),  local = TRUE)$value  
  
  # Plot sample x lot ----
  source(file.path("server", "server_plot_prob_sample.R"),  local = TRUE)$value  
  
  # Table ----
  source(file.path("server", "server_table.R"),  local = TRUE)$value  
})

# Run application 
shinyApp(ui = ui, server = server)