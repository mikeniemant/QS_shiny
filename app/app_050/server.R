# QS results dashboard - 041 - server

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
  titlePanel(title = "V 0.5.0"),
  
  fluidRow(
    # Inputs: edit in this file:
    column(3,
           source(file.path("ui", "sidebar.R"), local = TRUE)$value),
    
    # Outputs: edit in these files:
    column(8,
           tabsetPanel(id = "tabs",
                       source(file.path("ui", "main.R"),  local = TRUE)$value,
                       source(file.path("ui", "plot_file.R"),  local = TRUE)$value,
                       source(file.path("ui", "plot_trend.R"),  local = TRUE)$value,
                       source(file.path("ui", "table.R"),  local = TRUE)$value
           )
    )
  )
)

# All functionality in the back-end
# Server ----
server <- shinyServer(function(input, output, session) {
  # Import files ----
  output$file_uploaded <- reactive({
    output$import_message <- renderText("Please import files")
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
      i.df <- data.frame(results.sheet = F,
                         empty.file = F,
                         Name = NA,
                         Date = NA,
                         Instrument_ID = NA, 
                         N_missing_values = NA,
                         stringsAsFactors = F)
      
      sheets <- readxl::excel_sheets(path)
      if(!"Results" %in% sheets) {
        return(i.df)
      } else {
        i.df$results.sheet = T
      }
      # Extract raw data
      raw <- suppressMessages(readxl::read_xlsx(path = as.character(path), 
                                                sheet = "Results"))
      
      # Check if file is not empty
      if(nrow(raw) == 0) {
        i.df$empty.file = T
        message(nrow(raw))
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
    missing.results.rows <- which(!if.df$results.sheet)
    
    if(length(missing.results.rows) > 0) {
      output$import_message <- renderText(paste0(ifelse(length(missing.results.rows), "File ", "Files "), missing.results.rows, " missing `Results` sheet"))
      output$input_files_table <- DT::renderDataTable(if.df)
      return(F)
    }
    
    # Check for empty files
    empty.files <<- which(if.df$empty.file)
    if(length(empty.files) > 0) {
      output$import_message <- renderText(paste0(ifelse(length(empty.files) == 1, "File ", "Files "), empty.files, " no data"))
      output$input_files_table <- DT::renderDataTable(if.df)
      return(F)
    }
    
    # Check if all other values are valid
    incomplete.rows <- which(unname(apply(if.df %>% select(-empty.file), 2, function(x) sum(x == F)>0)))
    
    if(length(incomplete.rows) > 0) {
      output$import_message <- renderText(paste0(ifelse(length(incomplete.rows) == 1, "File ", "Files "), incomplete.rows, " incomplete"))
      
      output$input_files_table <- DT::renderDataTable(if.df)
      return(F)
    } else {
      output$import_message <- renderText(paste0("Successfully imported and pre-processed ", nrow(if.df), ifelse(nrow(if.df) == 0, " file", " files")))
      output$input_files_table <- DT::renderDataTable(if.df %>% select(-results.sheet))
      
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
  # Plot file ----
  # *radio button imported files ----
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
  
  # *radio button extract file ----
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
  
  # Plot ----
  # Plot trend by observing data_type
  observeEvent({
    
    c(input$tabs, input$data_type, input$radio_selected_file, input$control)
  },
  {
    if(is.null(data.df)) {
      return(NULL)
    }
    
    # Depending on the number of selected files
    plot.type <- sym(input$data_type)
    
    # Filter data of selected files (name)
    file.name <- extractSelectedFile()
    file.idx <- which(unique(files.df$name) %in% paste0(file.name, ".xlsx"))
    data.df <- data.df %>% filter(ID %in% unique(data.df$ID)[file.idx])
    
    # Remove data from control selection
    if(!is.null(input$control)) {
      if(length(input$control) == 2) {
        data.df <- data.df
      } else if(input$control[1] == "positive") {
        data.df <- data.df %>% filter(!`Sample ID` %in% "Negative Control")
      } else if(input$control[1] == "negative") {
        data.df <- data.df %>% filter(!`Sample ID` %in% "Positive Control")
      }
    } else {
      data.df <- data.df %>% 
        filter(!`Sample ID` %in% c("Positive Control", "Negative Control"))
    }
    
    # Define missing data frame
    missing.df <- data.df %>% filter(is.na(!!plot.type))
    if(nrow(missing.df) > 0) {
      data.df <- data.df %>% filter(!is.na(!!plot.type))
      plot.text <- paste0(nrow(missing.df), "  missing ", 
                          ifelse(nrow(missing.df) == 1, "value", "values"), 
                          " were found")
    } else {
      plot.text <- ""
      missing.df <- NULL
    }
    
    
    # Rebuild the data.df data frame
    # * ID: name run/file
    # * instrument: number machine
    # * date: date
    # * Sample ID: S1272122
    # * cT
    # * Cq Conf
    # * MTP
    # * Tm1
    # * id: machine name (QS 5 Dx)
    # --> this has to change!
    
    # Remove other measurements not selected by data_type from data.df
    #plot.type <- sym(input$data_type)
    remove.col <- which(input$data_type != c("cT", "Cq Conf", "MTP", "Tm1"))
    
    data.df <- data.df[ , !names(data.df) %in% 
                          c("cT", "Cq Conf", "MTP", "Tm1")[remove.col]]
    
    # Start the party
    data.df <- data.df %>% mutate(type = case_when(
      `Sample ID` == "Positive Control" ~ "Positive control",
      `Sample ID` == "Negative Control" ~ "Negative control",
      T ~ "Sample")) %>%
      mutate(type = factor(type, levels = c("Sample", "Positive control", "Negative control"), ordered = T))
    data.l.df <- data.df %>% gather(variable, value, -`Target Name`, -ID, -instrument, -date, -`Sample ID`, -id, -type)
    
    # Draw basic plot
    p <- ggplot(data.l.df, 
                aes(x = `Target Name`, y = value, colour = `Target Name`, shape = type)) + 
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(y = input$data_type) +
      scale_color_manual(values=colours) +
      theme_bw() +
      theme(legend.position="none")
  
    output$title <- renderText(paste0(input$data_type, " values for ", data.df$ID[1]))
    output$date <- renderText(paste0("Date: ", data.df$date[1]))
    output$instrument <- renderText(paste0(" Instrument: ", data.df$instrument[1]))
  
    output$plot_file <- renderPlotly(ggplotly(p))
    #output$plot_table <- DT::renderDataTable(missing.df)
    output$plot_table <- DT::renderDataTable(data.l.df)
    output$plot_info <- renderText(plot.text)
  })
  
  # Plot trend ----
  observeEvent({
    
    c(input$selected_files, input$select, input$deselect)
  },
  {
    if(is.null(data.df)) {
      return(NULL)
    }
    
    # Extract selected files
    selected.files <- selectedFiles()
    file.idx <- which(files.df$name %in% paste0(selected.files, ".xlsx"))
    data.df <- data.df %>% filter(ID %in% unique(data.df$ID)[file.idx])

    if(nrow(data.df) == 0) { 
      p <- NULL
      plot.text <- "No files selected"
    } else if(length(selectedFiles()) == 1) {
      p <- NULL
      plot.text <- "Please select more than one file"
    } else {
      data.df <- data.df %>%
        select(ID, date, instrument, id, `Sample ID`, `Target Name`, cT) %>%
        filter(`Sample ID`== "Positive Control")
      
      data.df <- data.df %>% mutate(date = as.POSIXct(date, format = "%d-%m-%Y %H:%M"))
      
      p <- ggplot(data.df, 
                  aes(x = date, y = cT, colour = `Target Name`, shape = instrument)) +
        geom_point(alpha = 0.5) +
        geom_line() +
        labs(x = "Date",
             colour = "Target name",
             shape = "Instrument") +
        scale_color_manual(values=colours) +
        theme_bw() +
        theme(legend.position="none")
      p <- ggplotly(p)
      plot.text <- ""
    }
    
    #output$trend <- renderPlot(p)
    output$trend_info <- renderText(plot.text)
    output$trend <- renderPlotly(p)
  }
  )
  
  # Table ----
  tableFunc <- reactive({
    if(is.null(data.df)) {
      return(NULL)
    }
    
    # Extract selected files
    selected.files <- selectedFiles()
    file.idx <- which(files.df$name %in% paste0(selected.files, ".xlsx"))
    data.df <- data.df %>% filter(ID %in% unique(data.df$ID)[file.idx])

    # Depending on selected data_type, return modified data
    if(nrow(data.df) == 0) {
      #data.df <- data.frame(Message = "Please select data"))
      return(data.df)
    }
    if(!is.null(input$control) & !is.null(data.df)) {
      if(length(input$control) == 2) {
        if(input$control[1] == "Positive" & input$control[2] == "Negative") {
          data.df <- data.df
        }
      } else if(input$control[1] == "Positive") {
        data.df <- data.df %>%
          filter(!`Sample ID` %in% "Negative Control")
      } else if(input$control[1] == "Negative") {
        data.df <- data.df %>%
          filter(!`Sample ID` %in% "Positive Control")
      } else {
        data.df <- data.df %>%
          filter(!`Sample ID` %in% c("Positive Control", "Negative Control"))
      }
    } else if(is.null(input$control) & !is.null(data.df)) {
      data.df <- data.df %>%
        filter(!`Sample ID` %in% c("Positive Control", "Negative Control"))
    }
    
    return(data.df)
  })
  
  output$results <- DT::renderDataTable(tableFunc(),
                                        extensions = c('Buttons'),
                                        options = list(dom = 'Bfrtip',
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       paging = FALSE,
                                                       iscrollX = TRUE))
  
  # Download button ----
  output$downloadData <- downloadHandler(
    filename = paste0("exported_QS_data_", strftime(Sys.time(), "%Y-%m-%d_%H-%M"), ".xlsx"),
    content = function(file) {
      
      if(is.null(data.df)) return()
      
      # Prepare data
      # - Preprocess every parameter dataframe
      # - Include as sheet
      # - Extract complete xlsx file
      output <- prepareDataXlsx(data.df)
      
      xlsx::saveWorkbook(output, file)
    }
  )
})

# Run application 
shinyApp(ui = ui, server = server)