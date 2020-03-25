# QS results dashboard - 030 - server

library(shiny)
library(shinydashboard)
library(shinyFiles)
library(dplyr)
library(ggplot2)
library(tidyr)
library(xlsx)
library(DT)

# import global variables and functions
source("./global.R")

shinyApp(
  # Dashboard functionality (front-end)
  ui <- dashboardPage(
    dashboardHeader(title = "V 0.3.0", titleWidth = 100),
    
    # Multiple tabs in sidebare
    dashboardSidebar(
      width = 100,
      sidebarMenu(
        menuItem("Main", tabName = "ui")
        )
    ),
    # Dashboard importes R files 
    dashboardBody(
      tabItems(
        tabItem(tabName = "ui",
                source(file = "./ui.R", local = TRUE)
        )
      )
    )
  ),
  
  # All functionality in the back-end
server = shinyServer(function(input, output, session) {
  # Retrieve imported files ----
  get_imported_files <- reactive({
    files <- input$QS_files
    if(length(files) > 0) {
      return(files)
    } else {
      return("")
    }
  })
  
  # Show all imported files
  output$files <- renderPrint(get_imported_files())
  
  # Check boxes imported files ----
  # Check boxes that visualize the imported files
  # - For checked check boxes, get and preprocess data
  observeEvent({

    input$QS_files
  },
  {
    files <- get_imported_files()

    if(nrow(files) > 0) {
      names <- unlist(lapply(files$name, function(x) substr(x, 1, nchar(x)-5)))
      updateCheckboxGroupInput(session, "selected_files",
                               choices = names,
                               selected = names)
    } else {
      updateCheckboxGroupInput(session, "selected_files",
                               choices = "")
    }
  })

  # Extract files that are selected by checkbox
  get_selected_files <- reactive({
    files <- input$selected_files
    if(length(files) > 0) {
      return()
    } else {
      return()
    }
  })

  # Show selected files
  output$selected_files <- renderPrint(get_selected_files())
  
  # Preprocess data of selected files
  preprocessSelectedFiles <- function() {
    # Extract selected files
    selected.files <- get_selected_files()
    
    if(is.null(selected.files)) return()
    
    # Extract imported files object to define path
    files.df <- get_imported_files()
    
    # Check if a filename is included twice in the vector
    n.files <- sum(unlist(lapply(files.df$name, function(x) length(which(files.df$name %in% x)))))
    if(n.files != nrow(files.df)) {
      df <- data.frame(ID = "error_1",
                       message = "Please import files with unique file names")
      return(df)
    }
    
    # Preprocess selected files
    df <- data.frame()
    for(i in selected.files) {
      path = files.df %>% filter(name == i) %>% pull(datapath)
      sheets <- readxl::excel_sheets(path)
      if(!"Results" %in% sheets) {
        df <- data.frame(ID = "error_2",
                         message = paste0(as.character("test"), " incorrect file. Please import QuantStudio output files"))
        return(df)
      }
      raw <- suppressMessages(readxl::read_xlsx(path = as.character(path), 
                                                sheet = "Results"))
      
      # Check if all files are Quant studio files
      if(!validateFileImport(raw)) {
        df <- data.frame(ID = "error_2",
                         message = paste0(as.character("test"), " incorrect file. Please import QuantStudio output files"))
        return(df)
      }
      
      pp <- processQSResults(raw)
      
      pp <- pp$data %>% 
        mutate(ID = pp$name,
               date = pp$date,
               instrument = pp$instr) %>% 
        select(ID, instrument, date, everything())
      
      df <- rbind(df, pp)
    }
    return(df)
  }
  
  # Button to (de-)select all imported files) ----
  observeEvent(input$select, {
    files <- get_imported_files()
    
    if(is.null(files)) return()
    if(files$name[1] == "") return()

    updateCheckboxGroupInput(session, "selected_files",
                             #choices = files$name,
                             selected = unlist(lapply(files$name, function(x) substr(x, 1, nchar(x)-5))))
  })
  
  observeEvent(input$deselect, {
    files <- get_imported_files()
    
    if(is.null(files)) return()
    if(files$name[1] == "") return()
    updateCheckboxGroupInput(session, "selected_files",
                             selected = "")
  })

  # Positive / negative choice box ----
  # Observe event for pos and negative control selection
  observeEvent({

    input$data_type
  },
  {
    if(input$data_type == "Positive Control") {
      updateCheckboxGroupInput(session, "control",
                               choices = c("Positive", "Negative"),
                               selected = "Positive",
                               )
    } else if(input$data_type == "Negative Control") {
      updateCheckboxGroupInput(session, "control",
                               choices = c("Positive", "Negative"),
                               selected = "Negative")
    } else{
      updateCheckboxGroupInput(session, "control",
                               choices = c("Positive", "Negative"),
                               selected = "")
    }
  })
  
  # Table ----
  output$QS_all_results <- renderDataTable(
    {
      # Visualize output in table
      df <- preprocessSelectedFiles()
      # Depending on selected data_type, return modified data
      if(!is.null(df)) {
        if(df$ID[1] == "error_1" | df$ID[1] == "error_2") {
          return(df[1, ])
        }
        
        if(input$data_type == "All"){
          df
        } else if(input$data_type == "Positive Control" | input$data_type == "Negative Control"){
          df <- df %>%
            select(ID, date, `Sample ID`, `Target Name`, cT) %>%
            spread(`Target Name`, cT) %>%
            filter(`Sample ID` == input$data_type)
        } else {
          df <- df %>%
            select(ID, date, `Sample ID`, `Target Name`, input$data_type) %>%
            spread(`Target Name`, input$data_type) %>% 
            filter(`Sample ID` != "Positive Control" & `Sample ID` != "Negative Control")
        }
      } else {
        return()
      }
      if(!is.null(input$control)) {
        if(length(input$control) != 2) {
          if(input$control[1] == "Positive") {
            df %>%
              filter(!`Sample ID` %in% "Negative Control")
          } else if(input$control[1] == "Negative") {
            df %>%
              filter(!`Sample ID` %in% "Positive Control")
          }
        } else {
          df
        }
      } else {
        df %>%
          filter(!`Sample ID` %in% c("Positive Control", "Negative Control"))
      }
    },
    extensions = c('Buttons'),
    options = list(dom = 'Bfrtip',
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                   paging = FALSE,
                   scrollX = TRUE)
  )
  
  # Download button ----
  output$downloadData <- downloadHandler(
    filename = paste0("exported_QS_data_", strftime(Sys.time(), "%Y-%m-%d_%H-%M"), ".xlsx"),
    content = function(file) {
      if(length(input$selected_files) > 0) {
        df <- preprocessSelectedFiles()
      } else {
        return()
      }
      
      # Prepare data
      # - Preprocess every parameter dataframe
      # - Include as sheet
      # - Extract complete xlsx file
      output <- prepareDataXlsx(df)

      saveWorkbook(output, file)
    }
  )

  # Plot ----
  # Plot trend by observing data_type
  observeEvent({

    c(input$data_type, input$selected_files)
  },
  {
    df <- preprocessSelectedFiles()
    if(!is.null(df)) {
      if(df$ID[1] == "error_1" | df$ID[1] == "error_2") {
        return()
      }
      if(input$data_type == "Positive Control") {
        output$plot_trend <- renderPlot({
          df <- preprocessSelectedFiles() # --> returns ALL data_type
          
          df <- df %>%
            select(ID, date, instrument, `Sample ID`, `Target Name`, cT) %>%
            filter(`Sample ID`== "Positive Control")

          df <- df %>% mutate(date = as.POSIXct(date, format = "%d-%m-%Y %H:%M"))

          ggplot(df,
                 aes(x = date, y = cT, colour = `Target Name`, shape = instrument)) +
            geom_point() +
            geom_line() +
            #facet_wrap(~`Target Name`) +
            theme_bw()
        })
      } else {
        output$plot_trend <- NULL
      }
    } else {
      output$plot_trend <- NULL
    }
  })
}
)
)