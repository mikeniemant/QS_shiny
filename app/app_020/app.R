# QS results dashboard - app

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
    dashboardHeader(title = "V 0.2.0", titleWidth = 100),
    
    # Multiple tabs in sidebare
    dashboardSidebar(
      width = 100,
      sidebarMenu(
        menuItem("Import file", tabName = "import_file"),
        menuItem("Import data", tabName = "import_data")
        )
    ),
    # Dashboard importes R files 
    dashboardBody(
      tabItems(
        tabItem(tabName = "import_file",
                source(file = "./import_file.R", local = TRUE)
        ),
        tabItem(tabName = "import_data",
                source(file = "./import_data.R", local = TRUE)
        )
      )
    )
  ),
  
  # All functionality in the back-end
server = shinyServer(function(input, output, session) {
  # TAB1: IMPORT FILE ------------------------------------------------------------
  getData <- function(datapath, sheet) {
    raw <- suppressMessages(readxl::read_xlsx(path = datapath, sheet = sheet))
    processQSResults(raw, sheet)
  }

  # Render DataTable
  output$QS_results <- renderDataTable({
    # If no import return null or 
    import <- input$QS_file
    if(is.null(import)) return(NULL)
    
    # Extract and return data object
    data <- getData(datapath = import$datapath, sheet = "Results")$data
    return(data)
  }, options = list(paging = FALSE)
  )
  
  get_plot <- reactive({
    import <- input$QS_file
    if(is.null(import)) return(NULL)
    
    data <- getData(datapath = import$datapath, sheet = "Results")
    
    if(is.null(data)) return(list(message = ""))
    df <- data$data
    
    plotType <- sym(input$type)
    
    if(plotType == "Positive Control" | plotType == "Negative Control") {
      df <- df %>% filter(`Sample ID` == input$type)
      missing_df <- df %>% filter(is.na(cT))
      
      if(nrow(missing_df) == nrow(df)) {
        p <- NULL
        message <- "No cT values were found"
      } else if(nrow(missing_df) > 0){
        p <- ggplot(df,
                    aes(x=`Target Name`, y = cT)) +
          geom_jitter(width = 0.2) +
          labs(title = data$name,
               subtitle = data$date) +
          theme_bw()
        message <- paste0(nrow(missing_df), " missing cT values were found")
      } else {
        p <- ggplot(df,
                    aes(x=`Target Name`, y = cT)) +
          geom_jitter(width = 0.2) +
          labs(title = data$name,
               subtitle = data$date) +
          theme_bw()
        message <- "No errors"
      }
      
    } else {
      df <- df %>% 
        select(`Sample ID`, `Target Name`, !!plotType)
      
      if(any(is.na(df))) {
        message <- paste0(sum(is.na(df)), " missing values in output")
      } else {
        message <- "No errors"
      }
      
      missing_df <- df %>% filter(is.na(!!plotType))
      
      # Change shape for positive and negative control
      df <- df %>% mutate(shape = as.factor(case_when(
        `Sample ID` == "Positive Control" ~ "Positive Control",
        `Sample ID` == "Negative Control" ~ "Negative Control",
        T ~ "Gene"
      )))
      
      p <- ggplot(df,
                  aes(x=`Target Name`, y = !!plotType, colour = `Sample ID`, shape = shape)) +
        geom_jitter(width = 0.2) +
        labs(title = data$name,
             subtitle = data$date,
             shape = "Type") +
        theme_bw()
    }

    obj <- list(plot = p,
                message = message,
                missing_df = missing_df)
    obj
  })
  
  # Visualize plot
  output$plot_file <- renderPlot({
    get_plot()$plot
  }
  )
  
  # Show message
  output$plot_text <- renderText(get_plot()$message)
  
  # Visualize missing df
  output$missing_df <- renderTable({
    df <- get_plot()$missing_df
    if(is.null(df)) {
      return()
    } else if(nrow(df) == 0) {
      return()
    }
    df
  }
  )

  # IMPORT DATA ------------------------------------------------------------
  # Retrieve imported files
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
  
  # Check boxes that visualize the imported files
  # - For checked check boxes, get and preprocess data
  observeEvent({

    input$QS_files
  },
  {
    files <- get_imported_files()

    if(nrow(files) > 0) {
      updateCheckboxGroupInput(session, "selected_files",
                               choices = files$name,
                               selected = files$name)
    } else {
      updateCheckboxGroupInput(session, "selected_files",
                               choices = "")
    }
  })

  # Extract files that are selected by checkbox
  get_selected_files <- reactive({
    files <- input$selected_files
    if(length(files) > 0) {
      return(files)
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