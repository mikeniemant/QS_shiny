# QS results dashboard - app

library(shiny)
library(shinydashboard, warn.conflicts = F)
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
    dashboardHeader(title = "V 0.1.0"),
    
    dashboardSidebar(
      sidebarMenu(
        # menuItem("Main", tabName = "main"),
        menuItem("Import file", tabName = "import_file"),
        menuItem("Import data", tabName = "import_data")
      )
    ),
    dashboardBody(
      tabItems(
        # tabItem(tabName = "main",
        #         source(file = "./main.R", local = TRUE)
        # ),
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
  # IMPORT FILE ------------------------------------------------------------
  # Reactive to import data from input
  get_data <- reactive({
    # If no import return null or 
    import <- input$QS_file
    if(is.null(import)) return(NULL)
  
    # Else preprocess import data
    raw <- suppressMessages(readxl::read_xlsx(path = import$datapath, sheet = input$sheet))
    processQSResults(raw, input$sheet)
  }
  )

  # Render DataTable
  output$QS_results <- renderDataTable({
    import <- input$QS_file
    if(is.null(import))
      return(NULL)
    get_data()$data
  }, options = list(paging = FALSE)
  )
  
  get_plot <- reactive({
    df <- get_data()$data
    
    if(is.null(df)) return(list(message = ""))
    
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
          labs(title = get_data()$name,
               subtitle = get_data()$date) +
          theme_bw()
        message <- paste0(nrow(missing_df), " missing cT values were found")
      } else {
        p <- ggplot(df,
                    aes(x=`Target Name`, y = cT)) +
          geom_jitter(width = 0.2) +
          labs(title = get_data()$name,
               subtitle = get_data()$date) +
          theme_bw()
        message <- "No errors"
      }
      
    } else {
      df <- get_data()$data %>% 
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
        labs(title = get_data()$name,
             subtitle = get_data()$date,
             shape = "Type") +
        theme_bw()
    }

    obj <- list(plot = p,
                message = message,
                missing_df = missing_df)
    obj
  })

  output$plot_file <- renderPlot({
    get_plot()$plot
  }
  )
  
  output$plot_text <- renderText(get_plot()$message)
  
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
  volumes <- getVolumes()
  shinyDirChoose(input, 
                 'dir', 
                 roots = volumes, 
                 filetypes = c("", ".", "txt", "xlsx")
                 ) 
  
  dir <- reactive({
    output <- input$dir
    if(length(output[[1]]) == 1) {
      return("")
    } else {
      # Prepare path
      home <- normalizePath(PATH)
      path <- file.path(home, paste(unlist(input$dir$path[-1]), collapse = .Platform$file.sep))
      return(path)
    }
  })
  
  output$dir <- renderPrint(dir())

   # files
  get_files <- reactive({
    files <- list.files(dir(), pattern = "*.xlsx")
    if(length(files) > 0) {
      return(files)
    } else {
      return("")
    }
  })

  output$files <- renderPrint(get_files())
  
  # Observe event to list files
  observeEvent({
    
    input$dir
  }, 
  {
    files <- get_files()
    if(files[1] != "") {
      updateCheckboxGroupInput(session, "selected_files",
                               choices = files,
                               selected = files) 
    } else {
      updateCheckboxGroupInput(session, "selected_files", 
                               choices = "")
    }
  })
  
  get_checked_file <- reactive({
    files <- input$selected_files
    if(length(files) > 0) {
      return(files)
    } else {
      return("")
    }
  })
  
  output$checked_files <- renderPrint(get_checked_file())
  
  # Preprocess data
  # Reactive to import all selected files
  get_all_data <- reactive({
    # If no import return null or 
    x <- input$selected_files
    if(is.null(x)) return(NULL)
    
    # Else preprocess import data
    df <- data.frame()
    for(i in x) {
      raw <- suppressMessages(readxl::read_xlsx(path = paste0(dir(), "/", i), sheet = "Results"))
      pp <- processQSResults(raw, "Results")
      pp <- pp$data %>% 
        mutate(ID = pp$name,
               date = pp$date) %>% 
        select(ID, date, everything())
      
      df <- rbind(df, pp)
    }
    return(df)
  }
  )
  
  # Observe event for pos and negative control selection
  observeEvent({
    
    input$data_type
  }, 
  {
    if(input$data_type == "Positive Control") {
      updateCheckboxGroupInput(session, "control",
                               choices = c("Positive", "Negative"),
                               #choiceValues = c("Positive Control", "Negative Control"),
                               selected = "Positive",
                               )
    } else if(input$data_type == "Negative Control") {
      updateCheckboxGroupInput(session, "control",
                               choices = c("Positive", "Negative"),
                               #choiceValues = c("Positive Control", "Negative Control"),
                               selected = "Negative") 
    } else{
      updateCheckboxGroupInput(session, "control",
                               choices = c("Positive", "Negative"),
                               #choiceValues = c("Positive Control", "Negative Control"),
                               selected = "")
    }
  })
  
  output$QS_all_results <- renderDataTable(
    {
      df <- get_all_data()
      if(!is.null(df)) {
        if(input$data_type == "All"){
          get_all_data(input$control)
        } else if(input$data_type == "Positive Control" | input$data_type == "Negative Control"){
          df <- df %>% 
            select(ID, date, `Sample ID`, `Target Name`, cT) %>% 
            spread(`Target Name`, cT) %>% 
            filter(`Sample ID` == input$data_type)
        } else {
          df <- df %>% 
            select(ID, date, `Sample ID`, `Target Name`, input$data_type) %>% 
            spread(`Target Name`, input$data_type)
        }
      } else {
        return()
      }
      if(!is.null(input$control)) {
        # message(input$control
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
                   paging = FALSE)
  )
  

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("exported_QS_data_", strftime(Sys.time(), "%Y-%m-%d_%H-%M"), ".xlsx")
    },
    content = function(file) {
      if(length(input$selected_files) > 0) {
        df <- get_all_data()  
      } else {
        return()
      }

      # Prepare data
      cT <- df %>% 
        select(ID, date, `Sample ID`, `Target Name`, cT) %>% 
        spread(`Target Name`, cT) %>% 
        filter(`Sample ID` != "Positive Control" & `Sample ID` != "Negative Control")
      `Cq Conf` <- df %>% 
        select(ID, date, `Sample ID`, `Target Name`, `Cq Conf`) %>% 
        spread(`Target Name`, `Cq Conf`)
      pc <- df %>% 
        select(ID, date, `Sample ID`, `Target Name`, cT) %>% 
        spread(`Target Name`, cT) %>% 
        filter(`Sample ID` == "Positive Control")
      nc <- df %>% 
        select(ID, date, `Sample ID`, `Target Name`, cT) %>% 
        spread(`Target Name`, cT) %>% 
        filter(`Sample ID` == "Negative Control")
      MTP <- df %>% 
        select(ID, date, `Sample ID`, `Target Name`, MTP) %>% 
        spread(`Target Name`, MTP)
      Tm1 <- df %>% 
        select(ID, date, `Sample ID`, `Target Name`, Tm1) %>% 
        spread(`Target Name`, Tm1)
      
      # Create workbook and fill with sheets
      output <- xlsx::createWorkbook()
      
      output_cT <- createSheet(wb=output, sheetName="cT")
      output_Cq_Conf <- createSheet(wb=output, sheetName="Cq Conf")
      output_pc <- createSheet(wb=output, sheetName="Positive Control")
      output_nc <- createSheet(wb=output, sheetName="Negative Control")
      output_MTP <- createSheet(wb=output, sheetName="MTP")
      output_Tm1 <- createSheet(wb=output, sheetName="Tm1")
      
      addDataFrame(x=cT, sheet=output_cT)
      addDataFrame(x=`Cq Conf`, sheet=output_Cq_Conf)
      addDataFrame(x=pc, sheet=output_pc)
      addDataFrame(x=nc, sheet=output_nc)
      addDataFrame(x=MTP, sheet=output_MTP)
      addDataFrame(x=Tm1, sheet=output_Tm1)
      
      saveWorkbook(output, file)
    }
  )
  
  # Plot trend by observing data_type
  observeEvent({
    
    c(input$data_type, input$selected_files)
  }, 
  {
    df <- get_all_data()
    if(!is.null(df)) {
      if(input$data_type == "Positive Control" | input$data_type == "Negative Control") {
        output$plot_trend <- renderPlot({
          df <- get_all_data() # --> returns ALL data_type
          
          df <- df %>% 
            select(ID, date, `Sample ID`, `Target Name`, cT)

          if(input$data_type == "Positive Control") {
            df <- df %>% 
              filter(`Sample ID`== "Positive Control")
          } else if(input$data_type == "Negative Control") {
            df <- df %>% 
              filter(`Sample ID` == "Negative Control")
          }

          df <- df %>% mutate(date = as.POSIXct(date, format = "%d-%m-%Y %H:%M"))

          ggplot(df,
                 aes(x = date, y = cT, colour = `Target Name`)) +
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
  # output$plot_trend <- renderPlot({
  #     
  #   }
  # )
}
)
)