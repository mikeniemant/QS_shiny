# QS shiny - server - table
tableFunc <- reactive({
  if(is.null(data.df)) {
    return(NULL)
  }
  
  # Extract selected files
  selected.files <- selectedFiles()
  # file.idx <- which(files.df$name %in% paste0(selected.files, ".xlsx"))
  file.idx <- which(files.df$name %in% selected.files)
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

observeEvent({
  
  input$input_files
},
{
  output$results <- DT::renderDataTable(tableFunc(),
                                        extensions = c('Buttons'),
                                        options = list(dom = 'Bfrtip',
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                       paging = FALSE,
                                                       iscrollX = TRUE))
}
)

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
