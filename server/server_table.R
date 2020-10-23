# QS shiny - server - table
tableFunc <- reactive({
  if(is.null(data.df)) {
    return(NULL)
  }
  
  # Extract selected files
  selected.files <- selectedFiles()
  file.idx <- c(which(if.df$`Exp name` %in% selected.files))
  data.df <- data.df %>% filter(`Exp name` %in% unique(data.df$`Exp name`)[file.idx])
  
  # Depending on selected data_type, return modified data
  if(nrow(data.df) == 0) {
    #data.df <- data.frame(Message = "Please select data"))
    return(data.df)
  }
  
  return(data.df)
})

observeEvent({
  
  input$input_files
},
{
  output$results <- DT::renderDataTable(tableFunc(),
                                        options = list(dom = 'Bfrtip',
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
    output <- prepareOutputDataXlsx(data.df)
    
    xlsx::saveWorkbook(output, file)
  }
)
