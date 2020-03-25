# QS shiny - server - trend
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
      select(ID, date, instrument, id, `Sample ID`, `Target Name`, Ct) %>%
      filter(`Sample ID`== "Positive Control")
    
    data.df <- data.df %>% mutate(date = as.POSIXct(date, format = "%d-%m-%Y %H:%M"))
    
    # Depending on the number of targets, set colors
    if(length(unique(data.df$`Target Name`)) == 10) {
      p <- ggplot(data.df, 
                  aes(x = date, y = Ct, colour = `Target Name`, shape = instrument)) +
        geom_point(alpha = 0.5) +
        geom_line() +
        labs(x = "Date",
             colour = "Target name",
             shape = "Instrument") +
        scale_color_manual(values=colors) +
        theme_bw() +
        theme(legend.position="none")
    } else {
      p <- ggplot(data.df, 
                  aes(x = date, y = Ct, colour = `Target Name`, shape = instrument)) +
        geom_point(alpha = 0.5) +
        geom_line() +
        labs(x = "Date",
             colour = "Target name",
             shape = "Instrument") +
        theme_bw() +
        theme(legend.position="none")
    }
    
    p <- ggplotly(p)
    plot.text <- ""
  }
  
  #output$trend <- renderPlot(p)
  output$trend_info <- renderText(plot.text)
  output$trend <- renderPlotly(p)
}
)