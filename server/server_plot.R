# QS shiny - server - plot
# Plot by observing data_type
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
  # * Ct
  # * Cq Conf
  # * MTP
  # * Tm1
  # * id: machine name (QS 5 Dx)
  # --> this has to change!
  
  # Remove other measurements not selected by data_type from data.df
  #plot.type <- sym(input$data_type)
  remove.col <- which(input$data_type != c("Ct", "Cq Conf", "MTP", "Tm1"))
  
  data.df <- data.df[ , !names(data.df) %in% 
                        c("Ct", "Cq Conf", "MTP", "Tm1")[remove.col]]
  
  # Start the party
  data.df <- data.df %>% mutate(type = case_when(
    `Sample ID` == "Positive Control" ~ "Positive control",
    `Sample ID` == "Negative Control" ~ "Negative control",
    T ~ "Sample")) %>%
    mutate(type = factor(type, levels = c("Sample", "Positive control", "Negative control"), ordered = T))
  data.l.df <- data.df %>% gather(variable, value, -`Target Name`, -ID, -instrument, -date, -`Sample ID`, -id, -type)
  
  # Depending on the number of targets, set colors
  if(length(unique(data.l.df$`Target Name`)) == 10) {
    p <- ggplot(data.l.df, 
                aes(x = `Target Name`, y = value, colour = `Target Name`, shape = type)) + 
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(y = input$data_type) +
      scale_color_manual(values=colors) +
      theme_bw() +
      theme(legend.position="none")  
  } else {
    p <- ggplot(data.l.df, 
                aes(x = `Target Name`, y = value, colour = `Target Name`, shape = type)) + 
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(y = input$data_type) +
      theme_bw() +
      theme(legend.position="none")  
  }
  
  output$title <- renderText(paste0(input$data_type, " values for ", data.df$ID[1]))
  output$date <- renderText(paste0("Date: ", data.df$date[1]))
  output$instrument <- renderText(paste0(" Instrument: ", data.df$instrument[1]))
  
  output$plot_file <- renderPlotly(ggplotly(p))
  #output$plot_table <- DT::renderDataTable(missing.df)
  output$plot_table <- DT::renderDataTable(data.l.df)
  output$plot_info <- renderText(plot.text)
})
