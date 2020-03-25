# QS shiny - server - plot sample lot
# Radio button imported samples
observeEvent({
  
  input$av_file
},
{
  if(length(NAMES) > 0) {
    updateRadioButtons(session, "radio_av_samples",
                       choices = NAMES,
                       selected = NAMES[1])
  } else {
    updateCheckboxGroupInput(session, "radio_av_samples",
                             choices = "")
  }
})

# Sample x lot plot
observeEvent({
  
  c(input$av_file, input$radio_av_samples)
},
{
  if(length(NAMES) > 0) {
    name <- input$radio_av_samples
    if(name == "") {
      return()
    }
  
    ct.df <- CT.DF %>% filter(sample == name)
    # ct.df <- CT.DF
    if(length(unique(ct.df$target)) == 10) {
      sample.lot.p <- ggplot(ct.df, aes(x = target, y = Ct)) +
        geom_boxplot(aes(fill = as.factor(lot)), alpha = 0.5) +
        #geom_jitter(aes(color = target), width = 0.2, alpha = 0.5) +
        #scale_color_manual(values=colors) +
        labs(x = "Target",
             y = "Ct",
             fill = "Lot",
             color = "Target",
             title = paste0("Ct distribution of analytical validation sample: ", name)) +
        theme_bw()    
    } else {
      sample.lot.p <- ggplot(ct.df, aes(x = target, y = Ct)) +
        geom_boxplot(aes(fill = as.factor(lot)), alpha = 0.5) +
        labs(x = "Target",
             y = "Ct",
             fill = "Lot",
             color = "Target",
             title = paste0("Ct distribution of analytical validation sample: ", name)) +
        theme_bw()
    }
    
    #output$sample_lot_plot <- renderPlotly(plotly::ggplotly(sample.lot.p))
    output$sample_lot_plot <- renderPlot(sample.lot.p)
  }
})

