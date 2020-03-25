# QS shiny - server - probability x sample lot
# Probability x sample plot
observeEvent({
  
  c(input$av_file, input$radio_av_samples)
},
{
  if(length(SHEETS) > 0) {
    # Plot probability x sample
    prob.sample.p <- ggplot(P.DF, aes(x = sample, y = p)) +
      geom_boxplot(alpha = 0.5) +
      geom_point() +
      labs(x = "Sample",
           y = "Probability",
           title = "Probability distribution of analytical validation samples") +
      scale_y_continuous(limits = c(0,0.1)) +
      theme_bw()
    
    output$prob_sample_plot <- renderPlotly(ggplotly(prob.sample.p))
    
    # Compute SD
    output$prob_table <- DT::renderDataTable(P.DF %>% 
                                               group_by(sample) %>% 
                                               summarize(sd_sample = sd(p)) %>% 
                                               ungroup() %>% 
                                               mutate(sd_sample = round(sd_sample, 5)) %>% 
                                               as.data.frame(),
                                          #extensions = c('Buttons'),
                                          options = list(dom = 'Bfrtip',
                                                         #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                         paging = FALSE,
                                                         iscrollX = TRUE))
  }
})
