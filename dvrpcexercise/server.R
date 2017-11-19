#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic
shinyServer(function(input, output) {
  
  national.data <- reactive({
    if(input$Rank == 'nonwhite'){
      blah <- top.tracts[top.tracts$nw.nat.rank <= input$N, ]
      blah <- blah[order(blah$nw.nat.rank), ]
      return(blah)
    }
    if(input$Rank == 'lowincome'){
      blah <- top.tracts[top.tracts$pov.nat.rank <= input$N, ]
      blah <- blah[order(blah$pov.nat.rank), ]
      return(blah)
    }
    
  })
  
  output$national.plot1 <- renderPlot({
    ggplot(national.data(), aes(x = factor(tract.label, levels = unique(tract.label)), y = nonwhite.est)) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = nonwhite.est - nonwhite.err,
                        ymax = nonwhite.est + nonwhite.err,
                        width = 0.5)) +
      xlab("Tract (Number, County, State)") +
      ylab("Non-white Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))
  })
  
  output$national.plot2 <- renderPlot({
    ggplot(national.data(), aes(x = factor(tract.label, levels = unique(tract.label)), y = lowincome.est)) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = lowincome.est - lowincome.err,
                        ymax = lowincome.est + lowincome.err,
                        width = 0.5)) +
      xlab("Tract (Number, County, State)") +
      ylab("Low-income Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))
  })
  
  # need a table output
  # national (cut to top 10 depending on reactive input, or show all for combo rank)
  # reactive for each state
  
  # plot outputs
  ## a ggplot showing top based on selection?
  ### national: poverty, income, combo
  ### allow input for top 10, 20, 30.... (slider input?)
  
  ### per state, the top 10 nonwhite or poverty
  ### how can i select for combo rank? (perhaps sort the table, then chop it at 10?)
   

  })
