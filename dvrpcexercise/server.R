#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
load("top.tracts")

# Define server logic
shinyServer(function(input, output) {
  
  national.data <- reactive({
    top.tracts <- top.tracts[, c(1, 3:5, 8:11, 15:17)]
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
    
    # might have to redo combo rankings.... is rank-summing the legit move here?
    if(input$Rank == 'combo'){
      blah <- top.tracts[order(top.tracts$combo.nat.rank), ]
      blah <- blah[1:input$N, ]
      return(blah)
    }
  })
  
  state.data <- reactive({
    top.tracts <- top.tracts[, c(1, 3:5, 8:14)]
      blah <- top.tracts[top.tracts$state == input$State, ]
      if(input$Rank == 'nonwhite'){
        blah <- blah[blah$nonwhite.rank <= 10, ]
        blah <- blah[order(blah$nonwhite.rank), ]
        return(blah)
      }
      if(input$Rank == 'lowincome'){
        blah <- blah[blah$poverty.rank <= 10, ]
        blah <- blah[order(blah$poverty.rank), ]
        return(blah)
      }
      if(input$Rank == 'combo'){
        blah <- blah[order(blah$combo.rank), ]
        blah <- blah[1:10, ]
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
  
  output$national.plot3 <- renderPlot({
    ggplot(national.data(), aes(x = factor(tract.label, levels = unique(tract.label)),
                                y = (nonwhite.est + lowincome.est))) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = (nonwhite.est + lowincome.est) - (nonwhite.err + lowincome.err),
                        ymax = (nonwhite.est + lowincome.est) + (nonwhite.err + lowincome.err),
                        width = 0.5)) +
      xlab("Tract (Number, County, State)") +
      ylab("Nonwhite + Low-income Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))
  })
  
  output$state.plot1 <- renderPlot({
    ggplot(state.data(), aes(x = factor(tract.label, levels = unique(tract.label)), y = nonwhite.est)) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = nonwhite.est - nonwhite.err,
                        ymax = nonwhite.est + nonwhite.err,
                        width = 0.5)) +
      xlab("Tract (Number, County, State)") +
      ylab("Non-white Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))
  })
  
  output$state.plot2 <- renderPlot({
    ggplot(state.data(), aes(x = factor(tract.label, levels = unique(tract.label)), y = lowincome.est)) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = lowincome.est - lowincome.err,
                        ymax = lowincome.est + lowincome.err,
                        width = 0.5)) +
      xlab("Tract (Number, County, State)") +
      ylab("Low-income Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))
  })
  
  output$state.plot3 <- renderPlot({
    ggplot(state.data(), aes(x = factor(tract.label, levels = unique(tract.label)),
                                y = (nonwhite.est + lowincome.est))) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = (nonwhite.est + lowincome.est) - (nonwhite.err + lowincome.err),
                        ymax = (nonwhite.est + lowincome.est) + (nonwhite.err + lowincome.err),
                        width = 0.5)) +
      xlab("Tract (Number, County, State)") +
      ylab("Nonwhite + Low-income Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13))
  })
  
  output$philly.tract <- renderImage({
    filename <- normalizePath(file.path(paste0(getwd(),'/phila_tracts.png')), winslash = "/")
    list(src = filename,
         alt = "Philadelphia Tracts")
  }, deleteFile = FALSE)
  

  
  # need to rethink combo rank a little bit
  ## rewrite processing script to be based on sum of both, even though you know
  ## there is a great deal of overlap between non-white and low-income?
  
  output$national.data.out <- renderDataTable(national.data()[, c(2:10)])
  output$national.data.out2 <- renderDataTable(national.data()[, c(2:10)])
  output$national.data.out3 <- renderDataTable(national.data()[, c(2:11)])
  output$state.data.out <- renderDataTable(state.data()[, c(2:10)])
  output$state.data.out2 <- renderDataTable(state.data()[, c(2:10)])
  output$state.data.out3 <- renderDataTable(state.data()[, c(2:11)])
  
  


  })
