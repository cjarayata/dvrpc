# server for DVRPC shiny app

library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(rgdal)
load("top.tracts") # churned tract data
load("pa.commute.data") # trimmed commuter data (cast and melted) for top PA tracts
load("pa.topshape.data") # trimmed geospatial dataframe for top PA tracts

# Define server logic
shinyServer(function(input, output) {
  
  national.data <- reactive({
    top.tracts <- top.tracts[, c(1, 3:5, 8:11, 15:17)]
    if(input$nRank == 'nonwhite'){
      blah <- top.tracts[top.tracts$nw.nat.rank <= input$N, ]
      blah <- blah[order(blah$nw.nat.rank), ]
      return(blah)
    }
    if(input$nRank == 'lowincome'){
      blah <- top.tracts[top.tracts$pov.nat.rank <= input$N, ]
      blah <- blah[order(blah$pov.nat.rank), ]
      return(blah)
    }
    
    # might have to redo combo rankings.... is rank-summing the legit move here?
    if(input$nRank == 'combo'){
      blah <- top.tracts[order(top.tracts$combo.nat.rank), ]
      blah <- blah[1:input$N, ]
      return(blah)
    }
  })
  
  state.data <- reactive({
    top.tracts <- top.tracts[, c(1, 3:5, 8:14)]
      blah <- top.tracts[top.tracts$state == input$State, ]
      if(input$sRank == 'nonwhite'){
        blah <- blah[blah$nonwhite.rank <= 10, ]
        blah <- blah[order(blah$nonwhite.rank), ]
        return(blah)
      }
      if(input$sRank == 'lowincome'){
        blah <- blah[blah$poverty.rank <= 10, ]
        blah <- blah[order(blah$poverty.rank), ]
        return(blah)
      }
      if(input$sRank == 'combo'){
        blah <- blah[order(blah$combo.rank), ]
        blah <- blah[1:10, ]
        return(blah)
      }
  })
  
  pa.data <- reactive({
    top.tracts <- top.tracts[, c(1, 3:5, 8:14)]
    blah <- top.tracts[top.tracts$state == input$State, ]
    if(input$pRank == 'nonwhite'){
      blah <- blah[blah$nonwhite.rank <= 10, ]
      blah <- blah[order(blah$nonwhite.rank), ]
      return(blah)
    }
    if(input$pRank == 'lowincome'){
      blah <- blah[blah$poverty.rank <= 10, ]
      blah <- blah[order(blah$poverty.rank), ]
      return(blah)
    }
    if(input$pRank == 'combo'){
      blah <- blah[order(blah$combo.rank), ]
      blah <- blah[1:10, ]
      return(blah)
    }
  })
  
  commute.data <- reactive({
    blah <- pa.commute.melt[pa.commute.melt$GEO.display.label == input$Tract, ]
    return(blah)
  })
  
  output$national.plot1 <- renderPlotly({
    p <- ggplotly(
    ggplot(national.data(), aes(x = factor(tract.label, levels = unique(tract.label)), y = nonwhite.est)) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = nonwhite.est - nonwhite.err,
                        ymax = nonwhite.est + nonwhite.err,
                        width = 0.5)) +
      labs(title = "Tracts with highest non-white population\n
           Bars represent -/+ error",
           x = "Tract (Number, County, State)",
           y = "Non-white Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    )
    p$elementId <- NULL
    p
  })
  
  output$national.plot2 <- renderPlotly({
    p <- ggplotly(
    ggplot(national.data(), aes(x = factor(tract.label, levels = unique(tract.label)), y = lowincome.est)) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = lowincome.est - lowincome.err,
                        ymax = lowincome.est + lowincome.err,
                        width = 0.5)) +
      labs(title = "Tracts with highest low-income population\n
           Bars represent -/+ error",
           x = "Tract (Number, County, State)",
           y = "Low-income Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    )
    p$elementId <- NULL
    p
  })
  
  output$national.plot3 <- renderPlotly({
    p <- ggplotly(
    ggplot(national.data(), aes(x = factor(tract.label, levels = unique(tract.label)),
                                y = (nonwhite.est + lowincome.est))) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = (nonwhite.est + lowincome.est) - (nonwhite.err + lowincome.err),
                        ymax = (nonwhite.est + lowincome.est) + (nonwhite.err + lowincome.err),
                        width = 0.5)) +
      labs(title = "Tracts with highest non-white and low-income population\n
           Bars represent -/+ error",
           x = "Tract (Number, County, State)",
           y = "Nonwhite + Low-income Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    )
    p$elementId <- NULL
    p
  })
  
  output$state.plot1 <- renderPlotly({
    p <- ggplotly(
    ggplot(state.data(), aes(x = factor(tract.label, levels = unique(tract.label)), y = nonwhite.est)) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = nonwhite.est - nonwhite.err,
                        ymax = nonwhite.est + nonwhite.err,
                        width = 0.5)) +
      labs(title = "Tracts with highest non-white population\n
           Bars represent -/+ error",
           x = "Tract (Number, County, State)",
           y = "Non-white Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    )
    p$elementId <- NULL
    p
  })
  
  output$state.plot2 <- renderPlotly({
    p <- ggplotly(
    ggplot(state.data(), aes(x = factor(tract.label, levels = unique(tract.label)), y = lowincome.est)) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = lowincome.est - lowincome.err,
                        ymax = lowincome.est + lowincome.err,
                        width = 0.5)) +
      labs(title = "Tracts with highest low-income population\n
           Bars represent -/+ error",
           x = "Tract (Number, County, State)",
           y = "Low-income Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    )
    p$elementId <- NULL
    p
  })
  
  output$state.plot3 <- renderPlotly({
    p <- ggplotly(
    ggplot(state.data(), aes(x = factor(tract.label, levels = unique(tract.label)),
                                y = (nonwhite.est + lowincome.est))) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = (nonwhite.est + lowincome.est) - (nonwhite.err + lowincome.err),
                        ymax = (nonwhite.est + lowincome.est) + (nonwhite.err + lowincome.err),
                        width = 0.5)) +
      labs(title = "Tracts with highest non-white and low-income population\n
           Bars represent -/+ error",
           x = "Tract (Number, County, State)",
           y = "Nonwhite + Low-income Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    )
    p$elementId <- NULL
    p
  })
  
  output$pa.plot1 <- renderPlotly({
    p <- ggplotly(
    ggplot(pa.data(), aes(x = factor(tract.label, levels = unique(tract.label)), y = nonwhite.est)) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = nonwhite.est - nonwhite.err,
                        ymax = nonwhite.est + nonwhite.err,
                        width = 0.5)) +
      labs(title = "Tracts with highest non-white population\n
           Bars represent -/+ error",
           x = "Tract (Number, County, State)",
           y = "Non-white Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    )
    p$elementId <- NULL
    p
  })
  
  output$pa.plot2 <- renderPlotly({
    p <- ggplotly(
    ggplot(pa.data(), aes(x = factor(tract.label, levels = unique(tract.label)), y = lowincome.est)) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = lowincome.est - lowincome.err,
                        ymax = lowincome.est + lowincome.err,
                        width = 0.5)) +
      labs(title = "Tracts with highest low-income population\n
           Bars represent -/+ error",
           x = "Tract (Number, County, State)",
           y = "Low-income Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    )
    p$elementId <- NULL
    p
  })
  
  output$pa.plot3 <- renderPlotly({
    p <- ggplotly(
    ggplot(state.data(), aes(x = factor(tract.label, levels = unique(tract.label)),
                             y = (nonwhite.est + lowincome.est))) +
      geom_bar(position="dodge", stat="identity", fill = "#FF6666") +
      geom_errorbar(aes(ymin = (nonwhite.est + lowincome.est) - (nonwhite.err + lowincome.err),
                        ymax = (nonwhite.est + lowincome.est) + (nonwhite.err + lowincome.err),
                        width = 0.5)) +
      labs(title = "Tracts with highest non-white and low-income population\n
           Bars represent -/+ error",
           x = "Tract (Number, County, State)",
           y = "Nonwhite + Low-income Estimate") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    )
    p$elementId <- NULL
    p
  })
  
  output$commute.plot <- renderPlotly({
    p <- ggplotly(
    ggplot(commute.data(), aes(x = factor(type, levels = unique(type)), y = value,
                     fill = factor(group, levels = unique(group)))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = value), position = position_dodge(0.9)) +
      labs(title = paste0(commute.data()$GEO.display.label[1],
                          "\nTotal Workers: ", commute.data()$total.workers[1],
                          " [Male: ", commute.data()$male.workers[1],
                          ", Female: ", commute.data()$female.workers[1], "]",
                          "\nMean Travel Time: ", commute.data()$total.meantraveltime,
                          " minutes")) +
      xlab("Commute Type") +
      ylab("Percent Mode Share") +
      theme(legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
    )
    p$elementId <- NULL
    p
  })
  # 
  # output$philly.tract <- renderImage({
  #   filename <- normalizePath(file.path(paste0(getwd(),'/phila_tracts.png')), winslash = "/")
  #   list(src = filename,
  #        alt = "Philadelphia Tracts")
  # }, deleteFile = FALSE)
  
  output$philly.map <- renderLeaflet({
    leaflet() %>%
      setView(lat = 39.9526, lng = -75.1652, zoom = 11) %>% 
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addPolygons(data = top.pa.shape,
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                  label = paste0("Tract #", top.pa.shape@data$NAME))
  })
  

  
  # need to rethink combo rank a little bit
  ## rewrite processing script to be based on sum of both, even though you know
  ## there is a great deal of overlap between non-white and low-income?
  
  output$national.data.out <- renderDataTable(national.data()[, c(2:10)])
  output$national.data.out2 <- renderDataTable(national.data()[, c(2:10)])
  output$national.data.out3 <- renderDataTable(national.data()[, c(2:11)])
  output$state.data.out <- renderDataTable(state.data()[, c(2:10)])
  output$state.data.out2 <- renderDataTable(state.data()[, c(2:10)])
  output$state.data.out3 <- renderDataTable(state.data()[, c(2:11)])
  output$pa.data.out <- renderDataTable(pa.data()[, c(2:10)])
  output$pa.data.out2 <- renderDataTable(pa.data()[, c(2:10)])
  output$pa.data.out3 <- renderDataTable(pa.data()[, c(2:11)])
  output$pa.commute.data <- renderDataTable(pa.commute)
  


  })
