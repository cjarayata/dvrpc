#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
load("top.tracts")

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Census Tract Rankings"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
       selectInput("Level", "National or State rankings?",
                   list("National" = "nat",
                        "State" = "sta")),
       selectInput("Rank", "Non-white, Low-income, or both?",
                   list("Non-white" = "nonwhite",
                        "Low-income" = "lowincome",
                        "Both" = "combo")),
       conditionalPanel(condition = "input.Level == 'nat'",
                        sliderInput("N", "Number to plot (10-30):", value = 10,
                                    min = 10, max = 30, step = 5)),
       conditionalPanel(condition = "input.Level == 'sta'",
                        selectInput("State", "State:",
                                    choices = levels(factor(top.tracts$state)))),
       submitButton("Run Selection"),
       
       # input/output
       verbatimTextOutput("Level"),
       verbatimTextOutput("Rank"),
       verbatimTextOutput("N")
    ),
    
    # Output
    mainPanel(
      p("Bar Plot: Estimate +/- Error"),
      conditionalPanel(condition = "input.Level == 'nat' &&
                         input.Rank == 'nonwhite'",
                       plotOutput("national.plot1")),
      conditionalPanel(condition = "input.Level == 'nat' &&
                         input.Rank == 'lowincome'",
                       plotOutput("national.plot2"))
      #                  dataTableOutput("national.table")),
      # conditionalPanel(condition = "input.Level == 'sta'",
      #                  plotOutput("state.plot")),
    )
  )
))
