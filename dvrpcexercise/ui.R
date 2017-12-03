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
load("pa.commute.data")

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
       conditionalPanel(condition = "input.Level == 'nat'",
                        sliderInput("N", "Number of tracts to plot (10-30):", value = 10,
                                    min = 10, max = 30, step = 5)),
       conditionalPanel(condition = "input.Level == 'sta'",
                        selectInput("State", "State:",
                                    choices = levels(factor(top.tracts$state)))),
       conditionalPanel(condition = "input.State != ' Pennsylvania'",
                        selectInput("Rank", "Non-white, Low-income, or both?",
                                    list("Non-white" = "nonwhite",
                                         "Low-income" = "lowincome",
                                         "Both" = "combo"))),
       conditionalPanel(condition = "input.State == ' Pennsylvania'",
                        selectInput("Rankk", "Non-white, Low-income, both, or commuter data?",
                                    list("Non-white" = "nonwhite",
                                         "Low-income" = "lowincome",
                                         "Both" = "combo",
                                         "Commuting Data" = "commute"))),
       conditionalPanel(condition = "input.Rankk == 'commute'",
                        selectInput("Tract", "Tract:",
                                    choices = levels(factor(pa.commute$GEO.display.label)))),
       submitButton("Run Selection")

    ),
    
    # Output
    mainPanel(
      # national rankings
      conditionalPanel(condition = "input.Level == 'nat' &&
                         input.Rank == 'nonwhite'",
                       p("Bar Plot: Estimate +/- Error"),
                       plotOutput("national.plot1"),
                       dataTableOutput("national.data.out")),
      
      conditionalPanel(condition = "input.Level == 'nat' &&
                         input.Rank == 'lowincome'",
                       p("Bar Plot: Estimate +/- Error"),
                       plotOutput("national.plot2"),
                       dataTableOutput("national.data.out2")),
      
      conditionalPanel(condition = "input.Level == 'nat' &&
                       input.Rank == 'combo'",
                       p("Bar Plot: Estimate +/- Error"),
                       # add a warning about the summing might be incorrect?
                       plotOutput("national.plot3"),
                       dataTableOutput("national.data.out3")),
      
      # state rankings
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State != ' Pennsylvania' &&
                                    input.Rank == 'nonwhite'",
                       p("Bar Plot: Estimate +/- Error"),
                       plotOutput("state.plot1"),
                       dataTableOutput("state.data.out")),
      
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State != ' Pennsylvania' &&
                         input.Rank == 'lowincome'",
                       p("Bar Plot: Estimate +/- Error"),
                       plotOutput("state.plot2"),
                       dataTableOutput("state.data.out2")),
      
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State != ' Pennsylvania' &&
                       input.Rank == 'combo'",
                       p("Bar Plot: Estimate +/- Error"),
                       # add a warning about the summing might be incorrect?
                       plotOutput("state.plot3"),
                       dataTableOutput("state.data.out3")),
      
      
      # PA
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State == ' Pennsylvania' &&
                         input.Rankk == 'nonwhite'",
                       p("Bar Plot: Estimate +/- Error"),
                       plotOutput("pa.plot1"),
                       dataTableOutput("pa.data.out")),
      
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State == ' Pennsylvania' &&
                         input.Rankk == 'lowincome'",
                       p("Bar Plot: Estimate +/- Error"),
                       plotOutput("pa.plot2"),
                       dataTableOutput("pa.data.out2")),
      
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State == ' Pennsylvania' &&
                       input.Rankk == 'combo'",
                       p("Bar Plot: Estimate +/- Error"),
                       # add a warning about the summing might be incorrect?
                       plotOutput("pa.plot3"),
                       dataTableOutput("pa.data.out3")),
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State == ' Pennsylvania' &&
                       input.Rankk == 'commute'",
                       plotOutput("commute.plot")),
      # PA output
      conditionalPanel(condition = "input.Level == 'sta' &&
                       input.State == ' Pennsylvania'",
                       imageOutput("philly.tract"))

    )
  )
))
