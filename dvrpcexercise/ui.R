# UI for DVRPC shiny app

library(shiny)
library(plotly)
library(leaflet)
load("top.tracts")
load("pa.commute.data")

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Census Tract Explorer"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
       selectInput("Level", "Would you like National or State rankings?",
                   list("National" = "nat",
                        "State" = "sta")),
       conditionalPanel(condition = "input.Level == 'nat'",
                        sliderInput("N", "How many tracts would you like to plot?", value = 10,
                                    min = 10, max = 30, step = 5),
                        selectInput("nRank", "Non-white, Low-income, or both?",
                                    list("Non-white" = "nonwhite",
                                         "Low-income" = "lowincome",
                                         "Both" = "combo"))),
       conditionalPanel(condition = "input.Level == 'sta'",
                        selectInput("State", "Which state would you like to see rankings for?",
                                    choices = levels(factor(top.tracts$state)))),
       conditionalPanel(condition = "input.Level == 'sta' &&
                        input.State != ' Pennsylvania'",
                        div("(Choose Pennsylvania to see extra features)"),
                        br(),
                        selectInput("sRank", "Non-white, Low-income, or both?",
                                    list("Non-white" = "nonwhite",
                                         "Low-income" = "lowincome",
                                         "Both" = "combo"))),
       conditionalPanel(condition = "input.Level == 'sta' &&
                        input.State == ' Pennsylvania'",
                        selectInput("pRank", "Non-white, Low-income, both, or commuter data?",
                                    list("Non-white" = "nonwhite",
                                         "Low-income" = "lowincome",
                                         "Both" = "combo",
                                         "Commuting Data" = "commute"))),
       # conditionalPanel(condition = "input.nRank == 'combo' || input.sRank == 'combo' || input.pRank == 'combo'",
       #                  div("Caution: Combo rankings were created by summing low-income and non-white rankings.
       #                      Does not account for overlap between groups!", style = "color:red"),
       #                  br()),
       conditionalPanel(condition = "input.Level == 'sta' &&
                        input.State == ' Pennsylvania' &&
                        input.pRank == 'commute'",
                        selectInput("Tract", "Tract:",
                                    choices = levels(factor(pa.commute$GEO.display.label)))),
       submitButton("Run Selection")

    ),
    
    # Output
    mainPanel(
      # national rankings
      conditionalPanel(condition = "input.Level == 'nat' &&
                         input.nRank == 'nonwhite'",
                       plotlyOutput("national.plot1", height = 600),
                       dataTableOutput("national.data.out")),
      
      conditionalPanel(condition = "input.Level == 'nat' &&
                         input.nRank == 'lowincome'",
                       plotlyOutput("national.plot2", height = 600),
                       dataTableOutput("national.data.out2")),
      
      conditionalPanel(condition = "input.Level == 'nat' &&
                       input.nRank == 'combo'",
                       # add a warning about the summing might be incorrect?
                       plotlyOutput("national.plot3", height = 600),
                       dataTableOutput("national.data.out3")),
      
      # state rankings
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State != ' Pennsylvania' &&
                                    input.sRank == 'nonwhite'",
                       plotlyOutput("state.plot1", height = 600),
                       dataTableOutput("state.data.out")),
      
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State != ' Pennsylvania' &&
                                    input.sRank == 'lowincome'",
                       plotlyOutput("state.plot2", height = 600),
                       dataTableOutput("state.data.out2")),
      
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State != ' Pennsylvania' &&
                                    input.sRank == 'combo'",
                       plotlyOutput("state.plot3", height = 600),
                       dataTableOutput("state.data.out3")),
      
      
      # PA
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State == ' Pennsylvania' &&
                                    input.pRank == 'nonwhite'",
                       plotlyOutput("pa.plot1", height = 600),
                       dataTableOutput("pa.data.out")),
      
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State == ' Pennsylvania' &&
                                    input.pRank == 'lowincome'",
                       plotlyOutput("pa.plot2", height = 600),
                       dataTableOutput("pa.data.out2")),
      
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State == ' Pennsylvania' &&
                                    input.pRank == 'combo'",
                       plotlyOutput("pa.plot3", height = 600),
                       dataTableOutput("pa.data.out3")),
      conditionalPanel(condition = "input.Level == 'sta' &&
                                    input.State == ' Pennsylvania' &&
                                    input.pRank == 'commute'",
                       plotlyOutput("commute.plot", height = 600),
                       dataTableOutput("pa.commute.data")),

      # PA map output
      conditionalPanel(condition = "input.Level == 'sta' &&
                       input.State == ' Pennsylvania'",
                       # imageOutput("philly.tract"), # don't need the picture anymore!
                       leafletOutput("philly.map", height = 750))

    )
  )
))
