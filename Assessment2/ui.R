install.packages("shiny")
install.packages("shinydashboard")

library(shiny)
library(shinydashboard)

shinyServer(
  pageWithSidebar(
    headerPanel("My First App 20BDS0138"),
    sidebarPanel(
      selectInput("Distribution",'Pls.Select Distribution type',
                  choices =c('Normal','Exponential')),
      sliderInput("sampleSize",
                  'Pls.Select Sample Size',
                  min =100,max=5000,
                  value=1000,step=100),
      conditionalPanel(condition="input.Distribution=='Normal'",
                       textInput("mean","Pls.Select mean:",10),
                       textInput("sd","Pls.Select SD:",3)),
      conditionalPanel(condition="input.Distribution=='Exponential'",
                       textInput("lambda","Pls.Select Exp lamda:",1))
    ),
    mainPanel(plotOutput('myPlot'))
  )
)
