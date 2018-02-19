library(shiny)
library(shinythemes)
shinyUI(fluidPage(theme=shinytheme("united"),shinythemes::themeSelector(),
                  titlePanel(tags$h1(tags$i(tags$b(("Demand Forecasting BI System"))))),
                  
                  sidebarPanel( 
                    conditionalPanel(
                      tags$head(tags$style(type="text/css", "
                                           #loadmessage {
                                           position: relative;
                                           top: 0px;
                                           left: 0px;
                                           width: 100%;
                                           padding: 5px 0px 5px 0px;
                                           text-align: center;
                                           font-weight: bold;
                                           font-size: 100%;
                                           color: #000000;
                                           border:1px solid black;
                                           background-color: #green;
                                           z-index: 105;
                                           }")),
                      selectInput("City", "select city", choices = c('city1'='a1', 'city2'='b1')),
                      selectInput("Territory", "select Territory", 
                                  choices = c('Asia Pacific and Japan'='JAPAC', 'North America'='T2')),
                      selectInput("Productcode", "select Productcode", choices=c('Productcode1'='e1', 'Productcode=f1')),
                       
                     # textOutput("quantity"),
                       uiOutput("companyName1"),
                     #uiOutput("companyName1"),
                    #  uiOutput("storesNames1"),
    uiOutput("bayName1"),
    tags$div(id="buttonDiv",tags$style("#buttonDiv{text-align:center;margin-left:3%;}"),actionButton("action","Predict",class = "btn-primary")),
    
    p(id="helpText",
      tags$style("#helpText{text-align:center;font-size:10px'}"),"Click Start button once to know the prediction") 
                      )), 
    mainPanel(  )
))