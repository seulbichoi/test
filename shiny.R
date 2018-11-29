#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("shiny")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("DT")
library(shiny)
require(plyr)
require(dplyr)
require(ggplot2)
require(DT)

input <- vb_raw
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Dates and date ranges"),
  
    column(3, wellPanel(
      h4("These inputs control the other inputs on the page"),
      textInput("prod_cd",
                label = "This controls some of the labels:",
                value = "asdf"),
      
      dateRangeInput('dateRange',
                     label = 'Date range input: yyyy-mm-dd')
                     #start =Sys.Date() - 2, end = Sys.Date() + 2)
    )),
  
  mainPanel(
    plotOutput('vbplot')
  )
  )

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  updateTextInput(session, "Prod_cd", 
                  value = "cd"),
  updateDateRangeInput(session, "DateRange",
                       start ="2014-01-01" ,
                       end = "2018-02-28"
  )
  
  
  
  output$vbplot <- renderPlot({
    
    ggplot(filter(input, prod_cd==prod_cd), aes(x=input$date, y=input$order_vol)) + geom_line()
    #barplot(filter(nhis1, SIDO==input$SIDO), main=input$SIDO)
  })
  
  #ggplot(filter(vb_raw, prod_cd=="110000072"), aes(x=date, y=order_vol)) + geom_line()
  
}


# Run the application
shinyApp(ui = ui, server = server)


