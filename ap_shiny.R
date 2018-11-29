# Load packages
# install.packages("shinythemes")
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
require(ggplot2)

# Load data
head(vb_raw)

# Define UI
# ui <- fluidPage(
#   theme = shinytheme("lumen"),
#   titlePanel("SB PRODUCTION"),
#   
#   
#   sidebarLayout(
#     sidebarPanel(
#       # Select type of trend to plot
#       selectInput(
#         inputId = "prod_cd",
#         label = strong("Trend index"),
#         choices = unique(vb_raw$prod_cd),
#         selected = "110000072"
#       )
#       ,
#       
#       #   # Select date range to be plotted
#       dateRangeInput(
#         "date",
#         strong("Date range"),
#         start = "2007-01-01",
#         end = "2017-07-31",
#         min = "2007-01-01",
#         max = "2017-07-31"
#       ),
#     )
#     ,
#     
#     # Output: Description, lineplot, and reference
#     mainPanel(plotOutput(outputId = "lineplot"))
#   )
# )

ui2 <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("SB PRODUCTION"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "prod_cd1", 
                                label = strong("Trend index"),
                                choices = unique(vb_raw$prod_cd),
                                selected = "110000072"),
                    selectInput(inputId = "prod_cd2", 
                                label = strong("Trend index"),
                                choices = unique(vb_raw$prod_cd),
                                selected = "110000072"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "2017-01-01", end = "2017-02-15",
                                   min = "2017-01-01", max = "2017-02-15")
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px"),
                    #plotOutput(outputId = "xgplot1"),
                    plotOutput(outputId = "xgplot"),
                    tableOutput(outputId = "xgresult1"),
                    verbatimTextOutput(outputId = "xgtable3")
                    
                    
                  )
                )
)




# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends1 <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    vb_raw %>% dplyr::filter(
        prod_cd == input$prod_cd1,
        date > as.Date(input$date[1]) & date < as.Date(input$date[2]
        ))
  })
  
  selected_trends2 <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    vb_raw %>% dplyr::filter(
      prod_cd == input$prod_cd2,
      date > as.Date(input$date[1]) & date < as.Date(input$date[2]
      ))
  })
  
  selected_trends3 <- reactive({
    selected_trends1() %>% group_by(prod_cd, date) %>% summarise(cnt = sum(order_vol))
  })
  
   
    #ts.tmp <- new.env()
    #ts.tmp$cnt <- ts(selected_trends3$cnt);
    #ts.tmp$cnt_cl <- tsClean(tt$cnt) %>% ts;
    #xgbar(ts.tmp$cnt,nrounds_method = 'cv') %>% as.data.frame(forecast(xg.md1, h = 10))
    #xg_result <- as.data.frame(forecast(xg.md1, h = 10))

  # xg_bar1 <- reactive({
  #   xgbar(selected_trends3$cnt %>% ts(),
  #         nrounds_method = 'cv')
  # 
  # })
  
# tt <- xgbar(selected_trends3()$cnt %>% ts(),
#            nrounds_method = 'cv')
  
  

  output$xgtable3 <- renderPrint({
    
    tt <- xgbar(selected_trends3()$cnt %>% ts(),
            nrounds_method = 'cv')
    summary(tt)
    # summary(xgbar(selected_trends3()$cnt %>% ts(),
    #                nrounds_method = 'cv'))

  })
  
  
  output$xgresult1 <- renderTable({
    
    asdf1 <- c("week1", "week2", 'week3', 'week4', 'week5', 'week6')
    asdf2 <- forecast(xgbar(selected_trends3()$cnt %>% ts(),
                   nrounds_method = 'cv'), h=6)[[2]]
    data.frame(week = asdf1, forecast = asdf2)
    
    
  })
  
  output$xgplot <- renderPlot({
    
    plot(forecast(xgbar(selected_trends3()$cnt %>% ts(),
                        nrounds_method = 'cv'), h=6))
    
  })

  
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    #color = "#434343"
    #par(mar = c(4, 4, 1, 1))
         #xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
  # ggplot(filter(vb_raw, prod_cd==input$prod_cd), aes(x=date, y=order_vol)) + geom_line()
    ggplot(rbind(selected_trends1(),selected_trends2()) ,
           aes(x = date , y = order_vol,
           colour = prod_cd)) +
      geom_smooth(aes(alpha = 0.5)) + geom_point()
    
    })
  
}

# Create Shiny object
shinyApp(ui = ui2, server = server)








# xgboost

# install.packages("xgboost")
# require(xgboost)
# install.packages("forecastxgb")
# install.packages("tsClean")
# require(forecastxgb)
# 
# ts.tmp <- new.env();
# ts.tmp$cnt <- ts(df.tmp1$cnt);
# ts.tmp$cnt_cl <- tsclean(df.tmp1$cnt)  %>% ts;
# 
# 
# 
# ts.tmp <- new.env()
# ts.tmp$cnt <- ts(tt$cnt);
# #ts.tmp$cnt_cl <- tsClean(tt$cnt) %>% ts;
# 
# tt <- vb_raw %>% filter(prod_cd == "110000072") %>% group_by(date) %>% summarise(cnt = sum(order_vol))
# 
# xg.md1 <- xgbar(ts.tmp$cnt,
#                 nrounds_method = 'cv')
# 
# xg_result <- forecast(xg.md1, h = 10)
# xg_result[[2]]
# 
# str(xg_result)
# str(vb_raw %>% head(1))
# 
# plot(xg_result)


