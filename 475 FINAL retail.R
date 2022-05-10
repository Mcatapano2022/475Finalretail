library(shiny)

library(fpp3)
library(shinyWidgets)
library(dplyr)
library(tidyverse)
library(tsibble)
library(readr)
library(shinydashboard)
library(plotly)


unique(aus_retail$Industry)
data(aus_retail)

library(shiny)

ui <- fluidPage(
  setBackgroundColor(
    color = c("white", "orange"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel("Australian States and their retail industries"),
  p("Welcome! The following application displays data regarding Australian states and their retail industries.
    To use the app, begin by selecting an industry and state from the drop-down box below. After making your selections you can filter betweeen Seasonality, 
    Autocorrelation, and Decomposition. The graphs will automatically update as you change your selections. Feel free to mix and match a variety of 
    states and industries!" ),
  plotOutput("hist"),
  plotOutput("Hist2"),
  
  radioGroupButtons(
    inputId = "Choice",
    label = "Options",
    choices = c("Seasonality", 
                "Autocorrelation", "Decomposition"),
    direction = "vertical"
  ),
  
  
  pickerInput(
    inputId = "Industry",
    label = "Select an Industry", 
    choices = c( "Cafes, restaurants and catering services" ,                        
                 "Cafes, restaurants and takeaway food services" ,                   
                 "Clothing retailing" ,                                              
                 "Clothing, footwear and personal accessory retailing" ,             
                 "Department stores" ,                                               
                 "Electrical and electronic goods retailing" ,                       
                 "Food retailing"  ,                                                 
                 "Footwear and other personal accessory retailing" ,                 
                 "Furniture, floor coverings, houseware and textile goods retailing",
                 "Hardware, building and garden supplies retailing" ,                
                 "Household goods retailing" ,                                       
                 "Liquor retailing" ,                                                
                 "Newspaper and book retailing" ,                                    
                 "Other recreational goods retailing" ,                              
                 "Other retailing" ,                                                 
                 "Other retailing n.e.c." ,                                          
                 "Other specialised food retailing" ,                                
                 "Pharmaceutical, cosmetic and toiletry goods retailing" ,            
                 "Supermarket and grocery stores" ,                                  
                 "Takeaway food services"                                           
    ),
    selected = "Takeaway food services",
    options = list(
      title = "Choose an Industry")
  ),
  pickerInput(
    inputId = "State",
    label = "Pick a State", 
    choices = c("Australian Capital Territory", "New South Wales"    ,         
                "Northern Territory"       ,    "Queensland"   ,               
                "South Australia"           ,   "Tasmania"   ,                 
                "Victoria"    ,                 "Western Australia"),
    selected = "South Australia",
    options = list(
      title = "Choose a State")
  ),
  
  p(
    "Decomposition: Our classical decompositions largest component is trend while random variation is the one with the most change. The seasonality component has a conistant, 
    repeating pattern with little variation thrroughout. This proves true regardless of industry and state selection. 
    
    Autocorrellation: The autocorrelation function provides an interesting indicator for some of our industries. Theree is not white noise in the graphs and our function 
    values are outside the control limit. So this means that for some industries, trends they see during this specific week would b good indicators of what
    could potentially happen in the near future.
    
    Seasonality: There appears to be some seasonality for specific industries around the warmer months. For example, food related industries see a spike during 
    these warmer months. This is probably due to the fact that more people are out and active when the weather is more comfortable."),
  
  radioGroupButtons(
    inputId = "Choice1",
    label = "Options",
    choices = c("Holt", 
                "Holt/Winter"),
    direction = "vertical"
  ),
  pickerInput(
    inputId = "State1",
    label = "Pick a State", 
    choices = c("Australian Capital Territory", "New South Wales"    ,         
                "Northern Territory"       ,    "Queensland"   ,               
                "South Australia"           ,   "Tasmania"   ,                 
                "Victoria"    ,                 "Western Australia"),
    selected = "South Australia",
    options = list(
      title = "Choose a State")
  ),
  pickerInput(
    inputId = "Industry1",
    label = "Select an Industry", 
    choices = c( "Cafes, restaurants and catering services" ,                        
                 "Cafes, restaurants and takeaway food services" ,                   
                 "Clothing retailing" ,                                              
                 "Clothing, footwear and personal accessory retailing" ,             
                 "Department stores" ,                                               
                 "Electrical and electronic goods retailing" ,                       
                 "Food retailing"  ,                                                 
                 "Footwear and other personal accessory retailing" ,                 
                 "Furniture, floor coverings, houseware and textile goods retailing",
                 "Hardware, building and garden supplies retailing" ,                
                 "Household goods retailing" ,                                       
                 "Liquor retailing" ,                                                
                 "Newspaper and book retailing" ,                                    
                 "Other recreational goods retailing" ,                              
                 "Other retailing" ,                                                 
                 "Other retailing n.e.c." ,                                          
                 "Other specialised food retailing" ,                                
                 "Pharmaceutical, cosmetic and toiletry goods retailing" ,            
                 "Supermarket and grocery stores" ,                                  
                 "Takeaway food services"                                           
    ),
  
),

plotOutput("Hist3"), 

 


radioGroupButtons(
  inputId = "Choice2",
  label = "Options",
  choices = c("Naive", 
              "Seasonal naive", 
              "Mean",
              "Drift"),
  direction = "vertical"
),
pickerInput(
  inputId = "State2",
  label = "Pick a State", 
  choices = c("Australian Capital Territory", "New South Wales"    ,         
              "Northern Territory"       ,    "Queensland"   ,               
              "South Australia"           ,   "Tasmania"   ,                 
              "Victoria"    ,                 "Western Australia"),
  selected = "South Australia",
  options = list(
    title = "Choose a State")
),
pickerInput(
  inputId = "Industry2",
  label = "Select an Industry", 
  choices = c( "Cafes, restaurants and catering services" ,                        
               "Cafes, restaurants and takeaway food services" ,                   
               "Clothing retailing" ,                                              
               "Clothing, footwear and personal accessory retailing" ,             
               "Department stores" ,                                               
               "Electrical and electronic goods retailing" ,                       
               "Food retailing"  ,                                                 
               "Footwear and other personal accessory retailing" ,                 
               "Furniture, floor coverings, houseware and textile goods retailing",
               "Hardware, building and garden supplies retailing" ,                
               "Household goods retailing" ,                                       
               "Liquor retailing" ,                                                
               "Newspaper and book retailing" ,                                    
               "Other recreational goods retailing" ,                              
               "Other retailing" ,                                                 
               "Other retailing n.e.c." ,                                          
               "Other specialised food retailing" ,                                
               "Pharmaceutical, cosmetic and toiletry goods retailing" ,            
               "Supermarket and grocery stores" ,                                  
               "Takeaway food services"                                           
  )
  
),
plotOutput("Hist5"),

radioGroupButtons(
  inputId = "Choice3",
  label = "Options",
  choices = c("Manual Selected parameters", 
              "Auto Selected parameters"),
  direction = "vertical"
),


pickerInput(
  inputId = "Industry3",
  label = "Select an Industry", 
  choices = c( "Cafes, restaurants and catering services" ,                        
               "Cafes, restaurants and takeaway food services" ,                   
               "Clothing retailing" ,                                              
               "Clothing, footwear and personal accessory retailing" ,             
               "Department stores" ,                                               
               "Electrical and electronic goods retailing" ,                       
               "Food retailing"  ,                                                 
               "Footwear and other personal accessory retailing" ,                 
               "Furniture, floor coverings, houseware and textile goods retailing",
               "Hardware, building and garden supplies retailing" ,                
               "Household goods retailing" ,                                       
               "Liquor retailing" ,                                                
               "Newspaper and book retailing" ,                                    
               "Other recreational goods retailing" ,                              
               "Other retailing" ,                                                 
               "Other retailing n.e.c." ,                                          
               "Other specialised food retailing" ,                                
               "Pharmaceutical, cosmetic and toiletry goods retailing" ,            
               "Supermarket and grocery stores" ,                                  
               "Takeaway food services"                                           
  ),
  selected = "Takeaway food services",
  options = list(
    title = "Choose an Industry")
),
pickerInput(
  inputId = "State3",
  label = "Pick a State", 
  choices = c("Australian Capital Territory", "New South Wales"    ,         
              "Northern Territory"       ,    "Queensland"   ,               
              "South Australia"           ,   "Tasmania"   ,                 
              "Victoria"    ,                 "Western Australia"),
  selected = "South Australia",
  options = list(
    title = "Choose a State")
),

plotOutput("Hist6")

)

server <- function(input, output, session) {
  output$hist <- renderPlot(
    aus_retail %>% 
      filter(Industry %in% input$Industry,
             State %in% input$State) %>%
      autoplot() 
  )

output$Hist2 <- renderPlot({
  if(input$Choice == "Seasonality"){
    aus_retail %>%
      filter(Industry %in% input$Industry,
             State %in% input$State) %>%
      gg_subseries()
  } else
    if (input$Choice == "Autocorrelation"){
      aus_retail %>%
        filter(Industry %in% input$Industry,
               State %in% input$State) %>%
        ACF(Turnover, lag_max = 4) %>%
        autoplot()
    }else
      if(input$Choice == "Decomposition"){
        aus_retail %>%
          filter(Industry %in% input$Industry,
                 State %in% input$State) %>%
          model(
            classical_decomposition(Turnover, type = "additive")
          ) %>%
          components()%>%
          autoplot()}
  
})

output$Hist3 <- renderPlot({
  

  if(input$Choice1 == "Holt/Winter"){
    aus_ret <- aus_retail %>%
      filter(Industry %in% input$Industry1,
             State %in% input$State1) %>%
      summarise(Turnover = sum(Turnover))
    fit <- aus_ret %>%
      model(
        additive = ETS(Turnover ~ error("A") + trend("A") +
                         season("A")),
        multiplicative = ETS(Turnover ~ error("M") + trend("A") +
                               season("M"))
      )
    fc <- fit %>% forecast(h = "3 years")
    fc %>%
      autoplot(aus_ret, level = NULL) +
      labs(title="Australian Retail",
           y="turnover") +
      guides(colour = guide_legend(title = "Forecast"))
  }else 
    if (input$Choice1 == "Holt"){
      aus_ret <- aus_retail %>%
        filter(Industry %in% input$Industry1,
               State %in% input$State1) %>%
        summarise(Turnover = sum(Turnover))
      autoplot(aus_ret, Turnover) +
        labs(y = "Turnover", title = "Australian Turnover")
      
}
  })

output$Hist5 <- renderPlot({
  if (input$Choice2 == "Naive"){
    train <- aus_retail %>%
      filter(Industry %in% input$Industry2,
             State %in% input$State2)
    Naive_fit <- train %>%
      model('Naive' = NAIVE(Turnover))
    Naive_fc <- Naive_fit %>% forecast(h = 12)
    Naive_fc %>%
      autoplot(train, level = NULL)
  }else 
    if (input$Choice2 == "Seasonal naive"){
    train <- aus_retail %>%
      filter(Industry %in% input$Industry2,
             State %in% input$State2)
    SNaive_fit <- train %>%
      model(SNAIVE(Turnover ~ lag("year")))
    SNaive_fc <- SNaive_fit %>% forecast(h = 12)
    SNaive_fc %>%
      autoplot(train, level = NULL)
    }else
      if (input$Choice2 == "Mean"){
        train <- aus_retail %>%
          filter(Industry %in% input$Industry2,
                 State %in% input$State2)
        mean_fit <- train %>%
          model(MEAN(Turnover))
        mean_fc <- mean_fit %>% forecast(h = 12)
        mean_fc %>%
          autoplot(train, level = NULL)
      }else
        if (input$Choice2 == "Drift"){
          train <- aus_retail %>%
            filter(Industry %in% input$Industry2,
                   State %in% input$State2)
          drift_fit <- train %>%
            model(RW(Turnover ~ drift()))
          drift_fc <- drift_fit %>% forecast(h = 12)
          drift_fc %>%
            autoplot(train, level = NULL)
    
        }
}
)

output$Hist6 <- renderPlot({
  if(input$Choice3 == "Manual Selected parameters"){
    train_2 <- aus_retail %>%
      filter(Industry %in% input$Industry3,
             State %in% input$State3)
    Arima_fit <- train_2 %>%
      model(arima210 = ARIMA(Turnover ~ pdq(2,1,0)),
            arima013 = ARIMA(Turnover ~ pdq(0,1,3)),
            stepwise = ARIMA(Turnover),
            search = ARIMA(Turnover, stepwise=FALSE))
    Arimafc <- Arima_fit %>% forecast(h = 12)
    Arimafc %>%
      autoplot(train_2, level = NULL)
  }else
    if(input$Choice3 == "Auto Selected parameters"){
      train_2 <- aus_retail %>%
        filter(Industry %in% input$Industry3,
               State %in% input$State3)
      Arima_fit <- train_2 %>%
        model(ARIMA(Turnover))
      Arimafc <- Arima_fit %>% forecast(h=12)
      Arimafc %>%
        autoplot(train_2, level = NULL)
    }
})

}


shinyApp(ui, server)


