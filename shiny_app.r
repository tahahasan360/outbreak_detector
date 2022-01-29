if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

## load rtweet
library(rtweet)

## store api keys (these are fake example values; replace with your own keys)
api_key <- "vNjxGpVQ8HmCCQs2OLhic6mlA"
api_secret_key <- "FtGOLCajn2mH8a7IR0lBUX9ek0papFws0N8R1u01yYLYBVGTHp"
access_token <- "1487230370395398144-2Vm4SJ7YztCMnIkSu8U4NXqbOaZSrC"
access_token_secret <- "h6rD1qYCyyhxkjJtSplMDe67xNRP7UqHJp6paDercQPke"

## authenticate via web browser
token <- create_token(
  app = "Raahim",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

token
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)

rstats_tweets <- search_tweets(q = "#rstats",
                               n = 50)
# view the first 3 rows of the dataframe
head(rstats_tweets, n = 3)

ui <- fluidPage(
  titlePanel("Disease Breakout"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose your city to see if there is a potential threat for disease outbreak in it."),
      
      selectInput("var", 
                  label = "City",
                  choices = sort(unique(us_cities))),
    ),
    
    mainPanel(
      verbatimTextOutput("selected_var") 
    )
  )
)

server <- function(input, output) {
  
  output$selected_var <- renderText({ 

    city <- input$var

    for i in (1: length(tuples)){
        tweets <- search_tweets(q = tuples, geocode = city, , n = 50000)
        if (tweets / pop(city) > 2 * sd(city) + base(city)){
            disease <- tuple_to_disease(tuple)
            return ("Potential outbreak detected.\n" + "Symptoms:" + tuple_to_string(tuple) + "Disease:" + tuple_to_disease(tuple) + 
            "Precautions:" + precautions(disease))
        } 
        else {
            return ("There is no outbreak detected in your area.")
        }
    }

  }
  )
}

shinyApp(ui = ui, server = server)