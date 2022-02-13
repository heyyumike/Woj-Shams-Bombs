library(shiny)
library(plotly)
library(tidyverse)
library(rtweet)
library(shinycssloaders)
library(lubridate)
library(scales)

appname <- "Woj/Sham Bomb Shiny App"
key <- "pildxkOlSBi1W4e3Sf7d0Df1P"
secret <- "10A2kWMoY1RZuPqcisWaTqWFcQPgSmpZUo0giPRjxEHWLyo2ED"
access_token <- "939911634297896960-XRDn4SKq3MM1ugaPohpwxftlmMFJ9hF"
access_secret <- "DKw8MIwVip0DLUzbpGPPebVwUzt88uDRSfWah3w63OAjq"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)

ui <- fluidPage(
    titlePanel("Woj/Shams Bombs"),
    
    sidebarLayout(
      sidebarPanel(width = 2,
                   selectInput(inputId = "insider",
                               label = "Please select a NBA Insider",
                               choices = c("Adrian Wojnarowski", "Shams Charania"))),
      mainPanel(
        plotlyOutput("tweets_graph") %>% withSpinner(color = "red")
      )
    )
)

server <- function(input, output) {
  woj_tweets <- reactive(
    get_timeline(user = "wojespn", n = 3250) %>% mutate(hour = hour(created_at), date = as.Date(created_at)) %>% 
      select(screen_name, created_at, text, source, favorite_count, retweet_count, is_retweet, hour, date)
      
  )
  
  output$tweets_graph <- renderPlotly({
    ggplotly(
      woj_tweets() %>% ggplot(aes(x = created_at, y = hour, size = favorite_count,
                                  text = paste0("@", screen_name,
                                                '<br>',
                                                '<b>', gsub('(.{1,90})(\\s|$)', '\\1\n', text), '</b>',
                                                '<br>',
                                                created_at,
                                                '<br>',
                                                '--------------------------------------------------------------------------------',
                                                '<br>',
                                                '<b>', paste(comma(retweet_count, accuracy = 1), "Retweets", "        ", comma(favorite_count, accuracy = 1), "Likes"), '</b>',
                                                '<br>',
                                                '--------------------------------------------------------------------------------'))) + 
        geom_jitter(alpha = 0.5) + xlab("Date") + ylab("Hour") + scale_size(range = c(1, 10)),
      tooltip = c("text"), height = 600) %>%
      style(hoverlabel = list(bgcolor = "white",
                              align = "left"))
  })
}


shinyApp(ui = ui, server = server)
