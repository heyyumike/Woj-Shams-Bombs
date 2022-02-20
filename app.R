library(shiny)
library(plotly)
library(tidyverse)
library(rtweet)
library(shinycssloaders)
library(lubridate)
library(scales)

# twitter credentials 
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
                   imageOutput(outputId = "twitter_image",
                               height = 175),
                   
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
    get_timeline(user = "wojespn", n = 3250) %>% 
      mutate(hour = hour(created_at), date = as.Date(created_at), tweet_category = case_when(
        # pattern matching for tweet categories (e.g. injury, trade, contract extension, etc. related news)
        grepl("woj pod|pod|podcast", text, ignore.case = TRUE) ~ "The Woj Pod/Podcast-Related",
        grepl("espn story", text, ignore.case = TRUE) ~ "ESPN Story", 
        grepl("covid|coronavirus|health and safety|h&|quarantine|vaccine|vaccinated|vaccination|virus|pandemic|mask", text, ignore.case = TRUE) ~ "COVID-Related",
        grepl("contract|contract extension|extension|deal|signing|player option", text, ignore.case = TRUE) ~ "Contract Talks",
        grepl("trade|trading|acquire|acquiring|sending|send", text, ignore.case = TRUE) ~ "Trade Talks",
        grepl("injury|injured|sprain|x-ray|surgery|procedure|mri|strain|hamstring|injuries|bruise|imaging|ankle|acl|fracture", text, ignore.case = TRUE) ~ "Injury-Related News",
        grepl("rookie", text, ignore.case = TRUE) ~ "Rookie-Related News",
        grepl("No\\.|draft", text, ignore.case = FALSE) ~ "Draft-Related News"
      )) %>% 
      select(screen_name, created_at, text, tweet_category, source, favorite_count, retweet_count, is_retweet, hour, date)
      
  )
  
  output$tweets_graph <- renderPlotly({
    ggplotly(
      woj_tweets() %>% ggplot(aes(x = created_at, y = hour, size = favorite_count, color = tweet_category,
                                  # tooltip adjustments
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
                              align = "left")) %>%
      layout(legend = list(title = list(text = "")))
  })
  
  output$twitter_image <- renderImage({
    list(src = paste0("twitter_avatars/", input$insider, ".png"),
         height = "70%", width = "100%")
  }, deleteFile = FALSE)
}


shinyApp(ui = ui, server = server)
