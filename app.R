library(shiny)
library(plotly)
library(tidyverse)
library(rtweet)
library(shinycssloaders)
library(lubridate)
library(zoo)
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
  tags$head(
    tags$script(async = NA, src = "https://platform.twitter.com/widgets.js")
  ),
  
  titlePanel("Woj/Shams Bombs"),
  
  sidebarLayout(
    sidebarPanel(width = 2,
                 helpText(strong("Popularity of tweets from NBA Insiders Adrian Wojnarowski (@wojespn)
                                   and Shams Charania (@ShamsCharania)")),
                 
                 br(),
                 
                 imageOutput(outputId = "twitter_image",
                             height = "120%"),
                 
                 br(),
                 
                 selectInput(inputId = "nba_insider",
                             label = "Select a NBA Insider",
                             choices = c("Adrian Wojnarowski", "Shams Charania")),
                 
                 textInput(inputId = "word_filter",
                           label = "Search tweets"),
                 
                 radioButtons(inputId = "tweet_selection_criteria",
                              label = "Tweet Selection",
                              choices = c("Most Liked", "Most Retweeted", "Most Recent"))),
    mainPanel(
      plotlyOutput("tweets_graph", height = "125%", width = "125%") %>% withSpinner(color = "red"),
      plotlyOutput("tweet_frequency_graph", width = "125%") %>% withSpinner(color = "red"),
      uiOutput("tweet") %>% withSpinner(color = "red")
    )
  )
)

server <- function(input, output) {
  woj_shams_tweets <- reactive(
    get_timeline(user = "wojespn", n = 3250) %>% mutate(nba_insider = "Adrian Wojnarowski") %>%
      bind_rows(
        get_timeline(user = "ShamsCharania", n = 3250) %>% mutate(nba_insider = "Shams Charania")
      ) %>% mutate(hour = hour(created_at), date = as.Date(created_at), month_year = as.yearmon(created_at), tweet_category = case_when(
        # pattern matching for tweet categories (e.g. injury, trade, contract extension, etc. related news)
        grepl("woj pod|podcast", text, ignore.case = TRUE) ~ "The Woj Pod/Podcast-Related",
        grepl("injury|injured|sprain|x-ray|surgery|procedure|mri|strain|hamstring|injuries|bruise|imaging|ankle|acl|fracture|concussion|rehab|achilles|tightness", text, ignore.case = TRUE) ~ "Injury-Related News",
        grepl("covid|coronavirus|health and safety|h&|quarantine|vaccine|vaccinated|vaccination|virus|pandemic|mask|protocol|contact tracing", text, ignore.case = TRUE) ~ "COVID-Related",
        grepl("contract|contract extension|extension|deal|signing|player option|offer|free agent", text, ignore.case = TRUE) ~ "Contract Talks",
        grepl("trade|trading|acquire|acquiring|sending|send", text, ignore.case = TRUE) ~ "Trade Talks",
        grepl("No\\.|draft|Draft|rookie", text, ignore.case = FALSE) ~ "Draft/Rookie Related News",
        grepl("coach|coaching|assistant", text, ignore.case = TRUE) ~ "Coach/Assistant Related News",
        grepl("espn story|theathletic", text, ignore.case = TRUE) ~ "ESPN Story/The Athletic Related"
        #TRUE ~ "Miscellaneous"
      )) %>% 
      select(screen_name, status_id, status_url, nba_insider, created_at, text, tweet_category, source, favorite_count, retweet_count, is_retweet, hour, date, month_year)
  )
  
  woj_shams_monthly_tweets <- reactive(
    woj_shams_tweets() %>% filter(is_retweet == FALSE) %>% group_by(nba_insider, month_year) %>%
      summarise(tweet_count = n(), avg_favorite = mean(favorite_count))
  )
  
  woj_shams_most_liked_tweets <- reactive(
    woj_shams_tweets() %>% group_by(nba_insider) %>% top_n(n = 1, wt = favorite_count) %>% select(nba_insider, screen_name, status_id, status_url, text, favorite_count)
  )
  
  woj_shams_most_retweeted_tweets <- reactive(
    woj_shams_tweets() %>% group_by(nba_insider) %>% top_n(n = 1, wt = retweet_count) %>% select(nba_insider, screen_name, status_id, status_url, text, retweet_count)
  )
  
  woj_shams_most_recent_tweets <- reactive(
    woj_shams_tweets() %>% group_by(nba_insider) %>% summarise_all(first) %>% select(nba_insider, screen_name, status_id, status_url, text, retweet_count, favorite_count)
  )
  
  output$tweet_frequency_graph <- renderPlotly({
    if (input$nba_insider == "Adrian Wojnarowski") {
      woj_monthly_data <- reactive(
        woj_shams_monthly_tweets() %>% filter(nba_insider == "Adrian Wojnarowski")
      )
      
      woj_monthly_coeff <- reactive(
        max(woj_monthly_data()$avg_favorite) / max(woj_monthly_data()$tweet_count)
      )
      
      ggplotly(
        woj_monthly_data() %>% ggplot(aes(x = month_year)) +
          geom_bar(aes(y = tweet_count,
                       text = paste0("Month Year: ", month_year,
                                     '<br>',
                                     "Tweet Count: ", tweet_count)), stat = "identity", color = "black", fill = "white") +
          geom_line(aes(y = avg_favorite / woj_monthly_coeff()), linetype = "dashed") +
          geom_point(aes(y = avg_favorite / woj_monthly_coeff(),
                         text = paste0("Month Year: ", month_year,
                                       '<br>',
                                       "Avg. Favorites: ", comma(avg_favorite)))) +
          scale_y_continuous(name = "Tweet Count", sec.axis = sec_axis(trans = ~.*woj_monthly_coeff(), name = "Avg. Favorites")) + xlab("Date") +
          ggtitle("Tweet Count and Avg. Likes Per Month (not including retweets)"),
        tooltip = c("text"))
    } else {
      shams_monthly_data <- reactive(
        woj_shams_monthly_tweets() %>% filter(nba_insider == "Shams Charania")
      )
      
      shams_monthly_coeff <- reactive(
        max(shams_monthly_data()$avg_favorite) / max(shams_monthly_data()$tweet_count)
      )
      
      ggplotly(
        shams_monthly_data() %>% ggplot(aes(x = month_year)) +
          geom_bar(aes(y = tweet_count,
                       text = paste0("Month Year: ", month_year,
                                     '<br>',
                                     "Tweet Count: ", tweet_count)), stat = "identity", color = "black", fill = "white") +
          geom_line(aes(y = avg_favorite / shams_monthly_coeff()), linetype = "dashed") +
          geom_point(aes(y = avg_favorite / shams_monthly_coeff(),
                         text = paste0("Month Year: ", month_year,
                                       '<br>',
                                       "Avg. Favorites: ", comma(avg_favorite)))) +
          scale_y_continuous(name = "Tweet Count", sec.axis = sec_axis(trans = ~.*shams_monthly_coeff(), name = "Avg. Favorites")) + xlab("Date") +
          ggtitle("Tweet Count and Avg. Likes Per Month (not including retweets)"),
        tooltip = c("text"))
    }
  })
  
  output$tweets_graph <- renderPlotly({
    if (input$nba_insider == "Adrian Wojnarowski") {
      woj_tweets <- reactive(
        woj_shams_tweets() %>% filter(nba_insider == "Adrian Wojnarowski", grepl(input$word_filter, text, ignore.case = TRUE))
      )
      
      validate(
        need(dim(woj_tweets())[1] != 0, paste0("Search term not found. Please try again.")),
        need(sum(woj_tweets()$favorite_count) != 0, paste0("You've searched for a term found only in retweets. Please try again."))
      )
      
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
        tooltip = c("text"), height = 600) %>% style(hoverlabel = list(bgcolor = "white", align = "left")) %>% 
        layout(legend = list(title = list(text = "")))
    } else {
      shams_tweets <- reactive(
        woj_shams_tweets() %>% filter(nba_insider == "Shams Charania", grepl(input$word_filter, text, ignore.case = TRUE))
      )
      
      validate(
        need(dim(shams_tweets())[1] != 0, paste0("Search term not found. Please try again."))
      )
      
      ggplotly(
        shams_tweets() %>% ggplot(aes(x = created_at, y = hour, size = favorite_count, color = tweet_category,
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
        tooltip = c("text"), height = 600) %>% style(hoverlabel = list(bgcolor = "white", align = "left")) %>% 
        layout(legend = list(title = list(text = "")))
    }
  })
  
  output$twitter_image <- renderImage({
    list(src = paste0("twitter_avatars/", input$nba_insider, ".jpeg"),
         height = "120%", width = "100%")
  }, deleteFile = FALSE)
  
  output$tweet <- renderUI({
    if (input$nba_insider == "Adrian Wojnarowski") {
      if (input$tweet_selection_criteria == "Most Liked") {
        tagList(
          tags$blockquote(class = "twitter-tweet",
                          tags$a(href = woj_shams_most_liked_tweets() %>% ungroup() %>% filter(nba_insider == "Adrian Wojnarowski") %>% select(status_url) %>% pull())),
          tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
      } else if (input$tweet_selection_criteria == "Most Retweeted") {
        tagList(
          tags$blockquote(class = "twitter-tweet",
                          tags$a(href = woj_shams_most_retweeted_tweets() %>% ungroup() %>% filter(nba_insider == "Adrian Wojnarowski") %>% select(status_url) %>% pull())),
          tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
      } else {
        tagList(
          tags$blockquote(class = "twitter-tweet",
                          tags$a(href = woj_shams_most_recent_tweets() %>% ungroup() %>% filter(nba_insider == "Adrian Wojnarowski") %>% select(status_url) %>% pull())),
          tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
      }
    } else {
      if (input$tweet_selection_criteria == "Most Liked") {
        tagList(
          tags$blockquote(class = "twitter-tweet",
                          tags$a(href = woj_shams_most_liked_tweets() %>% ungroup() %>% filter(nba_insider == "Shams Charania") %>% select(status_url) %>% pull())),
          tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
      } else if (input$tweet_selection_criteria == "Most Retweeted") {
        tagList(
          tags$blockquote(class = "twitter-tweet",
                          tags$a(href = woj_shams_most_retweeted_tweets() %>% ungroup() %>% filter(nba_insider == "Shams Charania") %>% select(status_url) %>% pull())),
          tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
      } else {
        tagList(
          tags$blockquote(class = "twitter-tweet",
                          tags$a(href = woj_shams_most_recent_tweets() %>% ungroup() %>% filter(nba_insider == "Shams Charania") %>% select(status_url) %>% pull())),
          tags$script('twttr.widgets.load(document.getElementById("tweet"));')
        )
      }
    }
  })
}

shinyApp(ui = ui, server = server)
