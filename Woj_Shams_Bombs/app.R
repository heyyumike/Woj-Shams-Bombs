library(shiny)
library(tidyverse)
library(rtweet)

ui <- fluidPage(
    titlePanel("Woj/Shams Bombs")
)

server <- function(input, output) {
}


shinyApp(ui = ui, server = server)
