library(tidyverse)
library(magrittr)
library(lubridate)
library(tidytext)
library(forcats)
library(stringr)
library(treemap)
library(shinydashboard)
library(shiny)
library(markdown)

# Load in tweets from pull_tweets.R
load('kyleg_tweet_shiny.rda')

# Create named vector to be used in select input.
legislators <- ky_leg_filtered$LastUnique
names(legislators) <- paste(ifelse(str_sub(ky_leg_filtered$District,1,1) == 'H','Rep.', 'Sen.'),
                            ky_leg_filtered$FirstName,
                            ky_leg_filtered$LastName)

# Data from `tidytext` used in sentiment analysis.
data('stop_words')
bing <- get_sentiments('bing')
nrc <- get_sentiments('nrc')

# ui portion of shiny app
ui <- dashboardPage(
  dashboardHeader(title = 'Kentucky Legislature Twitter Sentiment Dashboards'),
  dashboardSidebar(
    selectInput('legislator', 'Legislator:', legislators)
  ),
  dashboardBody(
    fluidRow(
      box(uiOutput('legislator_name'),
          uiOutput('legislator_handle'),
          uiOutput('legislator_party'),
          uiOutput('num_tweets'), width = 12, align = 'center')
    ),
    fluidRow(
      box(title = 'Bing Sentiment', status = 'primary', solidHeader = T,
          plotOutput('bing_sentiment')),
      box(title = 'NRC Sentiment', status = 'primary', solidHeader = T,
          plotOutput('nrc_sentiment'))
    ),
    fluidRow(
      box(title = 'Tweets By Hour Of Day', status = 'primary', solidHeader = T,
          plotOutput('time_hour')),
      box(title = 'Tweets Over Time', status = 'primary', solidHeader = T,
          plotOutput('time_day'))
    ),
    fluidRow(
      box(title = 'Tweets By Source', status = 'primary', solidHeader = T,
          plotOutput('source'))
    ),
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    fluidRow(
      box(includeMarkdown('footer.md'), width = 12)
    )
  )
)

# Server portion of shiny app.
server <- function(input, output){
  # Create several reactive items that will be used to create output. 
  handle <- reactive({paste0('@',ky_leg_filtered$user_name[which(ky_leg_filtered$lastunique == input$legislator)])})
  leg_party <- reactive({ky_leg_filtered$Party[which(ky_leg_filtered$lastunique == input$legislator)]})
  leg_name <- reactive({paste(ifelse(str_sub(ky_leg_filtered$District[which(ky_leg_filtered$lastunique == input$legislator)],1,1) == 'H',
                           'Rep.', 'Sen.'),
                    ky_leg_filtered$FirstName[which(ky_leg_filtered$lastunique == input$legislator)],
                    ky_leg_filtered$LastName[which(ky_leg_filtered$lastunique == input$legislator)])})
  race <- reactive({ky_leg_filtered$Race[which(ky_leg_filtered$lastunique == input$legislator)]})
  tweets <- reactive({ky_leg_filtered$tweets[[which(ky_leg_filtered$lastunique == input$legislator)]]})
  total_tweets <- reactive({nrow(tweets())})
  retweets <- reactive({nrow(filter(tweets(), is_retweet == F))})
  original_content <- reactive({total_tweets() - retweets()})
  sa <- reactive({tweets() %>% 
    filter(is_retweet == F) %>% 
    unnest_tokens(word, text) %>%  
    anti_join(stop_words) %>% 
    mutate(bing_sentiment = map_chr(word, function(i) ifelse(i %in% bing$word, bing$sentiment[which(bing$word == i)], NA)),
           nrc_sentiment = map_chr(word, function(i) ifelse(i %in% nrc$word, nrc$sentiment[which(nrc$word == i)], NA))) %>% 
    select(word, bing_sentiment, nrc_sentiment)})
  
  # Create output from the data above.
  output$legislator_name <- renderUI({ h1(leg_name(), align = 'center') })
  output$legislator_handle <- renderUI({ h2(handle(), align = 'center') %>% 
      a(href = paste0('https://twitter.com/',handle()))})
  output$legislator_party <- renderUI({ h3(leg_party(), align = 'center')})
  output$num_tweets <- renderUI({ 
    h3(paste0('Total Tweets: ', total_tweets(),
              ' | Retweets: ', retweets()),
       align = 'center') 
  })
  
  # Graphics created using `ggplot2`, based on data from reactive elements above.
  output$bing_sentiment <- renderPlot({
    sa() %>% 
      group_by(bing_sentiment) %>% 
      dplyr::summarize(n=n()) %>% 
      filter(!is.na(bing_sentiment)) %>% 
      ggplot(aes(fct_reorder(bing_sentiment, n), n, label = n)) +
      geom_bar(stat = 'identity') +
      geom_label() +
      theme_minimal() +
      labs(subtitle = 'Original Content Only',
           x = '', y = 'Total Words')
  })
  
  output$nrc_sentiment <- renderPlot({
    sa() %>% 
      select(word, sentiment = nrc_sentiment) %>% 
      group_by(sentiment) %>% 
      filter(!is.na(sentiment)) %>% 
      count(word, sort = T) %>% 
      treemap(index =c('sentiment', 'word'), vSize = 'n',
              title = 'Original Content Only')
  })
  
  output$time_hour <- renderPlot({
    ggplot(tweets(), aes(x = hour(created_at))) +
      geom_density() +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent) +
      labs(subtitle = 'Retweets Included',
           x = 'Hour', y = '')
  })
  
  output$time_day <- renderPlot({
    ggplot(tweets(), aes(x = created_at)) + 
      geom_histogram(bins = ymd(20170829) - ymd(20170101)) +
      theme_minimal() +
      labs(subtitle = 'Retweets Included',
           x = 'Day', y = 'Number of Tweets')
  })
  
  output$source <- renderPlot({
    tweets() %>% 
      group_by(source) %>% 
      summarize(n=n()) %>% 
      ggplot(aes(x = source, y = n, label = n)) +
      geom_bar(stat = 'identity') +
      geom_label(nudge_y = 5)+
      theme_minimal() +
      labs(subtitle = 'Retweets Included',
           x = '', y = 'Number of Tweets')
  })
}

shinyApp(ui,server)