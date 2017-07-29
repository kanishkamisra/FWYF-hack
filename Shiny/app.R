library(shiny)
library(highcharter)
library(tidyverse)
library(tidytext)
library(data.table)
library(reshape2)

lyric_words <- as.data.frame(fread("lyric_words.csv", sep = ","))


# functions
cloudify <- function(artist_name) {
  lyric_words %>%
    filter(artist == artist_name) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = T) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    wordcloud::comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                                max.words = 200, scale = c(6,0.5), title.size = 3) 
}

sentiment_over_time <- function(artist_names) {
  # result <- lyric_words %>%
  #   filter(artist == artist_name) %>%
  #   inner_join(get_sentiments("bing")) %>%
  #   count(song, year = year, sentiment) %>%
  #   spread(sentiment, n, fill = 0) %>%
  #   mutate(sentiment = positive - negative) %>%
  #   group_by(year) %>%
  #   summarise(sentiment = median(sentiment))
  result <- lyric_words %>%
    filter(artist %in% artist_names) %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(artist, song, year) %>%
    summarise(sentiment = median(score)) %>%
    group_by(artist, year) %>%
    summarise(sentiment = mean(sentiment))
  
  # result %>%
  #   ggplot(aes(year, sentiment, group = artist, color = artist)) + 
  #   geom_line(size = 1) +
  #   theme_kani() + 
  #   scale_color_kani()
  
  highchart() %>%
    hc_add_series(data = result %>% mutate(sentiment = round(sentiment, 2)), type = "line", hcaes(year, sentiment, group = artist)) %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_yAxis(
      title = list(text = "Sentiment"),
      plotLines = list(list(color = "#000000", width = 2,value = 0))
    ) %>%
    hc_xAxis(title = list(text = "Year")) %>%
    hc_tooltip(
      formatter = JS(
        "function () {
	return  '<b>Artist: </b>' + this.series.name  + '<br/> <b>Sentiment: </b>' + this.y
}"
      )
    )
}

position_plot <- function(artist_name) {
  result <- lyric_words %>%
    filter(artist == artist_name) %>%
    group_by(artist, word) %>%
    summarise(counts = n(),
              median_position = median(word_position)) %>%
    inner_join(get_sentiments("bing"))
  
  start_10 <- result %>%
    filter(median_position < 0.5) %>%
    top_n(10, counts)
  
  end_10 <- result %>%
    filter(median_position >= 0.5) %>%
    top_n(10, counts)
  
  all <- rbind(start_10, end_10)
  
  all %>%
    mutate(word = reorder(word, -median_position),
           direction = ifelse(median_position < .5, "Beginning", "End")) %>%
    ggplot(aes(median_position, word, color = direction)) +
    geom_point(size = 5) +
    geom_errorbarh(aes(xmin = .5, xmax = median_position), height = 0) +
    geom_vline(xintercept = .5, lty = 2) +
    scale_x_continuous(labels = scales::percent_format()) +
    expand_limits(x = c(0, 1)) +
    theme_minimal(base_family = "Roboto Condensed") +
    theme(
      plot.title = element_text(family = "Roboto", size = rel(2), face = "bold"),
      plot.subtitle = element_text(family = "Roboto", size = rel(1.4)),
      legend.position = "none"
    ) +
    labs(
      x = "Position in song",
      y = "Word",
      title = "Words that occur in either beginning or end",
      subtitle = "using words that occur atleast 100 times",
      color = ""
    )
}

artist_choices <- lyric_words %>%
  distinct(artist) %>%
  pull(artist)

# ui
ui <- shinyUI(fluidPage(
  titlePanel(title = h1("Some body once told me", align = "center"), windowTitle = "APP"),
  fluidRow(
    column(
      8,
      offset = 2,
      align = "center",
      tabsetPanel(
        tabPanel(
          "Cloudify",
          fluidRow(
            column(12,
                   selectInput(
                     "artist_1", 
                     label = "Choose and artist to cloudify",
                     choices = artist_choices, 
                     selected = "coldplay"
                   )),
            fluidRow(
              column(12,
                     plotOutput("cloudify"))
            )
          )
        ),
        tabPanel(
          "Position-Plot",
          fluidRow(
            column(
              12,
              selectInput(
                "artist_2", 
                label = "Choose and artist to analyze",
                choices = artist_choices, 
                selected = "coldplay"
              )
            ),
            fluidRow(
              column(
                12,
                plotOutput("positions")
              )
            )
          )
        ),
        tabPanel(
          "Sentiment over time",
          fluidRow(
            column(
              12,
              selectInput(
                "artists",
                label = "Choose an artists or compare multiple ones",
                choices = artist_choices,
                selected = "coldplay",
                selectize = T,
                multiple = T
              )
            ),
            fluidRow(
              column(
                12,
                highchartOutput("sentiment_time")
              )
            )
          )
        )
      )
    )
  )
))

# server

server <- shinyServer(function(input, output) {
  output$cloudify <- renderPlot({
    cloudify(input$artist_1)
  })
  
  output$positions <- renderPlot({
    position_plot(input$artist_2)
  })
  
  output$sentiment_time <- renderHighchart({
    sentiment_over_time(input$artists)
  })
  
  
})

# run
shinyApp(ui, server)
