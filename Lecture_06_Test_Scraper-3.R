# Scraping Web data in R
# Rvest is the scraping tool to scrape web page
library("rvest")
library("RCurl")
library("XML")
library("tidyverse")

# Start by reading a HTML page with read_html():
News.source <- read_html("https://www.bbc.com/news/world-europe-63190844")

News.time <- News.source %>%
  html_node("time") %>%
  html_text() %>% str_replace_all(pattern = "\t|\r|\n", replacement = "")

News.time

News.title <- News.source %>%
  html_node("#main-heading") %>%
  html_text() %>% str_replace_all(pattern = "\t|\r|\n", replacement = "")

News.title


News.text <- News.source %>%
  html_nodes("#main-content") %>%
  html_text() %>% str_replace_all(pattern = "\t|\r|\n", replacement = "")  # %>%
  # paste(collapse = " ")

News.text 


