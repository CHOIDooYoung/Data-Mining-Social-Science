## Load the packages
library(rvest)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(readxl)
library(httr)
library(purrr)
library(XML)


#========================
# escape sequence	
# \n	Starts a new line
# \r  Carrige return
# \t	Horizontal tab
# \b	Backspace
# \"	Double quote
# \\	Single Backslash
#=========================
# For more details on Character Class Sequences Refer following URL
# https://www.gastonsanchez.com/r4strings/character-sets.html
#========================

# ===========================================
# Semi Automatic Method
#===========================================



## Load the data source 
News.source <- read_excel(file.choose())

## Scrape the title and body of articles on the news page

News.source$date   <- ""
News.source$title  <- ""
News.source$texts  <- ""

for (i in 1:dim(News.source)[1]){
  html <- read_html(as.character(News.source$url[i]))
  
  
  temp_news_date    <- html %>% html_nodes("time") %>% html_text() %>%
    str_replace_all(pattern = "\t|\r|\n", replacement = "")
  temp_news_title   <- html %>% html_nodes("#main-heading") %>% html_text() %>%
    str_replace_all(pattern = "\t|\r|\n", replacement = "")
  temp_news_content <- html %>% html_nodes("#main-content") %>% html_text() %>%
    str_replace_all(pattern = "\t|\r|\n", replacement = "")
  
  
  
  
  if (length(temp_news_title)>0){
    
    News.source$date[i]   <- temp_news_date
    News.source$title[i]   <- temp_news_title
    News.source$texts[i] <- temp_news_content
  }
}

gsub("googletag.*\\}\\)\\W", "", News.source$texts)


## drop unnecessary text such as whitespace and line breaks in the body
News.source$texts<- gsub("\n", "", News.source$texts)
News.source$texts <- gsub("\t", "", News.source$texts)
News.source$texts <- gsub("//", "", News.source$texts)
News.source$texts <- gsub("\\()", "", News.source$texts)
News.source$texts <- gsub("\\{}", "", News.source$texts)


write.xlsx(News.source, "BBC_News_lecture06.xlsx")

News.source


# ===========================================
# Full Automatic Method
#===========================================

# An R package for parallel web crawling and scraping.
# For the specific details : https://cran.r-project.org/web/packages/Rcrawler/Rcrawler.pdf

library(Rcrawler)

Rcrawler(Website = "https://www.bbc.com/", 
         KeywordsFilter = c("Putin","Ukraine"), KeywordsAccuracy = 95,
         no_cores = 2, no_conn = 2, MaxDepth = 5,
         ExtractCSSPat = c("#main-heading", "#main-content"),
         PatternsNames = c("Title","Content"), Obeyrobots = TRUE)

