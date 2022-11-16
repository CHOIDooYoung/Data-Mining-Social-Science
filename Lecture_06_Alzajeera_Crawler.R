## Load the packages
library(rvest)
library(tidyverse)
library(dplyr)
library(openxlsx)
library(readxl)
library(httr)
library(purrr)
library(XML)


## Load the data source 
News.source <- read_excel(file.choose())

## Scrape the title and body of articles on the news page

News.source$date   <- ""
News.source$title  <- ""
News.source$texts  <- ""

for (i in 1:dim(News.source)[1]){
  html <- read_html(as.character(News.source$url[i]))
  
  # CSS
  
  #temp_news_date    <- html %>% html_nodes("body.post-template-default.single.single-post.postid-9653.single-format-standard.wp-custom-logo.group-blog.fullwidth_layout.right-sidebar:nth-child(2) div.site:nth-child(1) div.site-content div.mt-container div.content-area main.site-main article.post-9653.post.type-post.status-publish.format-standard.has-post-thumbnail.hentry.category-industry.category-jobscareer:nth-child(1) header.entry-header div.entry-meta span.posted-on a:nth-child(1) > time.entry-date.published.updated") %>% html_text() %>%
  #                     str_replace_all(pattern = "\t|\r|\n", replacement = "")
  #temp_news_title   <- html %>% html_nodes("body.post-template-default.single.single-post.postid-9653.single-format-standard.wp-custom-logo.group-blog.fullwidth_layout.right-sidebar:nth-child(2) div.site:nth-child(1) div.site-content div.mt-container div.content-area main.site-main article.post-9653.post.type-post.status-publish.format-standard.has-post-thumbnail.hentry.category-industry.category-jobscareer:nth-child(1) header.entry-header > h1.entry-title") %>% html_text() %>%
  #                     str_replace_all(pattern = "\t|\r|\n", replacement = "")
  #temp_news_content <- html %>% html_nodes("body.post-template-default.single.single-post.postid-9653.single-format-standard.wp-custom-logo.group-blog.fullwidth_layout.right-sidebar:nth-child(2) div.site:nth-child(1) div.site-content div.mt-container div.content-area main.site-main article.post-9653.post.type-post.status-publish.format-standard.has-post-thumbnail.hentry.category-industry.category-jobscareer:nth-child(1) div.entry-content > p:nth-child(20)") %>% html_text() %>%
  #                     str_replace_all(pattern = "\t|\r|\n", replacement = "")
  

  
  # Xpath
  
  temp_news_date    <- html %>% html_nodes(".date-simple") %>% html_text() %>%
                       str_replace_all(pattern = "\t|\r|\n", replacement = "")
  temp_news_title   <- html %>% html_nodes(".article-header h1") %>% html_text() %>%
                       str_replace_all(pattern = "\t|\r|\n", replacement = "")
  temp_news_content <- html %>% html_nodes("div.container.container--grid.container--article.container") %>% html_text() %>%
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


write.xlsx(News.source, "Ajazeera_News_lecture06.xlsx")

News.source



