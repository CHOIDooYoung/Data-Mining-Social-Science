#======================================================#
# Title: 'Korean Text Mining Updated Version           #
# Sub-theme: "한국어 조사 제거"                        #
# Author: "CHOI, Doo Young Wicks"                      #
# Date: 2023-04-24                                     #
#======================================================#

# Loading the libraries for the 'Text Mining' Process
# 1) loading and writing Text or Excel files

library(readtext)# A package for reading and handling text data in various formats like plain text, CSV, JSON, or XML.
library(readxl) # A library for reading Excel files (both .xls and .xlsx formats) into R.
library(writexl) # A package to write data to Excel files (.xlsx) without any dependencies on Java or other external libraries.
# library(pdftools) # A library for extracting text and metadata from PDF files.
library(xlsx)

# 2) Text Cleansing
library(textclean) # A package providing functions for cleaning and preprocessing text data.

# 3) LSE Mining Tool
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(SnowballC)
library(tidytext)

# 4) A collection of R packages, including ggplot2, dplyr, and tidyr, designed for data science workflows.
library(tidyverse)

# 5) Stopwords library
library(stopwords)
# 6) plot library
library(ggplot2)
library(igraph)
library(ggraph)
# 7) Topic Model library
library(topicmodels)
library(seededlda)
# 8) Sampling Library
library (rsample)
# 9) Extra Fonts
library(extrafont)
# 10) Managing dates
library(lubridate)
# 11) Sentiment Analysis
library(sentometrics)
# 12) Table Manipulation


#=============================================================================
# 맥 환경일 때만 01 -> 아래  Create Co-occurrences Network 부분에도 활성화
# install.packages("showtext")
# library(showtext)
# font_add("AppleGothic", "/System/Library/Fonts/Supplemental/AppleGothic.ttf")
# showtext_auto()
#==============================================================================

# File Loading
SourceFile <- read_excel(file.choose())
Text.source <- SourceFile

# #====================================#
# # Set specific Date and category
# #====================================#
# 
# #Convernt Date format
# # Parse the dates using the dmy() function
# Text.source$date <- dmy(Text.source$date)
# # Format the dates in the desired format (YYYY-MM-DD)
# Text.source$date <- format(Text.source$date, "%Y-%m-%d")
# 
# # Print the formatted dates
# print(Text.source$date)


# # Set specific date
# Text.source <- Text.source[which(Text.source$date >= as.Date("2019-01-01") & Text.source$date <= as.Date("2023-12-31")),]
# 
# # 1st text cleansing, pick the specific contents
# Text.source <- Text.source %>%
#   filter(str_detect(Text.source$text, "alimentaire"))

#====================================
# 한국어 조사 전처리 
#====================================

particles <- c("은", "는", "이", "가", "을", "를", "에", "와", "과", "으로", "에게", 
               "로", "에서", "의", "인", "한")

remove_particles_quanteda <- function(text, particles) {
  pattern <- paste0("(?<=\\S)(", paste(particles, collapse = "|"), ")(?=\\s|$)")
  tokens <- tokens(text, remove_punct = TRUE)
  tokens <- tokens_remove(tokens, pattern, valuetype = "regex")
  return(tokens)
}


# Preparing Corpus Target
Mining.text.corpus <- Text.source


# Processing Corpus
Mining.text.corpus <- corpus(Mining.text.corpus)


# 부사 제거
Mining.text.corpus <- remove_particles_quanteda(Mining.text.corpus, particles)
print(Mining.text.corpus)


# Check Corpus Summary

summary(Mining.text.corpus)

kwic(Mining.text.corpus, pattern = "대통령", valuetype = "regex")


#============================================#
# Pre data clensing for the Text analysis    #
# Setting personal stopwords                 #
#============================================#
# mystopwords <- arabic_stopwords
# 

mystopwords <- c(Korean_Stopwords$`Korean Stowords`)


# Grouping words by dictionary or equivalence class

# dict <- dictionary(list(terror = c("terrorism", "terrorists", "threat"), 
# economy = c("jobs", "business", "grow", "work")))

# Tokenizing
Mining.TXT.token <- tokens(Mining.text.corpus, what = "word",
                           remove_numbers = TRUE, remove_punct = TRUE,
                           remove_symbols = TRUE, split_hyphens = TRUE,
                           remove_url=TRUE) %>% 
  tokens_remove(pattern = mystopwords,
                valuetype = 'fixed', padding = T)%>%
  tokens_remove(stopwords("ko", source = "marimo"))


Mining.TXT.token

# Remove e-mails
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+",
                                  valuetype = 'regex', padding = TRUE)
# remove web sites
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9_.+-]+\\.[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+",
                                  valuetype = 'regex', padding = TRUE)
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9_.+-]+\\.[a-zA-Z0-9-]+\\.[com]+",
                                  valuetype = 'regex', padding = TRUE)
Mining.TXT.token <- tokens_remove(Mining.TXT.token, "[a-zA-Z0-9-]+\\.[com]+",
                                  valuetype = 'regex', padding = TRUE)



# # Lemmatization (Unifying Synonyms)
# # Define your synonyms and create a custom dictionary:
# # Define synonyms
# synonyms.tunisie <-  c("tunisie", "tunisiens", "tunisienne","tunisien", "nationale")
# lemma.tunisie <- rep("tunisie", length(synonyms.tunisie))
# 
# synonyms.produit <-  c("produit", "produits", "production")
# lemma.produit <- rep("produit", length(synonyms.produit))
# 
# synonyms.agricole <-  c("agricole", "agricoles", "agriculteurs")
# lemma.agricole <- rep("agricole", length(synonyms.agricole))
# 
# 
# 
# 
# # Replace synonyms in tokens
# Mining.TXT.token <- tokens_replace(Mining.TXT.token, synonyms.tunisie, lemma.tunisie, valuetype = "fixed")
# Mining.TXT.token <- tokens_replace(Mining.TXT.token, synonyms.produit, lemma.produit, valuetype = "fixed")
# Mining.TXT.token <- tokens_replace(Mining.TXT.token, synonyms.agricole, lemma.agricole, valuetype = "fixed")




##================================================
# Refine stopwords
#=================================================

Ref.Stopword <- c("" )

mystopwords2 <- Ref.Stopword


Mining.TXT.token <- Mining.TXT.token %>% tokens_remove(mystopwords2)
# document feature matrix (dfm) with the  bag-of-words
Mining.TXT.token.dfm <- dfm(Mining.TXT.token)
# remove the term of frequency lesser than 5 times
Mining.TXT.token.dfm <- dfm_trim(Mining.TXT.token.dfm, min_termfreq = 5) %>%
  dfm_weight("boolean")


# Confirm data matrix
Mining.TXT.token.matrix <- as.matrix (Mining.TXT.token.dfm)
View (Mining.TXT.token.matrix)
dim (Mining.TXT.token.matrix)

# Term frequency (TF)
term.frequency <- function(row) {
  row / sum (row)
}

# Inverse Document Frequency (IDF)
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col >0))
  
  log10(corpus.size / doc.count)
}

# TF-IDF Calculation
tf.idf <- function(x, idf) {
  x * idf
}

# First step, normalize all documents via TF.
Mining.TXT.token.df <- apply(Mining.TXT.token.matrix, 1, term.frequency)
dim(Mining.TXT.token.df)
View(Mining.TXT.token.df)

# Second step, Calculate IDF vector
# for training data and for test data!
Mining.TXT.token.idf <- apply(Mining.TXT.token.matrix, 2, inverse.doc.freq)
str(Mining.TXT.token.idf)


# ML training of corpus (TF-IDF)
Mining.TXT.token.tfidf <-  apply(Mining.TXT.token.df, 2, tf.idf, idf = Mining.TXT.token.idf)
dim(Mining.TXT.token.tfidf)
View(Mining.TXT.token.tfidf)


# convert to the matrix
Mining.TXT.token.tfidf <- t(Mining.TXT.token.tfidf)
dim(Mining.TXT.token.tfidf)
View(Mining.TXT.token.tfidf)


# check incomplete part 
incomplete.cases <- which(!complete.cases(Mining.TXT.token.tfidf))
Mining.TXT.token$text1[incomplete.cases]


# minor corrections
Mining.TXT.token.tfidf[incomplete.cases,] <- rep(0.0, ncol(Mining.TXT.token.tfidf))
dim(Mining.TXT.token.tfidf)
sum (which(!complete.cases (Mining.TXT.token.tfidf)))


# Make a clean data frame using the same process as before.
Mining.TXT.token.df <- cbind(data.frame(Mining.TXT.token.tfidf))
names(Mining.TXT.token.df) <- make.names(names(Mining.TXT.token.df))

Mining.TXT.token.dfm<-dfm_select(Mining.TXT.token.dfm, min_nchar=3)


# =======================================
# Create Frequency Graph
# =======================================

Mining.TXT.token.dfm.inaug <- textstat_frequency(Mining.TXT.token.dfm, n =50)


# Sort by reverse frequency order
Mining.TXT.token.dfm.inaug$feature <- with(Mining.TXT.token.dfm.inaug, reorder(feature, -frequency))

ggplot(Mining.TXT.token.dfm.inaug, aes(x =feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(text = element_text(family = "AppleMyungjo")) +
  update.packages(ask = FALSE)


frequency.table <- Mining.TXT.token.dfm.inaug

# =======================================
# Create Co-occurrences Network
# =======================================

# Use Pre-made dfm
Mining.TXT.textplot.Network <- fcm(Mining.TXT.token.dfm)
Mining.TXT.textplot.Network <- fcm(Mining.TXT.token.dfm, tri = FALSE) # Create Co-occurrence Network 
feat <- names(topfeatures(Mining.TXT.textplot.Network, 50)) # Select most frequent Co-occurrence corpuses
set.seed(100)
Mining.TXT.textplot.Network <- fcm_select(Mining.TXT.textplot.Network, pattern = feat, verbose = FALSE)

# CON 그리기

Coocurrence.Network <- textplot_network(Mining.TXT.textplot.Network,
                       vertex_labelsize = 2.5 * rowSums(Mining.TXT.textplot.Network)/
                       min(rowSums(Mining.TXT.textplot.Network)),
                       min_freq = 0.95,
                       edge_size = 1)



#=================================================================================
# Set the font for the graph on MacOS environment
# 
# Coocurrence.Network <- Coocurrence.Network +
#   theme(plot.title = element_text(family = "AppleGothic"),
#         axis.title = element_text(family = "AppleGothic"),
#         axis.text = element_text(family = "AppleGothic"),
#         legend.title = element_text(family = "AppleGothic"),
#         legend.text = element_text(family = "AppleGothic"))
#================================================================================



print(Coocurrence.Network)



# write.xlsx(Mining.TXT.textplot.Network, "networkname.xlsx")
# =======================================
# Create a Topic Model 
# =======================================

# Optimal topic number test
result <- FindTopicsNumber(
  Mining.TXT.token.dfm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)



# Process Topic Model

tmod_lda <- textmodel_lda(Mining.TXT.token.dfm, k = 4)
terms(tmod_lda, 5)

# assign topic as a new document-level variable
Mining.TXT.token.dfm$topic <- topics(tmod_lda)

# cross-table of the topic frequency
table(Mining.TXT.token.dfm$topic)

# Assign as topic model data frame
Topic.Model <- data.frame(terms(tmod_lda, 5))

Topic.Model




#=====================================
# word cloud
#=====================================
set.seed(100)
textplot_wordcloud(Mining.TXT.token.dfm, min_count = 10,
                   color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))




# =======================================
# New Sentiment Analysis (easier)
# =======================================
#14) Quanteda Sentiment library
# remotes::install_github("quanteda/quanteda.sentiment")
library(quanteda.sentiment)
#15) Time Series data
library(zoo)

# Political Sentiment Dictionary: "data_dictionary_LSD2015"	
# DOI: 10.1080/10584609.2012.671234
# For French check https://www.poltext.org/en/donnees-et-analyses/lexicoder

# Economic Sentiment Dictionary: "data_dictionary_LoughranMcDonald"
# Loughran-McDonald Master Dictionary

# ensure we have this package's version of the dictionary
data("data_dictionary_LoughranMcDonald", package = "quanteda.sentiment")

# inspect the dictionary and its valences
print(data_dictionary_LSD2015, max_nval = 8)

# compute the sentiment

Sentiment.com <- Mining.text.corpus %>% textstat_polarity(dictionary = data_dictionary_LoughranMcDonald)
Sentiment.com 

# Merge with Data

Sentiment.date <- data.frame (Text.source$date) %>%
  rename(date = Text.source.date)

Sentiment.date

Sentiment.com
Sentiment.com = subset(Sentiment.com, select = - c(doc_id))
Sentiment.com


Sentiment.com <- cbind.data.frame (Sentiment.date, Sentiment.com)
Sentiment.com
# Convert to the time series data 


plot(Sentiment.com$date, Sentiment.com$sentiment, type = "l")

#=====================================
# Old Sentiment Analysis (complicate)
#=====================================


# Rename column name
# DF <- rename(DF,c("newname"="oldname"))
Text.source.sentiment <- rename(Text.source,c("texts"="text"))
# Create the id with the matching number of the sample
id <-  random_id(203, 2)
Text.source.sentiment <- cbind(Text.source.sentiment, id)

# Drop column
#Text.source.sentiment <- subset(Text.source.sentiment, select = -c(url, title))
# Make it to the data frame
# construct a object with sentiment measures
corpus <- sento_corpus(corpusdf = Text.source.sentiment)

class(corpus)

# list of lexicons:  "HENRY_en", "GI_en", "LM_en"
corpusSample <- corpus_sample(corpus)
l <- sento_lexicons(list_lexicons[("LM_en")], list_valence_shifters[["en"]])
ctr <- ctr_agg(howTime = c("equal_weight", "linear"), by = "day", lag = 2)
sentomeasures <- sento_measures(corpusSample, l, ctr)

#=====================================
# plot sentiment measures
#=====================================

plot(sentomeasures, group = "lexicons")
View(sentomeasures)
Article.sentiment <-data.frame(sentomeasures$sentiment)
Article.sentiment[is.na(sentomeasures$sentiment)] <- 0
mean(Article.sentiment$LM_en..dummyFeature)



#=====================================
# Emotional Measures
#=====================================
# A bag-of-words approach for computing emotions in text data using the lexicon compiled by Araque, Gatti, Staiano, and Guerini (2018).



library(transforEmotion)

Emotion <- emoxicon_scores(text = Text.source$text)

# Plot

barplot(
  sort(colSums(prop.table(Emotion[, 3:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)
