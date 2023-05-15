#======================================================#
# Title: 'Lecture 11 Text Mining Reference             #
# Sub-theme: "Quanteda"                                #
# Author: "CHOI, Doo Young Wicks"                      #
# Date: 2023-04-24                                     #
#======================================================#

# This script is for conducting a text mining analysis on a specific source file. 
# It loads the necessary libraries, conducts text cleaning and preprocessing, 
# creates graphs of word frequencies and co-occurrences, performs sentiment analysis, 
# and computes emotion scores.


#======================================================
# Loading the libraries for the 'Text Mining' Process
#======================================================
# Each library is loaded individually with a brief comment on its purpose. 
# For example, the quanteda family of libraries is used for quantitative analysis of textual data. 
# The tidyverse library is used for data manipulation and visualization.



# 1) loading and writing Text or Excel files

library(readtext)# A package for reading and handling text data in various formats like plain text, CSV, JSON, or XML.
library(readxl) # A library for reading Excel files (both .xls and .xlsx formats) into R.
library(writexl) # A package to write data to Excel files (.xlsx) without any dependencies on Java or other external libraries.
# library(pdftools) # A library for extracting text and metadata from PDF files.
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
library(scales)
library(syuzhet)

#======================================================
# Load Source File
#======================================================
# The source file is loaded into a dataframe, and then a copy of the dataframe is made for text mining.


# File Loading
SourceFile <- read_excel(file.choose())
Text.source <- SourceFile


#====================================#
# Set specific Date and category
#====================================#

#Convernt Date format
# Parse the dates using the dmy() function
# Text.source$date <- dmy(Text.source$date)
# Format the dates in the desired format (YYYY-MM-DD)
# Text.source$date <- format(Text.source$date, "%Y-%m-%d")

# Print the formatted dates
print(Text.source$date)


# Set the specific dates
Text.source <- Text.source[which(Text.source$date >= as.Date("2015-01-01") & Text.source$date <= as.Date("2020-12-31")),]

# Select a Category
# Education, Energy, Agriculture, Healthcare, Logistics and Transportation

Category<-c("Agriculture")
Text.source <- Text.source %>%
  filter(str_detect(Text.source$category, Category))


# 1st text cleansing, pick the specific contents
Text.source <- Text.source %>%
  filter(str_detect(Text.source$text, "Kenya"))

#====================================
# Pre-processing Data
#====================================
# This section prepares the corpus (a collection of text documents) for text mining.
# It then performs a keyword in context (KWIC) analysis for the word "farming".

# Preparing Corpus Target
Mining.text.corpus <- Text.source

# Processing Corpus
Mining.text.corpus <- corpus(Mining.text.corpus)

# Check Corpus Summary

summary(Mining.text.corpus)

#Exploring corpus texts
# The kwic function (keywords-in-context) performs a search for 
# a word and allows us to view the contexts in which it occurs:

kwic(Mining.text.corpus, pattern = "farming", valuetype = "regex")


#============================================#
# Pre data clensing for the Text analysis    #
# Setting personal stopwords                 #
#============================================#
# This section tokenizes the corpus (breaks the text down into individual words), removes stopwords 
# (common words that do not carry much information), and conducts lemmatization 
# (reducing words to their base or dictionary form).



mystopwords <- c(English_Stopwords$English.Stopwords)


# Tokenizing
Mining.TXT.token <- tokens(Mining.text.corpus, what = "word",
                           remove_numbers = TRUE, remove_punct = TRUE,
                           remove_symbols = TRUE, split_hyphens = TRUE,
                           remove_url=TRUE) %>% 
  tokens_remove(pattern = mystopwords,
                valuetype = 'fixed', padding = T)%>%
  tokens_remove(stopwords("english")) %>%
  tokens_keep(pattern = "^[a-zA-Z]+$", valuetype = "regex") %>% 
  tokens_tolower(keep_acronyms = FALSE) %>%
  tokens_wordstem("english") # %>%
# tokens_compound(list(c("arm", "forc"))) %>%
# tokens_compound(list(c("air", "forc")))

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

# Lemmatization (Unifying Synonyms)
# Define your synonyms and create a custom dictionary:
# Define synonyms
synonyms.farm <-  c("farm", "farmer")
lemma.farm <- rep("farm", length(synonyms.farm))

synonyms.produit <-  c("produit", "produits", "production")
lemma.produit <- rep("produit", length(synonyms.produit))

synonyms.agricole <-  c("agricole", "agricoles", "agriculteurs")
lemma.agricole <- rep("agricole", length(synonyms.agricole))

synonyms.Kenya <-  c("Kenya", "Kenyan")
lemma.Kenya <- rep("Kenya", length(synonyms.Kenya))

# Replace synonyms in tokens
Mining.TXT.token <- tokens_replace(Mining.TXT.token, synonyms.farm, lemma.farm, valuetype = "fixed")
Mining.TXT.token <- tokens_replace(Mining.TXT.token, synonyms.produit, lemma.produit, valuetype = "fixed")
Mining.TXT.token <- tokens_replace(Mining.TXT.token, synonyms.agricole, lemma.agricole, valuetype = "fixed")
Mining.TXT.token <- tokens_replace(Mining.TXT.token, synonyms.Kenya, lemma.Kenya , valuetype = "fixed")




##================================================
# Refine stopwords
#=================================================
# This section removes additional stopwords that are specific to the text being analyzed.

Ref.Stopword <- c("farm", "agricultur","high", "manag", "includ", "nation", "increas", 
                  "good","area","acr", "Kenya", "cost", "sell",
                  "requir", "small", "start")

mystopwords2 <- Ref.Stopword


Mining.TXT.token <- Mining.TXT.token %>% tokens_remove(mystopwords2)

# document feature matrix (dfm) with the  bag-of-words
Mining.TXT.token.dfm <- dfm(Mining.TXT.token)
# remove the term of frequency lesser than 5 times
Mining.TXT.token.dfm <- dfm_trim(Mining.TXT.token.dfm, min_termfreq = 5)

# Calculate TF-IDF
Mining.TXT.token.tfidf <- dfm_tfidf(Mining.TXT.token.dfm)

# convert to the matrix
Mining.TXT.token.tfidf <- t(Mining.TXT.token.tfidf)
dim(Mining.TXT.token.tfidf)
View(Mining.TXT.token.tfidf)


# Make a clean data frame using the same process as before.
Mining.TXT.token.df <- cbind(data.frame(Mining.TXT.token.tfidf))
names(Mining.TXT.token.df) <- make.names(names(Mining.TXT.token.df))

Mining.TXT.token.dfm<-dfm_select(Mining.TXT.token.dfm, min_nchar=3)


# =======================================
# Create Frequency Graph
# =======================================
# This section creates a graph of the 50 most frequent words in the corpus.

Mining.TXT.token.dfm.inaug <- textstat_frequency(Mining.TXT.token.dfm, n =50)


# Sort by reverse frequency order
Mining.TXT.token.dfm.inaug$feature <- with(Mining.TXT.token.dfm.inaug, reorder(feature, -frequency))

ggplot(Mining.TXT.token.dfm.inaug, aes(x =feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

frequency.table <- Mining.TXT.token.dfm.inaug

# =======================================
# Create Co-occurrences Network
# =======================================
# This section creates a co-occurrence network, which shows which words often appear together in the corpus.

# Use Pre-made dfm
Mining.TXT.textplot.Network <- fcm(Mining.TXT.token.dfm)
Mining.TXT.textplot.Network <- fcm(Mining.TXT.token.dfm, tri = FALSE) # Create Co-occurrence Network 
feat <- names(topfeatures(Mining.TXT.textplot.Network, 50)) # Select most frequent Co-occurrence corpuses
set.seed(100)
Mining.TXT.textplot.Network <- fcm_select(Mining.TXT.textplot.Network, pattern = feat, verbose = FALSE)

# CON 그리기

textplot_network(Mining.TXT.textplot.Network,
                 vertex_labelsize = 2.5 * rowSums(Mining.TXT.textplot.Network)/
                   min(rowSums(Mining.TXT.textplot.Network)),
                 min_freq = 0.95,
                 edge_size = 1)


textplot_network(Mining.TXT.textplot.Network, min_freq = 0.95, 
                 edge_alpha = 0.8, 
                 edge_size = 0.5)



Cooccurrence.network <- convert(Mining.TXT.textplot.Network, to = "data.frame")
write_xlsx(Cooccurrence.network, "Network.xlsx")

# =======================================
# Create a Topic Model 
# =======================================
# This section creates a topic model, which is a type of statistical model for discovering the abstract "topics" 
# that occur in a collection of documents.


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


# # Optimal topic number test
# result <- FindTopicsNumber(
#   Mining.TXT.token.dfm,
#   topics = seq(from = 2, to = 15, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 77),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# 
# FindTopicsNumber_plot(result)
# 
# 




#=====================================
# Word cloud
#=====================================
# This section creates a word cloud, which is a visual representation of word frequency. 


set.seed(100)
textplot_wordcloud(Mining.TXT.token.dfm, min_count = 10,
                   color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'))




# =======================================
# New Sentiment Analysis
# =======================================
# This section performs a sentiment analysis, which is the use of natural language processing to identify 
# and extract subjective information from source materials.

#14) Quanteda Sentiment library
#remotes::install_github("quanteda/quanteda.sentiment")
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

names(Sentiment.com)


# # Convert the date column to month format
# data <- Sentiment.com %>%
#   mutate(month = floor_date(date, "month"))
# 
# # Calculate monthly averages
# monthly_averages <- data %>%
#   group_by(month) %>%
#   summarise(average_sentiment = mean(sentiment))
# 
# # View the monthly averages
# print(monthly_averages)



# Plot the Time series
# Plot the time series 1

ggplot(Sentiment.com, aes(x = date, y = sentiment)) +
  geom_line() +
  labs(x = "Date", y = "Sentiment")  # Format x-axis labels as dates


# Plot the time series 2
ggplot(Sentiment.com, aes(x = date, y = sentiment, color = sentiment >= 0)) +
  geom_line() +
  labs(x = "Date", y = "Sentiment") +
  scale_color_manual(values = c("blue", "red"), labels = c("Negative", "Positive"),
                     guide = guide_legend(title = "Sentiment"))



# Emotional Measures
#=====================================
# This section calculates emotion scores for the text data using a bag-of-words approach.
# A bag-of-words approach for computing emotions in text data using the lexicon compiled by Araque, Gatti, Staiano, and Guerini (2018).

mycorpus <- quanteda::corpus(Mining.text.corpus)

# Extract the texts from the corpus
mytexts <- as.character(mycorpus)

# Get the NRC sentiment of the texts using syuzhet
nrc_sentiment <- get_nrc_sentiment(mytexts)

# Summarize the results
summary <- colSums(nrc_sentiment)

# Convert to data frame for better viewing
df <- as.data.frame(summary)
df <- tibble::rownames_to_column(df, "emotion")
colnames(df) <- c("emotion", "count")

# Create a bar chart
ggplot(df, aes(x = emotion, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Emotion", y = "Count", title = "Emotion Counts in Text Data")