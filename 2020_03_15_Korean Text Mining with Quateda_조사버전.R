#======================================================#
# Title: 'Korean Text Mining Updated Version           #
# Sub-theme: "한국어 조사 제거"                        #
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

#=============================================================================
# 맥 환경일 때만 01 -> 아래  Create Co-occurrences Network 부분에도 활성화
# install.packages("showtext")
 library(showtext)
 font_add("AppleGothic", "/System/Library/Fonts/Supplemental/AppleGothic.ttf")
 showtext_auto()
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
# 주제격 조사 (Subject markers)
particles_subject <- c("은", "는", "이", "가") 
# 주제격 조사는 문장에서 주어를 나타냅니다.

# 목적격 조사 (Object markers)
particles_object <- c("을", "를") 
# 목적격 조사는 문장에서 목적어를 나타냅니다.

# 부사격 조사 (Adverbial markers)
particles_adverbial <- c("에", "에서", "에서부터", "으로", "로", "으로부터", "으로서", "로서", "로써", "만큼", "처럼", "마냥", "랑", "하고", "이랑", "같이", "과", "와", "더러", "서", "보고", "인", "한", "로부터") 
# 부사격 조사는 문장에서 부사어를 나타냅니다.

# 격조사 (Case markers)
particles_case <- c("의", "에게", "께서", "아", "야", "이시여", "이여", "에게서") 
# 격조사는 문장에서 목적어나 수식어를 나타내며, 인칭이나 존칭을 표현할 수 있습니다.

# 어미 (Endings)
particles_endings <- c("라서", "고", "가라", "만치", "로써")
# 어미는 문장에서 어근 뒤에 붙어 동사, 형용사의 활용을 나타냅니다.

# 각 성격별 조사를 가나다 순으로 정렬합니다.
particles_subject_sorted <- sort(particles_subject)
particles_object_sorted <- sort(particles_object)
particles_adverbial_sorted <- sort(particles_adverbial)
particles_case_sorted <- sort(particles_case)
particles_endings_sorted <- sort(particles_endings)

# 정렬된 조사를 하나의 조사 사전으로 합칩니다.
particles_dictionary <- c(particles_subject_sorted, particles_object_sorted, particles_adverbial_sorted, particles_case_sorted, particles_endings_sorted)

# 결과를 출력합니다.
print(particles_dictionary)


# 조사를 제거

remove_particles_quanteda <- function(text, particles_dictionary) {
  pattern <- paste0("(?<=\\S)(", paste(particles_dictionary, collapse = "|"), ")(?=\\s|$)")
  tokens <- tokens(text, remove_punct = TRUE)
  tokens <- tokens_remove(tokens, pattern, valuetype = "regex")
  return(tokens)
}


# Preparing Corpus Target
Mining.text.corpus <- Text.source


# Processing Corpus
Mining.text.corpus <- corpus(Mining.text.corpus)


# 조부사 제거
Mining.text.corpus <- remove_particles_quanteda(Mining.text.corpus, particles_dictionary)
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

mystopwords <- c(K_stopwords$K_Stoword2)


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

synonyms.president <-  c("대통령", "대통령실", "윤석열")
lemma.president <- rep("대통령", length(synonyms.president))
# 
# synonyms.produit <-  c("produit", "produits", "production")
# lemma.produit <- rep("produit", length(synonyms.produit))
# 
# synonyms.agricole <-  c("agricole", "agricoles", "agriculteurs")
# lemma.agricole <- rep("agricole", length(synonyms.agricole))
# 
# 
# # Replace synonyms in tokens
Mining.TXT.token <- tokens_replace(Mining.TXT.token, synonyms.president, lemma.president, valuetype = "fixed")
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
                       min_freq = 0.99,
                       edge_size = 1)



#=================================================================================
#Set the font for the graph on MacOS environment

Coocurrence.Network <- Coocurrence.Network +
  theme(plot.title = element_text(family = "AppleGothic"),
        axis.title = element_text(family = "AppleGothic"),
        axis.text = element_text(family = "AppleGothic"),
        legend.title = element_text(family = "AppleGothic"),
        legend.text = element_text(family = "AppleGothic"))
#================================================================================



print(Coocurrence.Network)



# write.xlsx(Mining.TXT.textplot.Network, "networkname.xlsx")
# =======================================
# Create a Topic Model 
# =======================================

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
# 

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
