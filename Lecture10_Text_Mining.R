#======================================================#
# Title: 'Text Mining with Quanteda for Social Science #
# Sub-theme: "GSIAS HUFS Lecture 09"                      #
# Author: "CHOI, Doo Young Wicks"                      #
# Date: 2022-10-28                                     #
#======================================================#

#====================================#
# Adding basic Library Environment   #
#====================================#

# Loading the libraries for the 'Text Mining' Process
# 1) loading and writing Text or Excel files
library(readtext)
library(readxl)
library(writexl)
library(pdftools)
# 2) Text Cleansing
library(pacman)
library(textclean)
# 3) LSE Mining Tool
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(SnowballC)
# 4) To perform "part-of-speech tagging", "named entity recognition's", dependency relationship analysis.
library(spacyr)
# 5) Stopwords library
library(stopwords)
# 6) plot library
library(ggplot2)
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
library(plyr)
library(stringr) #for controlling raw data
library(tidyr) #for convert to wide format
library(tidyverse)
library(tcltk2)
library(igraph)
library(ldatuning) #measure optimum topic
library(Rmpfr)
# 13) Create Random ID
library(ids)

#====================================
# File Loading
#====================================
# Load a source file
SourceFile <- read_excel(file.choose())
# Assign name as Text.source
Text.source <- SourceFile

# In cases of a CSV or a TXT file
# Text.source <- readtext(fileToLoad)


# TIP01: Change Cell Name, if needs
# X <- X %>% rename(text=texts)


#====================================#
# Set specific Date and category
#====================================#

# Set specific date
Text.source <- Text.source[which(Text.source$date >= as.Date("2016-01-01") & Text.source$date <= as.Date("2020-12-31")),]

# 1st text cleansing, pick the specific contents
Text.source <- Text.source %>%
               filter(str_detect(Text.source$text, "Kenya"))

# 2nd text cleansing, pick the specific contents
Text.source <- Text.source %>%
  filter(str_detect(Text.source$category, "Healthcare"))



#====================================
# Pre-processing Data
#====================================

# Preparing Corpus Target
Mining.text.corpus <- Text.source

# Processing Corpus
Mining.text.corpus <- corpus(Mining.text.corpus)
                                 
# Check Corpus Summary

summary(Mining.text.corpus)

#Exploring corpus texts
# The kwic function (keywords-in-context) performs a search for 
# a word and allows us to view the contexts in which it occurs:

kwic(Mining.text.corpus, pattern = "africa", valuetype = "regex")


#============================================#
# Pre data clensing for the Text analysis    #
# Setting personal stopwords                 #
#============================================#
# mystopwords <- arabic_stopwords
# 

mystopwords <- c ("0o", "0s", "3a", "3b", "3d", "6b", "6o", "a", "a1", "a2", "a3",
                  "a4", "ab", "able", "about", "above", "abst", "ac", "accordance",
                  "according", "accordingly", "across", "act", "actually", "ad", 
                  "added", "adj", "ae", "af", "affected", "affecting", "affects", 
                  "after", "afterwards", "ag", "again", "against", "ah", "ain", 
                  "ain't", "aj", "al", "all", "allow", "allows", "almost", "alone", 
                  "along", "already", "also", "although", "always", "am", "among", 
                  "amongst", "amoungst", "amount", "an", "and", "announce", "another",
                  "any", "anybody", "anyhow", "anymore", "anyone", "anything", "anyway",
                  "anyways", "anywhere", "ao", "ap", "apart", "apparently", "appear",
                  "appreciate", "appropriate", "approximately", "ar", "are", "aren",
                  "arent", "aren't", "arise", "around", "as", "a's", "aside", "ask",
                  "asking", "associated", "at", "au", "auth", "av", "available", "aw",
                  "away", "awfully", "ax", "ay", "az", "b", "b1", "b2", "b3", "ba",
                  "back", "bc", "bd", "be", "became", "because", "become", "becomes",
                  "becoming", "been", "before", "beforehand", "begin", "beginning",
                  "beginnings", "begins", "behind", "being", "believe", "below", "beside",
                  "besides", "best", "better", "between", "beyond", "bi", "bill", "biol",
                  "bj", "bk", "bl", "bn", "both", "bottom", "bp", "br", "brief", "briefly",
                  "bs", "bt", "bu", "but", "bx", "by", "c", "c1", "c2", "c3", "ca", "call",
                  "came", "can", "cannot", "cant", "can't", "cause", "causes", "cc", "cd",
                  "ce", "certain", "certainly", "cf", "cg", "ch", "changes", "ci", "cit",
                  "cj", "cl", "clearly", "cm", "c'mon", "cn", "co", "com", "come", "comes",
                  "con", "concerning", "consequently", "consider", "considering", "contain",
                  "containing", "contains", "corresponding", "could", "couldn", "couldnt",
                  "couldn't", "course", "cp", "cq", "cr", "cry", "cs", "c's", "ct", "cu",
                  "currently", "cv", "cx", "cy", "cz", "d", "d2", "da", "date", "dc", "dd",
                  "de", "definitely", "describe", "described", "despite", "detail", "df",
                  "di", "did", "didn", "didn't", "different", "dj", "dk", "dl", "do", "does",
                  "doesn", "doesn't", "doing", "don", "done", "don't", "down", "downwards",
                  "dp", "dr", "ds", "dt", "du", "due", "during", "dx", "dy", "e", "e2", "e3",
                  "ea", "each", "ec", "ed", "edu", "ee", "ef", "effect", "eg", "ei", "eight",
                  "eighty", "either", "ej", "el", "eleven", "else", "elsewhere", "em", "empty",
                  "en", "end", "ending", "enough", "entirely", "eo", "ep", "eq", "er", "es",
                  "especially", "est", "et", "et-al", "etc", "eu", "ev", "even", "ever", "every",
                  "everybody", "everyone", "everything", "everywhere", "ex", "exactly", "example",
                  "except", "ey", "f", "f2", "fa", "far", "fc", "few", "ff", "fi", "fifteen",
                  "fifth", "fify", "fill", "find", "fire", "first", "five", "fix", "fj", "fl",
                  "fn", "fo", "followed", "following", "follows", "for", "former", "formerly",
                  "forth", "forty", "found", "four", "fr", "from", "front", "fs", "ft", "fu",
                  "full", "further", "furthermore", "fy", "g", "ga", "gave", "ge", "get", "gets",
                  "getting", "gi", "give", "given", "gives", "giving", "gj", "gl", "go", "goes",
                  "going", "gone", "got", "gotten", "gr", "greetings", "gs", "gy", "h", "h2", "h3",
                  "had", "hadn", "hadn't", "happens", "hardly", "has", "hasn", "hasnt", "hasn't",
                  "have", "haven", "haven't", "having", "he", "hed", "he'd", "he'll", "hello",
                  "help", "hence", "her", "here", "hereafter", "hereby", "herein", "heres",
                  "here's", "hereupon", "hers", "herself", "hes", "he's", "hh", "hi", "hid", "him",
                  "himself", "his", "hither", "hj", "ho", "home", "hopefully", "how", "howbeit",
                  "however", "how's", "hr", "hs", "http", "hu", "hundred", "hy", "i", "i2", "i3",
                  "i4", "i6", "i7", "i8", "ia", "ib", "ibid", "ic", "id", "i'd", "ie", "if", "ig",
                  "ignored", "ih", "ii", "ij", "il", "i'll", "im", "i'm", "immediate", "immediately",
                  "importance", "important", "in", "inasmuch", "inc", "indeed", "index", "indicate",
                  "indicated", "indicates", "information", "inner", "insofar", "instead", "interest",
                  "into", "invention", "inward", "io", "ip", "iq", "ir", "is", "isn", "isn't", "it",
                  "itd", "it'd", "it'll", "its", "it's", "itself", "iv", "i've", "ix", "iy", "iz", "j",
                  "jj", "jr", "js", "jt", "ju", "just", "k", "ke", "keep", "keeps", "kept", "kg", "kj",
                  "km", "know", "known", "knows", "ko", "l", "l2", "la", "largely", "last", "lately",
                  "later", "latter", "latterly", "lb", "lc", "le", "least", "les", "less", "lest", "let",
                  "lets", "let's", "lf", "like", "liked", "likely", "line", "little", "lj", "ll", "ll",
                  "ln", "lo", "look", "looking", "looks", "los", "lr", "ls", "lt", "ltd", "m", "m2", "ma",
                  "made", "mainly", "make", "makes", "many", "may", "maybe", "me", "mean", "means", 
                  "meantime", "meanwhile", "merely", "mg", "might", "mightn", "mightn't", "mill", "million",
                  "mine", "miss", "ml", "mn", "mo", "more", "moreover", "most", "mostly", "move", "mr",
                  "mrs", "ms", "mt", "mu", "much", "mug", "must", "mustn", "mustn't", "my", "myself", "n",
                  "n2", "na", "name", "namely", "nay", "nc", "nd", "ne", "near", "nearly", "necessarily",
                  "necessary", "need", "needn", "needn't", "needs", "neither", "never", "nevertheless",
                  "new", "next", "ng", "ni", "nine", "ninety", "nj", "nl", "nn", "no", "nobody", "non",
                  "none", "nonetheless", "noone", "nor", "normally", "nos", "not", "noted", "nothing", "novel",
                  "now", "nowhere", "nr", "ns", "nt", "ny", "o", "oa", "ob", "obtain", "obtained", "obviously",
                  "oc", "od", "of", "off", "often", "og", "oh", "oi", "oj", "ok", "okay", "ol", "old", "om",
                  "omitted", "on", "once", "one", "ones", "only", "onto", "oo", "op", "oq", "or", "ord", "os",
                  "ot", "other", "others", "otherwise", "ou", "ought", "our", "ours", "ourselves", "out",
                  "outside", "over", "overall", "ow", "owing", "own", "ox", "oz", "p", "p1", "p2", "p3", "page",
                  "pagecount", "pages", "par", "part", "particular", "particularly", "pas", "past", "pc", "pd",
                  "pe", "per", "perhaps", "pf", "ph", "pi", "pj", "pk", "pl", "placed", "please", "plus", "pm",
                  "pn", "po", "poorly", "possible", "possibly", "potentially", "pp", "pq", "pr", "predominantly",
                  "present", "presumably", "previously", "primarily", "probably", "promptly", "proud", "provides",
                  "ps", "pt", "pu", "put", "py", "q", "qj", "qu", "que", "quickly", "quite", "qv", "r", "r2", "ra", 
                  "ran", "rather", "rc", "rd", "re", "readily", "really", "reasonably", "recent", "recently", "ref",
                  "refs", "regarding", "regardless", "regards", "related", "relatively", "research", "research-articl",
                  "respectively", "resulted", "resulting", "results", "rf", "rh", "ri", "right", "rj", "rl", "rm", "rn",
                  "ro", "rq", "rr", "rs", "rt", "ru", "run", "rv", "ry", "s", "s2", "sa", "said", "same", "saw", "say",
                  "saying", "says", "sc", "sd", "se", "sec", "second", "secondly", "section", "see", "seeing", "seem",
                  "seemed", "seeming", "seems", "seen", "self", "selves", "sensible", "sent", "serious", "seriously",
                  "seven", "several", "sf", "shall", "shan", "shan't", "she", "shed", "she'd", "she'll", "shes",
                  "she's", "should", "shouldn", "shouldn't", "should've", "show", "showed", "shown", "showns", "shows", 
                  "si", "side", "significant", "significantly", "similar", "similarly", "since", "sincere", "six", "sixty",
                  "sj", "sl", "slightly", "sm", "sn", "so", "some", "somebody", "somehow", "someone", "somethan",
                  "something", "sometime", "sometimes", "somewhat", "somewhere", "soon", "sorry", "sp", "specifically",
                  "specified", "specify", "specifying", "sq", "sr", "ss", "st", "still", "stop", "strongly", "sub",
                  "substantially", "successfully", "such", "sufficiently", "suggest", "sup", "sure", "sy", "system",
                  "sz", "t", "t1", "t2", "t3", "take", "taken", "taking", "tb", "tc", "td", "te", "tell", "ten",
                  "tends", "tf", "th", "than", "thank", "thanks", "thanx", "that", "that'll", "thats", "that's",
                  "that've", "the", "their", "theirs", "them", "themselves", "then", "thence", "there", "thereafter",
                  "thereby", "thered", "therefore", "therein", "there'll", "thereof", "therere", "theres", "there's",
                  "thereto", "thereupon", "there've", "these", "they", "theyd", "they'd", "they'll", "theyre", "they're",
                  "they've", "thickv", "thin", "think", "third", "this", "thorough", "thoroughly", "those", "thou", "though",
                  "thoughh", "thousand", "three", "throug", "through", "throughout", "thru", "thus", "ti", "til", "tip", "tj",
                  "tl", "tm", "tn", "to", "together", "too", "took", "top", "toward", "towards", "tp", "tq", "tr", "tried",
                  "tries", "truly", "try", "trying", "ts", "t's", "tt", "tv", "twelve", "twenty", "twice", "two", "tx", "u",
                  "u201d", "ue", "ui", "uj", "uk", "um", "un", "under", "unfortunately", "unless", "unlike", "unlikely",
                  "until", "unto", "uo", "up", "upon", "ups", "ur", "us", "use", "used", "useful", "usefully", "usefulness",
                  "uses", "using", "usually", "ut", "v", "va", "value", "various", "vd", "ve", "ve", "very",
                  "via", "viz", "vj", "vo", "vol", "vols", "volumtype", "vq", "vs", "vt", "vu", "w", "wa", "want",
                  "wants", "was", "wasn", "wasnt", "wasn't", "way", "we", "wed", "we'd", "welcome", "well", "we'll",
                  "well-b", "went", "were", "we're", "weren", "werent", "weren't", "we've", "what", "whatever", "what'll",
                  "whats", "what's", "when", "whence", "whenever", "when's", "where", "whereafter", "whereas", "whereby",
                  "wherein", "wheres", "where's", "whereupon", "wherever", "whether", "which", "while", "whim", "whither",
                  "who", "whod", "whoever", "whole", "who'll", "whom", "whomever", "whos", "who's", "whose", "why", "why's",
                  "wi", "widely", "will", "willing", "wish", "with", "within", "without", "wo", "won", "wonder", "wont",
                  "won't", "words", "world", "would", "wouldn", "wouldnt", "wouldn't", "www", "x", "x1", "x2", "x3", "xf",
                  "xi", "xj", "xk", "xl", "xn", "xo", "xs", "xt", "xv", "xx", "y", "y2", "yes", "yet", "yj", "yl", "you",
                  "youd", "you'd", "you'll", "your", "youre", "you're", "yours", "yourself", "yourselves", "you've", 
                  "yr", "ys", "yt", "z", "zero", "zi", "zz", "Monday","Tuesday","Wednesday","Thursday","Friday",
                  "Saturday","Sunday","January", "February", "March", "April", "May","June", "July","August","September",
                  "October", "November", "December", "Jan.", "Feb.","Mar.", "Apr.", "May.", "Jun.",
                  "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec.", "Year", "Month", "Week", "Day")


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
                           tokens_remove(stopwords("en")) %>%
                           tokens_keep(pattern = "^[a-zA-Z]+$", valuetype = "regex") %>% 
                           tokens_tolower(keep_acronyms = FALSE) %>%
                           tokens_wordstem("en") %>%
                           tokens_compound(list(c("arm", "forc"))) %>%
                           tokens_compound(list(c("air", "forc")))
  



# Different Languages Setting
# Arabic
# change||| tokens_remove(stopwords("english")) to  tokens_remove(stopwords("ar", source = "marimo"))
# reshape document to the level of paragraphs
# corp_arb <- corpus_reshape(data_corpus_udhr["arb"], to = "paragraphs")
# tokenize corpus and apply pre-processing
# toks_arb <- tokens(corp_arb, remove_punct = TRUE, remove_numbers = TRUE) %>% 
#  tokens_keep(pattern = "^[\\p{script=Arab}]+$", valuetype = "regex") %>% 
#  tokens_remove(pattern = stopwords("ar", source = "marimo"))
#  print(toks_arb[2], max_ndoc = 1, max_ntoken = -1)



# French
# change||| tokens_remove(stopwords("english")) to  tokens_remove(stopwords("fr", source = "nltk"))

# Germany 

# change||| tokens_remove(stopwords("english")) to tokens_remove(stopwords("de", source = "snowball"))

# Korean
# change||| tokens_remove(stopwords("english")) to  tokens_remove(stopwords("ko", source = "marimo"))

# Japanese
# change||| tokens_remove(stopwords("english")) to  tokens_remove(stopwords("ja", source = "marimo"))
# reshape document to the level of paragraphs
# corp <- corpus_reshape(data_corpus_udhr["jpn"], to = "paragraphs")

# tokenize corpus and apply pre-processing
# toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE, padding = TRUE) %>% 
#   tokens_remove(pattern = stopwords("ja", source = "marimo"), padding = TRUE) %>% 
#   tokens_select(pattern = "^[ぁ-んァ-ヶー一-龠]+$", valuetype = "regex", padding = TRUE)
# print(toks[2], max_ndoc = 1, max_ntok = -1)


# with other language check 
# stopwords_getlanguages("snowball")
# stopwords_getlanguages("nltk")
# stopwords_getlanguages("marimo")





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



##================================================
# Refine stopwords
#=================================================

Ref.Stopword<- c ("kenya", "health")

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

textplot_network(Mining.TXT.textplot.Network,
                 vertex_labelsize = 2.5 * rowSums(Mining.TXT.textplot.Network)/
                 min(rowSums(Mining.TXT.textplot.Network)),
                 min_freq = 0.95,
                 edge_size = 1)


#textplot_network(Mining.TXT.textplot.Network, min_freq = 0.95, 
#                 edge_alpha = 0.8, 
#                 edge_size = 0.5)




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

