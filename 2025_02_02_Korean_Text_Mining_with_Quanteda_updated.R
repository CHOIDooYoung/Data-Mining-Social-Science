#======================================================#
# Title: 'Korean Text Mining Updated Version           #
# Sub-theme: "한국어 조사 제거"                        #
# Author: "CHOI, Doo Young Wicks"                      #
# Date: 2025-02-2                                     #
#======================================================#

# 1) 텍스트 및 엑셀 파일 입출력을 위한 라이브러리 로딩
library(readtext)      # 다양한 형식의 텍스트 데이터를 읽기 위한 패키지
library(readxl)        # 엑셀 파일 읽기
library(writexl)       # 엑셀 파일 쓰기
library(xlsx)

# 2) 텍스트 전처리
library(textclean)

# 3) 텍스트 마이닝 관련 라이브러리
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(SnowballC)
library(tidytext)

# 4) 데이터 사이언스 워크플로우를 위한 tidyverse 패키지 모음
library(tidyverse)

# 5) 불용어 처리 라이브러리
library(stopwords)

# 6) 시각화 관련 라이브러리
library(ggplot2)
library(igraph)
library(ggraph)

# 7) 토픽 모델링 관련 라이브러리
library(topicmodels)
library(seededlda)

# 8) 샘플링 라이브러리
library(rsample)

# 9) 추가 폰트 관련 라이브러리
library(extrafont)

# 10) 날짜 처리 라이브러리
library(lubridate)

# 11) 감성 분석 관련 라이브러리
library(sentometrics)

# 12) 테이블 조작 라이브러리
# (필요시 추가)

#=============================================================================
# 맥 환경일 경우, 아래 주석 해제 후 사용 (코-발생 네트워크 시각화 등)
# install.packages("showtext")
# library(showtext)
# font_add("AppleGothic", "/System/Library/Fonts/Supplemental/AppleGothic.ttf")
# showtext_auto()
#==============================================================================

# File Loading
SourceFile <- read_excel(file.choose())
Text.source <- SourceFile

# (필요한 경우, 채널명으로 데이터 필터링

# 채널명으로 데이터 필터링하는 함수
# 채널명으로 Text.source 데이터 필터링하는 함수
filter_by_channel <- function(data, channel_name) {
  filtered_data <- data %>% filter(source == channel_name)
  return(filtered_data)
}

# 예제: 'Joe튜브' 채널 데이터 필터링
Text.source <- filter_by_channel(Text.source, "빠니보틀")

# 결과 확인
print(Text.source)

# Corpus 생성
Mining.text.corpus <- corpus(Text.source)

# --- 토크나이징 후 접미사 조사 제거를 위한 사전 정의 ---
particles <- c("은", "는", "이", "가", "을", "를", "에", "와", "과", "으로", "에게", 
               "로", "에서", "의", "인", "한", "마냥", "더러", "라서", "에서부터",
               "처럼", "가라", "같이", "고", "로써", "만치", "만큼", "랑", "로부터",
               "로서", "로써", "보고", "서", "에게서", "으로부터", "으로서", "이랑", 
               "하고", "께서", "아", "야", "이시여", "이여","은데", "는데") 

remove_particle_suffix <- function(toks, particles) {
  # 토큰의 끝에 particles 목록에 포함된 조사가 있으면 제거하는 정규표현식 생성
  pattern <- paste0("(", paste(particles, collapse = "|"), ")$")
  # tokens 객체는 직접 수정할 수 없으므로 리스트로 변환하여 처리
  toks_list <- as.list(toks)
  for (i in seq_along(toks_list)) {
    modified <- gsub(pattern, "", toks_list[[i]], perl = TRUE)
    # 제거 후 빈 토큰("")는 삭제
    toks_list[[i]] <- modified[modified != ""]
  }
  tokens(toks_list)
}

#============================================
# Pre data cleansing for the Text analysis
# 개인 불용어 파일 로딩 (엑셀 파일로부터 불용어 목록 읽기)
mystopwords <- read_excel(file.choose())
mystopwords <- mystopwords$`Korean Stowords`

# Tokenizing 단계 (여러 옵션 적용)
Mining.TXT.token <- tokens(Mining.text.corpus, what = "word",
                           remove_numbers = TRUE, 
                           remove_punct = TRUE,
                           remove_symbols = TRUE, 
                           split_hyphens = TRUE,
                           remove_url = TRUE) %>% 
  tokens_remove(pattern = mystopwords, valuetype = 'fixed', padding = FALSE) %>%
  tokens_select(pattern = "^\\s*$", selection = "remove", valuetype = "regex") %>%
  tokens_select(pattern = "^.$", selection = "remove", valuetype = "regex") %>%
  tokens_remove(stopwords("ko", source = "marimo")) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_compound(list(c("아예", "없어요"))) %>%
  tokens_compound(list(c("보면", "좋아요")))

# 토크나이징 후 접미사 조사 제거 적용
Mining.TXT.token <- remove_particle_suffix(Mining.TXT.token, particles)

# 이메일, 웹사이트 등 불필요한 패턴 제거
Mining.TXT.token <- tokens_remove(Mining.TXT.token, 
                                  "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+",
                                  valuetype = 'regex', padding = TRUE)
Mining.TXT.token <- tokens_remove(Mining.TXT.token, 
                                  "[a-zA-Z0-9_.+-]+\\.[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+",
                                  valuetype = 'regex', padding = TRUE)
Mining.TXT.token <- tokens_remove(Mining.TXT.token, 
                                  "[a-zA-Z0-9_.+-]+\\.[a-zA-Z0-9-]+\\.[com]+",
                                  valuetype = 'regex', padding = TRUE)
Mining.TXT.token <- tokens_remove(Mining.TXT.token, 
                                  "[a-zA-Z0-9-]+\\.[com]+",
                                  valuetype = 'regex', padding = TRUE)

#--- 동의어 통합 (Lemmatization) ---

synonyms.eat <-  c("먹", "먹어")
lemma.eat <- rep("먹다", length(synonyms.eat))

Mining.TXT.token <- tokens_replace(Mining.TXT.token, synonyms.eat, lemma.eat, valuetype = "fixed")

synonyms.cairo <-  c("카이")
lemma.cairo <- rep("카이로", length(synonyms.cairo))

Mining.TXT.token <- tokens_replace(Mining.TXT.token, synonyms.cairo, lemma.cairo, valuetype = "fixed")

synonyms.people <-  c("사람", "사람들")
lemma.people <- rep("사람", length(synonyms.people))

Mining.TXT.token <- tokens_replace(Mining.TXT.token, synonyms.people, lemma.people, valuetype = "fixed")

# 동의어 통합 후, 한 글자만 포함된 토큰 제거
Mining.TXT.token <- tokens_select(Mining.TXT.token, pattern = "^.$", 
                                  selection = "remove", valuetype = "regex")


#--- 사용자 정의 불용어 제거 (추가) ---
Ref.Stopword <- c("지금", "진짜", "이제", "여기", "오늘", "이게","그냥",
                  "같아요", "있어", "한번", "가지", "되게", "yeah", "없어",
                  "정말", "엄청", "정도", "보니까", "우리", "시간", 
                  "하겠습니다", "어제", "보면", "아까", "아무튼", "음악", 
                  "music", "들어", "생각", "원래", "아마", "기다리", "그게",
                  "절대", "있어요", "완전", "웃음", "얘기", "같습니다", "like",
                  "아직", "이걸", "빨리", "거에요","내일", "없다", "있다", 
                  "woo", "있을까", "거예요", "항상", "가장", "사실", "여기도", 
                  "분들", "아예", "드디어", "저희", "어디", "하루", "같아", 
                  "봐요", "마음", "올라", "oo", "없어요", "봐요", "자전거",
                  "것도", "가면", "있네요", "이거", "인데", "거죠", "안녕하세요",
                  "이건", "일단", "처음", "있죠", "땡큐", "되요", "그렇죠", 
                  "제일", "나오", "했습니다", "건데", "솔직히", "오케", "몰라",
                  "ok", "거지", "저도", "넘어", "그걸", "oh", "이미", "거기", 
                  "돼요", "하니까", "can", "되어","실제", "제대", "당시",
                  "오면", "경우", "know", "왔습니다", "있지", "만원", "갔다",
                  "괜찮", "갑자기", "되지", "확실히", "감사합니다", "먼저",
                  "여기까지", "여긴", "먼저", "달라", "하게", "뭐라", "않아요",
                  "봤어", "직접", "지나", "그런지", "하네요", "안녕", "이번",
                  "말도", "보통", "저거", "거라","시킨"
                  )
Mining.TXT.token <- tokens_remove(Mining.TXT.token, pattern = Ref.Stopword, valuetype = "fixed")

#======================================================
# 문서 기능 행렬 (dfm) 생성: 'bag-of-words' 접근법 사용
#======================================================
Mining.TXT.token.dfm <- dfm(Mining.TXT.token)
# 출현 빈도가 5번 미만인 용어 제거
Mining.TXT.token.dfm <- dfm_trim(Mining.TXT.token.dfm, min_termfreq = 5)

#======================================================
# 빈도 분석 및 시각화
#======================================================
Mining.TXT.token.dfm.freq <- textstat_frequency(Mining.TXT.token.dfm, n = 50)
Mining.TXT.token.dfm.freq$feature <- with(Mining.TXT.token.dfm.freq, reorder(feature, -frequency))

ggplot(Mining.TXT.token.dfm.freq, aes(x = reorder(feature, -frequency), y = frequency)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Feature", y = "Frequency")

head(Mining.TXT.token.dfm.freq, 50)

# =======================================
# Create Frequency Graph 2
# =======================================
Mining.TXT.token.dfm.inaug <- textstat_frequency(Mining.TXT.token.dfm, n = 50)
Mining.TXT.token.dfm.inaug$feature <- with(Mining.TXT.token.dfm.inaug, reorder(feature, -frequency))

ggplot(Mining.TXT.token.dfm.inaug, aes(x = feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

frequency.table <- Mining.TXT.token.dfm.inaug
total.words <- sum(frequency.table$frequency)
frequency.table$proportion <- (frequency.table$frequency / total.words) * 100

print(frequency.table)

ggplot(frequency.table, aes(x = feature, y = proportion)) +
  geom_point() +
  labs(y = "Proportion (%)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#======================================================
# 워드 클라우드 생성
#======================================================
set.seed(100)
textplot_wordcloud(Mining.TXT.token.dfm, 
                   min_count = 10,  
                   color = c('red', 'pink', 'green', 'purple', 'orange', 'blue'),
                   min_size = 0.5,  
                   max_size = 4,    
                   random_order = FALSE)

#======================================================
# 공기어 행렬 및 네트워크 분석
#======================================================
Mining.TXT.textplot.Network <- fcm(Mining.TXT.token)
top_feat <- names(sort(colSums(Mining.TXT.textplot.Network), decreasing = TRUE)[1:30])
fcm_selected <- fcm_select(Mining.TXT.textplot.Network, pattern = top_feat)

textplot_network(fcm_selected, min_freq = 0.95, max.overlaps = 50)

#======================================================
# 토픽 모델링
#======================================================
lda_model <- textmodel_lda(Mining.TXT.token.dfm, k = 3)  # 3개의 토픽 생성
top_terms_per_topic <- terms(lda_model, 4)  # 각 토픽별 상위 4개 용어 출력
print(top_terms_per_topic)

#======================================================
# 감성 분석 (Sentiment Analysis)
#======================================================
remotes::install_github("quanteda/quanteda.sentiment")
library(quanteda.sentiment)
data("data_dictionary_LoughranMcDonald", package = "quanteda.sentiment")
print(data_dictionary_LoughranMcDonald, max_nval = 8)

Sentiment.com <- Mining.text.corpus %>% 
  textstat_polarity(dictionary = data_dictionary_LoughranMcDonald)
print(Sentiment.com)

Sentiment.date <- data.frame(Text.source$date) %>%
  rename(date = Text.source.date)

Sentiment.com <- subset(Sentiment.com, select = - c(doc_id))
Sentiment.com <- cbind.data.frame(Sentiment.date, Sentiment.com)

theme_set(theme_grey(base_family="Arial"))

ggplot(Sentiment.com, aes(x = date, y = sentiment)) +
  geom_line() +
  labs(x = "Date", y = "Sentiment") +
  theme(text = element_text(family = "Arial"))

ggplot(Sentiment.com, aes(x = date, y = sentiment, color = sentiment >= 0)) +
  geom_line() +
  labs(x = "Date", y = "Sentiment") +
  scale_color_manual(values = c("blue", "red"), labels = c("Negative", "Positive"),
                     guide = guide_legend(title = "Sentiment")) +
  theme(text = element_text(family = "Arial"))

#======================================================
# 감정 분석 (Emotion Analysis)
#======================================================
library(syuzhet)
mycorpus <- corpus(Mining.text.corpus)
nrc_sentiment <- get_nrc_sentiment(mycorpus)
summary_nrc <- colSums(nrc_sentiment)

df_nrc <- as.data.frame(summary_nrc)
df_nrc <- tibble::rownames_to_column(df_nrc, "emotion")
colnames(df_nrc) <- c("emotion", "count")

total_count <- sum(df_nrc$count)
df_nrc$percent <- (df_nrc$count / total_count) * 100

negative_emotions <- c("anger", "disgust", "fear", "sadness")
positive_emotions <- c("anticipation", "surprise")

negative_df <- df_nrc[df_nrc$emotion %in% negative_emotions, ]
positive_df <- df_nrc[df_nrc$emotion %in% positive_emotions, ]

negative_df$group <- "Negative Emotions"
positive_df$group <- "Positive Emotions"

merged_df <- rbind(negative_df, positive_df)

ggplot(merged_df, aes(x = group, y = percent, fill = emotion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = sprintf("%.1f%%", percent)), vjust = -0.5, position = position_dodge(width = 0.8)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(text = element_text(family = "Arial")) +
  labs(x = "Emotion Group", y = "Frequency (%)", title = "Emotion Frequency in Text Data") +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "brown", "grey"))
