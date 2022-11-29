
## 패키지 불러오기
library(rvest)
library(dplyr)

## 변수 입력하기
QUERY <- "이슬람+무슬" # 검색키워드
DATE  <- as.Date(as.character(20101218),format="%Y%m%d") # 검색시작날짜 & 검색종료날짜
DATE <- format(DATE, "%Y.%m.%d")
PAGE  <- 1

naver_url_1 <- "https://search.naver.com/search.naver?&where=news&query="
naver_url_2 <- "&pd=3&ds="
naver_url_3 <- "&de="
naver_url_4 <- "&start="

## 날짜 리스트 만들기
DATE_START <- as.Date(as.character(20101218),format="%Y%m%d") # 시작일자
DATE_END   <- as.Date(as.character(20191231),  format="%Y%m%d") # 종료일자
DATE <- DATE_START:DATE_END
DATE <- as.Date(DATE,origin="1970-01-01")

## 네이버 검색결과 url 리스트에서 관련기사 url 리스트 만들기
news_url <- c()
news_date <-c() 

for (date_i in DATE){
  for (page_i in PAGE){
    dt <- format(as.Date(date_i,origin="1970-01-01"), "%Y.%m.%d")
    naver_url <- paste0(naver_url_1,QUERY,naver_url_2,dt,naver_url_3,dt,naver_url_4,page_i)
    html <- read_html(naver_url)
    temp <- unique(html_nodes(html,'#main_pack')%>% # id= 는 # 을 붙인다
                     html_nodes(css='.news ')%>%    # class= 는 css= 를 붙인다 
                     html_nodes(css='.type01')%>%
                     html_nodes('a')%>%
                     html_attr('href'))
    news_url <- c(news_url,temp)
    news_date <- c(news_date,rep(dt,length(temp)))
  }
  # print(dt) # 진행상황을 알기 위함이니 속도가 느려지면 제외
}

NEWS0 <- as.data.frame(cbind(date=news_date, url=news_url, query=QUERY))
NEWS1 <- NEWS0[which(grepl("news.naver.com",NEWS0$url)),]         # 네이버뉴스(news.naver.com)만 대상으로 한다   
NEWS1 <- NEWS1[which(!grepl("sports.news.naver.com",NEWS1$url)),] # 스포츠뉴스(sports.news.naver.com)는 제외한다  
NEWS2 <- NEWS1[!duplicated(NEWS1), ] # 중복된 링크 제거 


## 뉴스 페이지에 있는 기사의 제목과 본문을 크롤링
NEWS2$news_title   <- ""
NEWS2$news_content <- ""

for (i in 1:dim(NEWS2)[1]){
  html <- read_html(as.character(NEWS2$url[i]))
  temp_news_title   <- repair_encoding(html_text(html_nodes(html,'#articleTitle')),from = 'utf-8')
  temp_news_content <- repair_encoding(html_text(html_nodes(html,'#articleBodyContents')),from = 'utf-8')
  if (length(temp_news_title)>0){
    NEWS2$news_title[i]   <- temp_news_title
    NEWS2$news_content[i] <- temp_news_content
  }
}

NEWS2$news_content <- gsub("// flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()", "", NEWS2$news_content)
NEWS <- NEWS2 # 최종 결과 저장

save(NEWS, file="./DATA0/NEWS.RData") # working directory 아래 subfolder "DATA0" 에 저장