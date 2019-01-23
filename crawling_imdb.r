setwd("C:/Users/korea/Desktop/Ayeon/TM/2.3주")
rm(list=ls());gc()

library(rvest)
library(dplyr)
library(stringr)
library(beepr)

#영화코드 크롤링
url_base <- 'https://www.imdb.com/list/ls059972894/?ref_=otl_1&st_dt=&mode=detail&page='
movie_num <- NULL #영화고유번호
code <- NULL #행 전체 코드
for(page in 1:10) { #movie_num crawling
  url <- paste(url_base,page,'&sort=list_order,asc')
  url <- gsub(' ','',url)
  txt<-readLines(paste(url,page), encoding='eun-kr')
  code <- txt[which(str_detect(txt, 'lister-item-image ribbonize" data-tconst='))]
  movie_num <- c(movie_num, substr(code, 63, 71))
  
}

all.review <- NULL
all.point <- NULL
helpful_per <- NULL
all.helpful <- NULL

#library(beepr)

#영화리뷰 크롤링
for (i in 1:(length(movie_num)-1) ) { #review crawling
  url_review <- paste('https://www.imdb.com/title/',movie_num[i],'/reviews?sort=helpfulnessScore&dir=desc&ratingFilter=0')
  url_review <- gsub(' ','',url_review)
  txt_review <- readLines(url_review, encoding='euc-kr')
  
  code_point <- txt_review[which(str_detect(txt_review, '<span class="point-scale">'))]
  
  code_review <- txt_review[which(str_detect(txt_review, '<span class="point-scale">'))+10]
  code_helpful <- txt_review[which(str_detect(txt_review, '<span class="point-scale">'))+12]
  code_helpful <- gsub('[[:punct:]]','',code_helpful)
  code_helpful <- gsub('out of|found this helpful','',code_helpful)
  code_helpful[grep('[A-z]',code_helpful)] <- '0 0'
  review <- gsub('<.+?>|\t','',code_review)
  all.review <- c(all.review,str_trim(review) )
  
  point <- gsub('/10','',code_point)
  all.point <- c(all.point, gsub('\\D','',point) )
  
  helpful <- unlist(regmatches(code_helpful,gregexpr('[[:digit:]]{1,}', code_helpful)))
  all.helpful <- c(all.helpful, as.numeric(helpful) )
  
}

beep(3)

# 유용성이 10 이하면 해당 내용 삭제를 위함
for(j in 1:(length(all.helpful)/2)) {
  ifelse(all.helpful[j*2] < 10, helpful_per[j] <- 'low', helpful_per[j] <- all.helpful[j*2-1]/all.helpful[j*2])
}

dic_f <- function(dic_1, dic_2, data) {
  result<-data;
  for(i in 1:length(dic_1)) {
    result <- gsub(dic_1[i], dic_2[i], result);
  }
  return(result);
} 
#pre-processing
#grep('[가-힣]',all.review)

dic_3 <- c('&#39;', '&quot;', '혯','혯','혚', '챕','챔', '책', '채')
dic_4 <- c("'", '"', rep('',3), rep('e',2), rep('a',2))
all.review <- dic_f(dic_3, dic_4, all.review)

#리뷰+포인트+유용성
imdb_review <- data.frame(all.review,all.point,helpful_per)
#imdb_review <- imdb_review[!duplicated(imdb_review$text),]
#imdb_review <- imdb_review[c(str_count(imdb_review$all.review)!=0),]
imdb_review <- imdb_review[imdb_review$helpful_per!='low',]

names(imdb_review) <- c('text','point','helpful')
imdb_review <- imdb_review[-grep('[가-힣]',imdb_review$text),]
imdb_review <- imdb_review[-grep('[[:cntrl:]]',imdb_review$text),]
imdb_review <- imdb_review[-grep('&',imdb_review$text),]
#grep('<U+',imdb_review$text)
#imdb_review <- imdb_review[-grep('<U+',imdb_review$text),]

write.csv(imdb_review,"review_clean_10108.csv", row.names=F)
