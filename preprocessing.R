### preprocessing
rm(list=ls());gc()

setwd("C:/Users/korea/Desktop/Ayeon/TM/code_imdb")
raw <- read.table("imdb_labelled.txt", sep='\t', quote='\n')
imdb <- raw[-c(1:5),]
names(imdb) <- c("text","point")
imdb$point = as.factor(imdb$point)
imdb$text = as.character(imdb$text)
#trim
imdb$text = gsub("^\\s+|\\s+$", "",imdb$text)

power_cnt <- str_count(imdb$text, '[[:upper:]]{2,}')
# str(imdb)
library(tidytext)
library(tm)
library(dplyr)
library(stringr)
library(tidyr)
#### 단어들 및 인코딩 전처리 -------
dic_f <- function(dic_1, dic_2, data) {
  result<-data;
  for(i in 1:length(dic_1)) {
    result <- gsub(dic_1[i], dic_2[i], result)
  }
  return(result);
}


dic_1 <- c('혯','혯','혚', '챕', '책',"scenes","sucked","sucks","wasted","things", 'points', '/10','out of 10',
           '10.10','makes','effects','characters','minutes','acting','acted', 'actors', 'liked', 'likes',
           'films','movies')

dic_2 <- c(rep('',3), 'e', 'a',"scene",rep("suck",2),"waste","thing",rep("point",3),'tenpoint',
           'make','effect','character','minute',rep('act',2) ,'actor', rep('like',2),
           'film','movie')
imdb$text <- dic_f(dic_1, dic_2, imdb$text)

cnt_dic = function(txt, dic){
  x = sum(str_count(txt, dic))
  return(x)
}