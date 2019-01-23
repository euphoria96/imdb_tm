rm(list=ls());gc()
setwd('C:/Users/korea/Desktop/Ayeon/TM/2.3주')
imdb <- read.csv('review_clean_10108.csv') #crwaling data

imdb$text <- gsub("^\\s+|\\s+$", "",imdb$text) #trim
imdb$text <- gsub('[[:space:]]{1,}',' ',imdb$text) #trim
imdb$text <- as.character(imdb$text)
##################power_cnt
power_cnt <- str_count(imdb$text, '[[:upper:]]{2,}')
##################
imdb$text <- tolower(imdb$text)

dic_f <- function(dic_1, dic_2, data) {
  result<-data;
  for(i in 1:length(dic_1)) {
    result <- gsub(dic_1[i], dic_2[i], result);
  }
  return(result);
} 
##################preprocessing
dic_1 <- c("scenes","sucked","sucks","wasted","things", 'points', '/10','out of 10',
           '10.10','makes','effects','characters','minutes','acting','acted', 'actors', 'liked', 'likes',
           'films','movies')

dic_2 <- c("scene",rep("suck",2),"waste","thing",rep("point",3),'tenpoint',
           'make','effect','character','minute',rep('act',2) ,'actor', rep('like',2),
           'film','movie')
imdb$text <- dic_f(dic_1, dic_2, imdb$text)

#encoding error
dic_3 <- c('혯','혯','혚', '챕', '책')
dic_4 <- c(rep('',3), 'e', 'a')
imdb$text <- dic_f(dic_3, dic_4, imdb$text)

#neg
neg  <- c("isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't",
          "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't",
          "can't", "cannot", "couldn't", "mustn't", "never", 
          "neither", "not", "nor", "nobody", "nothing")

imdb$text <- dic_f(neg, rep('no',24), imdb$text)

imdb$text <- gsub('one|movie|film|will|just|can|ever', '', imdb$text)
library(tidytext)
library(tm)
library(dplyr)
library(stringr)
library(tidyr)
####

cnt_dic = function(txt, dic){
  x = sum(str_count(txt, dic))
  return(x)
}
#######################sent_cnt
afinn <- get_sentiments("afinn")
afinn <- Corpus(VectorSource(afinn))
afinn <- TermDocumentMatrix(afinn)
afinn <- as.vector(rownames(afinn))
afinn <- stemDocument(afinn)
afinn <- unique(afinn)

sent_cnt <- sapply(imdb$text, cnt_dic, afinn)
#######################factpr_cnt
### 1. sound ----------------
# sound 관련 단어 사용(음악의 음향)
dic1 <-  c("sound", "audio", "accent", "harmony", "melody", "music", "noise", "note", "tone", "vibration", "voice", "din", "intonation", "loudness", "modulation", "pitch", "racket", "report", "resonance", "reverberation", "ringing", "softness", "sonority", "sonorousness", "static", "tenor", "tonality", "sonance", "sonancy")
sound = sapply(imdb$text, cnt_dic, dic1)

### 2. plot ----------------
# plot 관련 단어 사용(영화 구성, 줄거리)
dic2 <- c("plot", "action", "design", "movement", "narrative", "scenario", "scene", "scheme", "story", "structure", "theme", "development", "enactment", "events", "incidents", "outline", "picture", "progress", "subject", "suspense", "thread", "unfolding")
composition <- sapply(imdb$text, cnt_dic, dic2)

### 3. play --------------
dic3 =  c("act","enact", "perform", "play", "portray", "burlesque", "characterize", "dramatize", "emote", "feign", "ham", "impersonate", "mime", "mimic", "mug", "parody", "personate", "personify", "pretend", "rehearse", "represent", "simulate", "star", "stooge", "strut", "be on", "bring down the house", "do a turn", "go on", "go over", "ham it up", "lay an egg", "make debut",
          "play act", "portrait", "play gig", "play part", "play role", "put it over", "say one's piece", "take part", "tread the boards", "actor", "actors", "actress", "actresses", "artist", "character", "clown", "comedian", "entertainer", "performer", "player", "star", "villain", "amateur", "barnstormer", "foil", "ham", "headliner", "idol", "impersonator", "lead", "mime", "mimic", "pantomimist", "play-actor", "soubrette", "stand-in", "stooge", "thespian",
          "trouper", "role", "understudy", "ventriloquist", "walk-on", "bit player", "hambone", "ingenue", "straight person", "thesp")
play = sapply(imdb$text, cnt_dic, dic3)

factor_cnt <- sound+composition+play

##############word_cnt
word_cnt = sapply(imdb$text, cnt_dic, '[[:space:]]{1,}')+1

##############
data <- cbind(imdb,word_cnt,factor_cnt,sent_cnt, power_cnt)

#multiple linear regression
attach(data)
reg <- lm(formula = helpful ~ point + word_cnt + factor_cnt + sent_cnt + power_cnt)
summary(reg)

#stepwise regression
stp_reg <- step(reg)
summary(stp_reg)
