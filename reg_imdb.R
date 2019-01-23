###### A3 길이 ---------
word_cnt = sapply(imdb$text, cnt_dic, '[[:space:]]{1,}')
# word_cnt <- sapply(imdb$text, cnt_dic, ' ')+1
###### A4 강조 -----------
power_cnt <- str_count(imdb$text, '[[:upper:]]{2,}')
###### A2 감정 -----------
txt = tolower(imdb$text)
txt = gsub("^\\s+|\\s+$", "", txt)   ## 
txt = gsub('[[:space:]]{1,}', ' ', txt) ## 

afinn <- get_sentiments("afinn")
afinn <- Corpus(VectorSource(afinn))
afinn <- TermDocumentMatrix(afinn)
afinn <- as.vector(rownames(afinn))
afinn <- stemDocument(afinn)
afinn <- unique(afinn)

sent_cnt <- sapply(txt, cnt_dic, afinn)
###### A5 설명 -----------
### 1. sound 
# sound 관련 단어 사용(음악의 음향)
dic1 <-  c("sound", "audio", "accent", "harmony", "melody", "music", "noise", "note", "tone", "vibration", "voice", "din", "intonation", "loudness", "modulation", "pitch", "racket", "report", "resonance", "reverberation", "ringing", "softness", "sonority", "sonorousness", "static", "tenor", "tonality", "sonance", "sonancy")
sound = sapply(imdb$text, cnt_dic, dic1)
# sound = ifelse(sound>0,1,0)

### 2. plot / composition 
# plot 관련 단어 사용(영화 구성, 줄거리)
dic2 <- c("plot", "action", "direct", "composition", "design", "movement", "visual", "narrative", "graphic", "scenario", "scene", "scheme", "story", "structure", "theme", "development", "enactment", "events", "incidents", "outline", "picture", "progress", "subject", "suspense", "thread", "unfolding")
composition = sapply(imdb$text, cnt_dic, dic2)
# plot = ifelse(plot>0,1,0)
### 3. play 
dic3 =  c("act", "perform", "play", "portray", "burlesque", "characterize", "dramatize", "emote", "feign", "ham", "impersonate", "mime", "mimic", "mug", "parody", "personate", "personify", "pretend", "rehearse", "represent", "simulate", "star", "stooge", "strut", "be on", "bring down the house", "do a turn", "go on", "go over", "ham it up", "lay an egg", "make debut",
          "portrait", "put it over", "say one's piece", "take part", "tread the boards", "character", "clown", "comedian", "entertainer", "performer", "star", "villain", "amateur", "barnstormer", "foil", "ham", "headliner", "idol", "impersonator", "lead", "mime", "mimic", "pantomimist", "soubrette", "stand-in", "stooge", "thespian",
          "trouper", "role", "understudy", "ventriloquist", "walk-on", "bit player", "hambone", "ingenue", "straight person", "thesp")
play = sapply(imdb$text, cnt_dic, dic3)
# play = ifelse(play>0,1,0)

factor_cnt <- sound+composition+play
###### A1 -----------
library(RTextTools)
#memory.limit()
#memory.limit(16000)
mat = create_matrix(imdb$text, language = 'english',
                    removeStopwords = T, removeNumbers = T,
                    stripWhitespace = T, removePunctuation = T,
                    toLower = T, stemWords = F, tm::weightTfIdf) 
mat = as.matrix(mat)

x=1:1000
gc()
imdb[imdb$point<5,]$point <- 0
imdb[imdb$point>=5,]$point <- 1


for(i in 1:10) {
  idx = c(1:100)+(i-1)*100
  smp = sample(1:1000,300)
  container = create_container(mat, as.numeric(imdb$point), trainSize = x[-smp],
                               testSize = smp, virgin = F)
  myclassifier = train_models(container, algorithms = c("SVM", "GLMNET", "SLDA","TREE",
                                                        "BAGGING","RF","MAXENT","BOOSTING"))
  myresult = classify_models(container, myclassifier)
  y = cbind(as.numeric(myresult$SVM_LABEL), as.numeric(myresult$GLMNET_LABEL), as.numeric(myresult$SLDA_LABEL), as.numeric(myresult$TREE_LABEL),
            as.numeric(myresult$BAGGING_LABEL),as.numeric(myresult$FORESTS_LABEL), as.numeric(myresult$MAXENTROPY_LABEL), as.numeric(myresult$LOGITBOOST_LABEL))
  z = apply(y, 1, mean)-1
  avrg[idx] = z #avrg: average of ML return value
beep(sound=3)
}

imdb$avrg <- avrg
table(imdb$point, imdb$avrg)
######

summary(imdb$helpful)

test <- cbind(imdb,word_cnt,factor_cnt,sent_cnt, power_cnt)


# helpful
test$helpful <- 4.964e-01 + 1.958e-02 * aa$x + 1.599e-04 * word_cnt - 3.501e-04*sent_cnt - 9.983e-04 * factor_cnt + 4.669e-04*power_cnt

summary(helpful)
