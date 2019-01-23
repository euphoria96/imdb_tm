rm(list=ls());gc()
setwd('C:/Users/korea/Desktop/Ayeon/TM/2.3주')
imdb <- read.table("imdb_labelled.txt", sep='\t', quote='\n')
imdb <- imdb[-c(1:5),]
names(imdb) <- c('text','point')

imdb$point = factor(imdb$point, levels = c('0','1'), labels = c('Neg','Pos'))

## 여기서부턴 Neg는 1 Pos는 2 ######
library(stringr)
# 1. !
a1 = str_count(imdb$text, '!')
table(a1)

# 2. ?
a2 = str_count(imdb$text, '\\?')
table(a2)

# 3. power_cnt
power_cnt = str_count(imdb$text, '[[:upper:]]{2,}')
table(power_cnt)

cnt_dic = function(txt, dic){
  x = sum(str_count(txt, dic))
  return(x)
}

# 4. sent_cnt
imdb$text <- tolower(imdb$text)
imdb$text <- gsub("^\\s+|\\s+$", "",imdb$text)
imdb$text <- gsub('[[:space:]]{1,}', ' ',imdb$text) 

afinn <- get_sentiments("afinn")
afinn <- Corpus(VectorSource(afinn))
afinn <- TermDocumentMatrix(afinn)
afinn <- as.vector(rownames(afinn))
afinn <- stemDocument(afinn)
afinn <- unique(afinn)

sent_cnt <- sapply(imdb$text, cnt_dic, afinn)
table(sent_cnt)

# 5. word_cnt
word_cnt <- sapply(imdb$text, cnt_dic, '[[:space:]]{1,}')+1
table(word_cnt)

# 6. nchar
imdb$text <- as.character(imdb$text)
a6 <- nchar(imdb$text)
table(a6)

# 7. factor_cnt
# sound 관련 단어 사용(음악의 음향)
dic1 <-  c("sound", "audio", "accent", "harmony", "melody", "music", "noise", "note", "tone", "vibration", "voice", "din", "intonation", "loudness", "modulation", "pitch", "racket", "report", "resonance", "reverberation", "ringing", "softness", "sonority", "sonorousness", "static", "tenor", "tonality", "sonance", "sonancy")
a7 <- sapply(imdb$text, cnt_dic, dic1)

# 8. composition
dic2 <- c("plot", "action", "direct", "composition", "design", "movement", "visual", "narrative", "graphic", "scenario", "scene", "scheme", "story", "structure", "theme", "development", "enactment", "events", "incidents", "outline", "picture", "progress", "subject", "suspense", "thread", "unfolding")
a8 <- sapply(imdb$text, cnt_dic, dic2)

# 9. play
dic3 <- c("act", "perform", "play", "portray", "burlesque", "characterize", "dramatize", "emote", "feign", "ham", "impersonate", "mime", "mimic", "mug", "parody", "personate", "personify", "pretend", "rehearse", "represent", "simulate", "star", "stooge", "strut", "be on", "bring down the house", "do a turn", "go on", "go over", "ham it up", "lay an egg", "make debut",
          "portrait", "put it over", "say one's piece", "take part", "tread the boards", "character", "clown", "comedian", "entertainer", "performer", "star", "villain", "amateur", "barnstormer", "foil", "ham", "headliner", "idol", "impersonator", "lead", "mime", "mimic", "pantomimist", "soubrette", "stand-in", "stooge", "thespian",
          "trouper", "role", "understudy", "ventriloquist", "walk-on", "bit player", "hambone", "ingenue", "straight person", "thesp")
a9 <- sapply(imdb$text, cnt_dic, dic3)

factor_cnt <- a7 + a8 + a9
# 10. i
I <- sapply(imdb$text, cnt_dic, '\\Wi\\W')
table(I)

# 11. you
you <- sapply(imdb$text, cnt_dic, 'you')
table(you)

# 12. he/she
he.she <- sapply(imdb$text, cnt_dic, 'he|she')
table(he.she)

# 13. not
imdb$text <- gsub('not|nor|nobody', 'no', imdb$text)
neg = c("isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't",
        "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't",
        "can't", "cannot", "couldn't", "mustn't", "no", "never", 
        "neither", "nobody")
not <- sapply(imdb$text, cnt_dic, neg)
table(not)
