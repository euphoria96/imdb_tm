
neg  <- c("isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't",
              "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't",
              "can't", "cannot", "couldn't", "mustn't", "never", 
              "neither", "not", "nor", "nobody", "nothing")

imdb$text <- dic_f(neg, rep('no',24), imdb$text)

imdb$text <- gsub('one|movie|film|will|just|can|ever', '', imdb$text)

#library(RTextTools)

##전체 토픽 분석
Dtm <- create_matrix(imdb$text, language="english",
              removeStopwords=TRUE,
              removeNumbers=TRUE,
              stripWhitespace=TRUE,
              removePunctuation = TRUE,
              toLower=TRUE,
              stemWords=FALSE, tm::weightTfIdf)

lda <- LDA(Dtm, control=list(seed=171), k=3)
terms(lda, 12)
write.csv(as.data.frame(terms(lda,12)), 'lda.csv')

ctm <- CTM(Dtm, control=list(seed=171), k=3)
terms(ctm, 12)
write.csv(as.data.frame(terms(lda,12)), 'ctm.csv')

##긍정 토픽 분석
Dtm_pos <- create_matrix(imdb[imdb$point==1,]$text, language="english",
                     removeStopwords=TRUE,
                     removeNumbers=TRUE,
                     stripWhitespace=TRUE,
                     removePunctuation = TRUE,
                     toLower=TRUE,
                     stemWords=FALSE, tm::weightTfIdf)

lda_pos <- LDA(Dtm_pos, control=list(seed=50), k=3)
terms(lda_pos, 12)

ctm_pos <- CTM(Dtm_pos, control=list(seed=50), k=3)
terms(ctm_pos, 12)

##부정 토픽 분석
Dtm_neg <- create_matrix(imdb[imdb$point==0,]$text, language="english",
                         removeStopwords=TRUE,
                         removeNumbers=TRUE,
                         stripWhitespace=TRUE,
                         removePunctuation = TRUE,
                         toLower=TRUE,
                         stemWords=FALSE, tm::weightTfIdf)

lda_neg <- LDA(Dtm_neg, control=list(seed=90), k=3)
terms(lda_neg, 12)

ctm_neg <- CTM(Dtm_neg, control=list(seed=50), k=3)
terms(ctm_neg, 12)
