#install.packages("treemap")
library(treemap)

#preprocessing
imdb$text <- gsub('not|nor|nobody|nothing', 'no', imdb$text)
neg  <- c("isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't",
          "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't",
          "can't", "cannot", "couldn't", "mustn't", "no", "never", 
          "neither")

imdb$text <- dic_f(neg, rep('no',21), imdb$text)

imdb$text <- gsub('one|movie|film|will|just|can|ever', '', imdb$text)
##################전체데이터
Cps <- Corpus(VectorSource(imdb$text))
Cps <- tm_map(Cps, removePunctuation)
Cps <- tm_map(Cps, removeNumbers)
Cps <- tm_map(Cps, stripWhitespace)
Cps <- tm_map(Cps, tolower)
Cps <- tm_map(Cps, removeWords, stopwords('english'))

Tdm <- TermDocumentMatrix(Cps)
termFq <- rowSums(as.matrix(Tdm))

aa <- as.data.frame(termFq, row.names=F)
aa$term <- rownames(aa)
Termdf <- data.frame(aa$term, aa$termFq)
names(Termdf) <- c('term','freq')
#########################긍정

Cps_pos <- Corpus(VectorSource(imdb[imdb$point==1,]$text))
Cps_pos <- tm_map(Cps_pos, removePunctuation)
Cps_pos <- tm_map(Cps_pos, removeNumbers)
Cps_pos <- tm_map(Cps_pos, stripWhitespace)
Cps_pos <- tm_map(Cps_pos, tolower)
Cps_pos <- tm_map(Cps_pos, removeWords, stopwords('en'))

Tdm_pos <- TermDocumentMatrix(Cps_pos)
termFq_pos <- rowSums(as.matrix(Tdm_pos))

aa <- as.data.frame(termFq_pos, row.names=F)
aa$term <- rownames(aa)
Termdf_pos <- data.frame(aa$term, aa$termFq)
names(Termdf_pos) <- c('term','freq')

#########################부정

Cps_neg <- Corpus(VectorSource(imdb[imdb$point==0,]$text))
Cps_neg <- tm_map(Cps_neg, removePunctuation)
Cps_neg <- tm_map(Cps_neg, removeNumbers)
Cps_neg <- tm_map(Cps_neg, stripWhitespace)
Cps_neg <- tm_map(Cps_neg, tolower)
Cps_neg <- tm_map(Cps_neg, removeWords, stopwords('en'))

Tdm_neg <- TermDocumentMatrix(Cps_neg)
termFq_neg <- rowSums(as.matrix(Tdm_neg))

aa <- as.data.frame(termFq_neg, row.names=F)
aa$term <- rownames(aa)
Termdf_neg <- data.frame(aa$term, aa$termFq)
names(Termdf_neg) <- c('term','freq')

######Join
Termfq_all <- merge(Termdf, Termdf_neg, by='term', all=TRUE)
Termfq_all <- merge(Termfq_all, Termdf_pos, by='term', all=TRUE)
names(Termfq_all) <- c('term','freq_all','freq_neg','freq_pos')
Termfq_all$freq_neg[is.na(Termfq_all$freq_neg)]=0 #NA -> 0
Termfq_all$freq_pos[is.na(Termfq_all$freq_pos)]=0 #NA -> 0
Termfq_all$freq_sub <- Termfq_all$freq_neg - Termfq_all$freq_pos #for color

######treemap

#library(RColorBrewer)

#box size: freq_all, box color: freq_sub(Greys)
map <- treemap(Termfq_all[Termfq_all$freq_all>15,], title='Term freq', 
               index='term', vSize='freq_all', type='value',
               vColor='freq_sub', palette = 'Greys',  inflate.labels = TRUE)

summary(Termfq_all$freq_sub)

####termFrequency
termFq = sort(termFq, decreasing = T)
termFq = subset(termFq, termFq>30)
df <- data.frame(term = names(termFq), freq = termFq)
df <- t(df)
names(df) <- c('1')
