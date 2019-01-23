### EDA
summary(model[,c(8:15)])
library(ggplot2)
colnames(model)
str(model)
summary()
## all_variables
aa = read.csv('aa.csv',head=T,stringsAsFactors = F)
aaa = aa[-c(5,6,11:26),]
library(reshape2)
ggplot(aaa,aes(x = name, y=pp, fill = as.factor(pos))) + 
  geom_bar(stat = 'identity', position = 'fill')  + theme(legend.position='none')


## avrg
ggplot(model, aes(x=avrg,fill=point)) + geom_density(alpha = 0.6)
## sent_cnt
ggplot(model, aes(x=sent_cnt,fill=point)) + geom_density(alpha = 0.6)
## word_cnt
ggplot(model, aes(x=word_cnt,fill=point)) + geom_density(alpha = 0.6)
## power_cnt
ggplot(model, aes(x=power_cnt,fill=point)) + geom_bar()
## factor_cnt
ggplot(model, aes(x=factor_cnt,fill=point)) + geom_bar()
## !
ggplot(model, aes(x=`!`,fill=point)) + geom_bar()
## ?
ggplot(model, aes(x=`?`,fill=point)) + geom_bar()
## upper
ggplot(model, aes(x=upper,fill=point)) + geom_bar()
## nchar
ggplot(model, aes(x=nchar,fill=point)) + geom_density(alpha=0.6)
## I
ggplot(model, aes(x=I,fill=point)) + geom_bar()
## you
ggplot(model, aes(x=you,fill=point)) + geom_bar()
## he.she
ggplot(model, aes(x=he.she,fill=point)) + geom_density(alpha=0.6)
## not
ggplot(model, aes(x=not,fill=point)) + geom_density(alpha=0.6)
ggplot(model, aes(x=not,fill=point)) + geom_bar()


table(sound)
table(composition)
table(play)

a = data.frame(imdb$point, sound, composition, play)
str(a)
require('gridExtra')
g1=ggplot(a, aes(x=sound,fill=imdb.point)) + geom_bar() + theme(legend.position='none')
g2=ggplot(a, aes(x=composition,fill=imdb.point)) + geom_bar() + theme(legend.position='none')
g3=ggplot(a, aes(x=play,fill=imdb.point)) + geom_bar() + theme(legend.position='none')
grid.arrange(g1,g2,g3,ncol=3)

a.p = subset(a, a$imdb.point=='1')
a.n = subset(a, a$imdb.point=='0')
colSums(a.p[,2:4])
colSums(a[,2:4])
colSums(a.n[,2:4])

library(openxlsx)
a = read.xlsx('aaaaa.xlsx')
a
ggplot(a,aes(x = name, y=pp, fill = as.factor(pos))) + 
  geom_bar(stat = 'identity', position = 'fill')  + theme(legend.position='none')

####  wordcloud
library(stringr)
library(wordcloud2)
library(RColorBrewer)

a = tolower(imdb$text)
a = removeNumbers(a)
a = stripWhitespace(a)
a = removePunctuation(a)
a = removeWords(a, stopwords('en'))
Cps = VCorpus(VectorSource(a))
Tdm = TermDocumentMatrix(Cps)
#### wordcloud
termFq = rowSums(as.matrix(Tdm))
termFq = sort(termFq, decreasing = T)
termFq = subset(termFq, termFq>10)
df <- data.frame(term = names(termFq), freq = termFq)
summary(termFq)
wordcloud2(df, minRotation=0, maxRotation=0, shape='circle', ellipticity=0.50,
           color='random-light', fontFamily='나눔고딕', shuffle=F)
#### ngram
library(RWeka)
bigramTokenizer = function(x) NGramTokenizer(x, Weka_control(min=2, max=3))
ngram.tdm = TermDocumentMatrix(Cps, control = list(tokenize = bigramTokenizer))
bigramlist = apply(ngram.tdm[,],1,sum)
sort(bigramlist, decreasing=T)[1:10]

dtm = DocumentTermMatrix(Cps)
findFreqTerms(dtm,lowfreq=15)
findAssocs(dtm, 'act', corlimit = 0.2)