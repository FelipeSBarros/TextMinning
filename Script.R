rm(list=ls())
# Script from http://neylsoncrepalde.github.io/2016-03-18-analise-de-conteudo-twitter/
# http://neylsoncrepalde.github.io/2017-03-20-nuvens-de-palavras-dinamicas/
install.packages('twitteR', dependencies = TRUE)
install.packages('wordcloud', dependencies = TRUE)
install.packages('tm', dependencies = TRUE)
install.packages('plyr', dependencies = TRUE)

# Loading libraries ----
library(twitteR)
library(wordcloud)
library(tm)
library(plyr)

# Adding parameters Twitter
consumer_key <- "OHx4INa5d7zWIeNsX4cUVOFeR"
consumer_secret <- "cjpr4bpM6aJKkL2tip7elMoSqIBjOLA0F8WA5TjwKrfhCbYdTd"
access_token <- "3323332149-Imwe4hSACTbapTorZRM8DFTkYNtJ5sX3np3olmR"
access_secret <- "h2dOSMgRE4Ns4jDai9GgOdTdQUsvWvmcqjFkJCOm4BZsN"

# Setup ----
setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

# Buscando #hackathonposadas
tweets <- searchTwitter("#hackathonposadas", n=10000) # 371 returned
tweets <- searchTwitter("#hackathonposadas", n=10000, since="2017-05-25", until="2017-05-29") # 277 returned
?searchTwitter

#Convertendo para data.frame e salvando em csv.
bd <- ldply(tweets, function(t) t$toDataFrame() )
write.csv(bd, "./tweetsHackathonPosadas2.csv")
bd <- read.csv("./tweetsHackathonPosadas2.csv")
names(bd)
head(bd)
# convertendo de JSON para corpus 
text <- sapply(tweets, function(x) x$getText())
head(text)
corpus <- Corpus(VectorSource(text))

(f <- content_transformer(function(x) iconv(x, to='latin1', sub='byte')))
corpus <- tm_map(corpus, f)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x)removeWords(x,stopwords("pt")))

# Montando nuvem
wordcloud(corpus, min.freq = 2, max.words = 100, random.order = F)

# adincionando cores
library(RColorBrewer)
?wordcloud
?Corpus
str(corpus)
str(corpus[[1]])
?brewer.pal
display.brewer.all(n=10)

pal2 <- brewer.pal(9,"RdYlGn")
pal2 <- brewer.pal(9,"YlOrRd")
pal2 <- pal2[-(1:3)]
pal2 <- rev(pal2)

png("./WordCloud.png", res=150, height = 600, width = 800)
wordcloud(corpus, min.freq=10,
          max.words=100, random.order=F, colors=pal2,
          scale=c(1.5,.5))
dev.off()
?wordcloud

# analise classificatoria
tdm <- TermDocumentMatrix(corpus)
str(tdm$dimnames)
tdm$dimnames$Docs

tdm <- removeSparseTerms(tdm, sparse = 0.89)
df <- as.data.frame(inspect(tdm))
dim(df)
df.scale <- scale(df)
d <- dist(df.scale, method = "euclidean")
fit.ward2 <- hclust(d, method = "ward.D2")
plot(fit.ward2)
png("./WordCloudDendogram.png", res=150, width = 600, height = 800)
plot(fit.ward2)
rect.hclust(fit.ward2, k=3)
dev.off()

#hackathonposadas
#hacatonposadas





# Seguindo outro tutorial

author <- paste("@", bd$screenName[which(bd[[13]] == "TRUE")], sep = "")

retweets <- bd$retweetCount[which(bd[[13]] == "TRUE")]

isretweet <- bd$retweetCount[which(bd[[13]] == "TRUE")]

library(stringr)
handles <- str_extract_all(bd$text[which(bd[[13]] == "TRUE")], '@[A-Za-a]+[A-Za-z0-9_]+')

date <- bd$created[which(bd[[13]] == "TRUE")]

text <- bd$text[which(bd[[13]] == "TRUE")]

n.handles <- sapply(handles, function(x) length(x))

author.paired <- author[which(n.handles > 0)]

handles.paired <- handles[which(n.handles > 0)]

author.retweeted <- sapply(handles.paired, function(x) (x[[1]]))

n.handles.paired <- sapply(handles.paired, function(x) length(x))

retweetd.paired <- retweets[which(n.handles > 0)]

isretweetd.paired <- isretweet[which(n.handles > 0)]

text.paired <- text[which(n.handles > 0)]

data <- cbind(author.paired, author.retweeted)

library(igraph)
net <- graph.empty()
net <- add.vertices(net, length(unique(c(data))), name=as.character(unique(c(data))))
net <- add.edges(net, t(data))
summary(net)

?layout_with_fr
l <- layout_with_fr(net)
?walktrap.community
str(net)
my.com.fast <- walktrap.community(net)

cent <- data.frame(bet = log(betweenness(net) + 1), eig = log(evcent(net)$vector + 1))
#cent <- data.frame(bet = log(closeness(net) + 1), eig = log(evcent(net)$vector + 1))

res <- lm(eig ~ bet, data = cent)$residuals

cent <- transform(cent, res = res)
head(cent)
library(ggplot2)
p <- ggplot(cent, aes(x = bet, y = eig, label = rownames(cent), colour = res, size = abs(res)
                      )) +
  xlab("Betweenness Central") +
  ylab("Eigenvector Centrality") + geom_point()

p
dev.off()
?degree
outd <- degree(net, mode = "out")
ind <- degree(net, mode = "in")

hubscore <- hub.score(net)$vector
authscore <- hub.score(net)$vector

myauthority <- as.data.frame(cbind(names(outd), hubscore, authscore, outd, ind))

V(net)$size <- abs(res)
nodes <- as.vector(V(net))
V(net)[c(1,3,4,5,6,8,21,22,33,34)]
nodes[which(abs(res) < 0.1)] <- NA

names.label <- c("@CMArzamendiA", "@posadastech", "FelipeSBarros")
names.label <- c(V(net)[c(1,3,4,5,6,8,21,22,33,34)])
my.label <- names(ind)
select.id <- unlist(sapply(names.label, function(x) which(my.label == x)))
my.label[which(log(ind + 1) < 1)] <- ""
my.label2 <- my.label

for (i in 1:length(select.id)) {
  my.label2[select.id[i]] <- names(ind)[select.id[i]]
}

E(net)$color <- "black"

new.color <- data.frame(t(col2rgb(my.com.fast$membership) / 255))
new.color <- rgb(new.color, alpha = 0.25)

png(file = "network2.png")
?plot.igraph
plot.igraph(net, vertex.label = my.label2,
            vertex.size = log(ind + 1) / 3.5, vertex.label.color = "red",
            vertex.color = new.color, vertex.frame.color = new.color,
            edge.width = 0.2, edge.arrow.size = .4, vertex.label.cex = 1,
            edge.curved = TRUE, vertex.label.dist = rnorm(length(ind), 
                                                          .1, .03))
dev.off()


sort(log(ind + 1) / 3.5)
#https://github.com/SMAPPNYU/smappR#5-how-can-i-prepare-a-word-cloud


###-----
require(devtools)
install_github("lchiffon/wordcloud2")
library(wordcloud2)
library(dplyr)
#wordcloud2(data = demoFreq)
tdm.word = TermDocumentMatrix(corpus) %>% as.matrix
tdm.df = data.frame(words = rownames(tdm.word),
                    freq = apply(tdm.word,1,sum))
figPath = system.file("examples/t.png",package = "wordcloud2")
tdm.df2 <- tdm.df[which(tdm.df$freq>50),]
tdm.df2 <- tdm.df[which(tdm.df2$freq>5),]
wordcloud2(tdm.df2, figPath = figPath, size = 1.5,color = "skyblue")
?wordcloud2


corpus = Corpus(VectorSource(enc2native(dificuldades)))
#preparando o df para wordcloud2
tdm.word = TermDocumentMatrix(corpus) %>% as.matrix
tdm.df = data.frame(words = rownames(tdm.word),
                    freq = apply(tdm.word,1,sum))
wordcloud2(tdm.df, size=.6)