# Instalando pacote
doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
toInstall <- c("ROAuth", "igraph", "ggplot2", "wordcloud", "devtools", "tm",
               "R2WinBUGS", "rmongodb", "scales")
if(doInstall){
  install.packages(toInstall, repos = "http://cran.r-project.org")
  library(devtools)
  # R packages to get twitter and Facebook data
  install_github("streamR", "pablobarbera", subdir="streamR")
  install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
  # smapp R package
  install_github("smappR", "SMAPPNYU")
}

# config

consumer_key <- "GPcABoeDttTwa6T8AlpjgMcu1"
consumer_secret <- "O38rauihcR8aD3EYTTNpYx9v6iG2Ja6HDr89e0u3UmUwKvZgk7"
access_token <- "3323332149-bPaMg9WkYrWQXiGKTFe6pBIuF01aQzpbAuv4ibp"
access_secret <- "E2y1z1D8TJc03WrGqNGe6NgPKPd9EppqUEUZhdn6Q5RQ0"

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

library(smappR)
mongo <- mongo.create("MONGO_HOST:PORT", db="DATABASE")
mongo.authenticate(mongo, username='USERNAME', password='PASSWORD', db="DATABASE")

# how can I prepare a word cloud of recent tweets?
tweets <- extract.recent.tweets(set, limit=5000)
  wordFreq <- word.frequencies(tweets1$text) ## word counts

library(wordcloud)
wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
          random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)

# how can I prepare a word cloud of tweets from a given day?
tweets <- extract.tweets(set, from="2014-02-20 00:00:00", to="2014-02-20 23:59:59")
df <- tweetsToDF(tweets)

wordFreq <- word.frequencies(df$text) ## word counts
wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
          random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)

# another example, for tweets mentioning drones
tweets <- extract.tweets(set, string="drone")
wordFreq <- word.frequencies(tweets$text, stopwords=c("amp", "cant", "drones"))
wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
          random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)

# word cloud for recent tweets of any user
getTimeline(screen_name = "p_barbera",
            filename = "pablo_tweets.json", # where tweets will be stored
            n=500, ## number of tweets to download (max is 3,200)
            oauth_folder = "~/Dropbox/credentials" )

library(streamR)
tweets <- parseTweets("pablo_tweets.json") ## reading tweets in R

wordFreq <- word.frequencies(tweets$text)

wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
          random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)

wordFreq <- word.frequencies(tweets$text, 
                             stopwords=c("que", "@p_barbera"))
wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
          random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)

# for more info, again:
?getTimeline
?formatTwDate
?extract.recent.tweets
