# Pre-Req. Needs libucurl, which can be installed on Ubuntu using "sudo apt-get install libcurl4-openssl-dev"
# setwd("~/Code/Masters/IS607/HW10")

# Install the necessary packages
# install.packages("ROAuth")
# install.packages("twitteR")
# install.packages("streamR")
# install.packages("wordcloud")
# install.packages("tm")

# Load the necessary packages
library("ROAuth")
library("twitteR")
library("streamR")
library("wordcloud")
library("tm")

# On first run, create an OAuth object. This generates a URL, which needs to be opened to generate a custom PIN number.
# Enter it when asked by the handshake(...) function and you'll be authenticated. The code then saves this oauth object
# to file, so we can load it without reauthoirzing again
# Taken from: http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/
#
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
#cred <- OAuthFactory$new(consumerKey='hJFNU2CUnrWP7wtYrOmdw',
#                        consumerSecret='mYpRIVG92xbk58rFg1UDwGH4xKryWpgyz3obaqDVqM',
#                        requestURL='http://api.twitter.com/oauth/request_token',
#                        accessURL='http://api.twitter.com/oauth/access_token',
#                        authURL='http://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
## Enter in PIN
#registerTwitterOAuth(cred)
#save(list="cred", file="twitteR_credentials")

# Sign into twitter
load("twitteR_credentials")
registerTwitterOAuth(cred)

# Get Tweets on ObamaCare and store their text
obamacaretweets <- searchTwitter("#obamacare", n=1500, cainfo="cacert.pem")
obamacaretweets_text <- sapply(obamacaretweets, function(x) x$getText())

# Create a corpus and turn it into a word cloud
obamacaretweets_corpus <- Corpus(VectorSource(obamacaretweets_text))
obamacaretweets_corpus <- tm_map(obamacaretweets_corpus, tolower)
obamacaretweets_corpus <- tm_map(obamacaretweets_corpus, removePunctuation)
obamacaretweets_corpus <- tm_map(obamacaretweets_corpus, function(x)removeWords(x,stopwords()))
wordcloud(obamacaretweets_corpus)

# Stream some data from Twitter (timeout is important, as RStudio locks until it timesout)
filterStream(file="output.json", track="obamacare", timeout=10, oauth=cred)