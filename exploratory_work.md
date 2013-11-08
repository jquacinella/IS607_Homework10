Exploratory Work
================

Required Packages
-----------------


```r
list.of.packages <- c("ggplot2", "ggmap", "rjson", "ROAuth", "twitteR", "streamR", 
    "wordcloud", "tm", "plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, 
    "Package"])]
if (length(new.packages)) install.packages(new.packages)
rm(list.of.packages, new.packages)

# Maps
library(ggplot2)
library(ggmap)

# JSON
library(rjson)

# Twitter
library("ROAuth")
```

```
## Loading required package: RCurl
## Loading required package: bitops
## Loading required package: digest
```

```r
library("twitteR")
library("streamR")
library("wordcloud")
```

```
## Loading required package: Rcpp
## Loading required package: RColorBrewer
```

```r
library("tm")

# Text tools
library("plyr")
```

```
## 
## Attaching package: 'plyr'
## 
## The following object is masked from 'package:twitteR':
## 
##     id
```

```r
library("stringr")
```



Getting Data From Twitter
-------------------------

On first run, we need to create an OAuth object. This generates a URL, which needs to be opened to generate a custom PIN number. Enter this PIN when asked by the handshake(...) function and you'll be authenticated. The code then saves this oauth object to file, so we can load it without reauthoirzing again.

Taken from: http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/


```r
download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
cred <- OAuthFactory$new(consumerKey = "hJFNU2CUnrWP7wtYrOmdw", consumerSecret = "mYpRIVG92xbk58rFg1UDwGH4xKryWpgyz3obaqDVqM", 
    requestURL = "http://api.twitter.com/oauth/request_token", accessURL = "http://api.twitter.com/oauth/access_token", 
    authURL = "http://api.twitter.com/oauth/authorize")
cred$handshake(cainfo = "cacert.pem")
# Enter in PIN
registerTwitterOAuth(cred)
save(list = "cred", file = "twitteR_credentials")
```


However, we'll just load the credentials from file.


```r
# Sign into twitter
load("twitteR_credentials")
registerTwitterOAuth(cred)
```

```
## [1] TRUE
```

```r

# Get Tweets on ObamaCare and store their text
obamacaretweets <- searchTwitter("#obamacare", n = 1500, cainfo = "cacert.pem")
```

```
## Warning: 1500 tweets were requested but the API can only return 699
```

```r
obamacaretweets_text <- sapply(obamacaretweets, function(x) x$getText())
```


Generating a Wordcloud
----------------------


```r
# Create a corpus and turn it into a word cloud
obamacaretweets_corpus <- Corpus(VectorSource(obamacaretweets_text))
obamacaretweets_corpus <- tm_map(obamacaretweets_corpus, tolower)
obamacaretweets_corpus <- tm_map(obamacaretweets_corpus, removePunctuation)
obamacaretweets_corpus <- tm_map(obamacaretweets_corpus, function(x) removeWords(x, 
    stopwords()))
obamacaretweets_corpus <- tm_map(obamacaretweets_corpus, function(x) removeWords(x, 
    c("obamacare")))
wordcloud(obamacaretweets_corpus)
```

```
## Warning: conversion failure on 'fastandfurıous' in 'mbcsToSbcs': dot substituted for <c4>
## Warning: conversion failure on 'fastandfurıous' in 'mbcsToSbcs': dot substituted for <b1>
## Warning: conversion failure on 'fastandfurıous' in 'mbcsToSbcs': dot substituted for <c4>
## Warning: conversion failure on 'fastandfurıous' in 'mbcsToSbcs': dot substituted for <b1>
## Warning: font metrics unknown for Unicode character U+0131
## Warning: httptcok2jpm4a7bh could not be fit on page. It will not be plotted.
## Warning: teaparty could not be fit on page. It will not be plotted.
## Warning: repmartharoby could not be fit on page. It will not be plotted.
## Warning: mikandynothem could not be fit on page. It will not be plotted.
## Warning: coming could not be fit on page. It will not be plotted.
## Warning: httptcoodnwxz5twk could not be fit on page. It will not be plotted.
## Warning: obama could not be fit on page. It will not be plotted.
## Warning: remember could not be fit on page. It will not be plotted.
## Warning: fridaykitty could not be fit on page. It will not be plotted.
## Warning: mycancellation could not be fit on page. It will not be plotted.
## Warning: democrats could not be fit on page. It will not be plotted.
## Warning: affordable could not be fit on page. It will not be plotted.
## Warning: felons could not be fit on page. It will not be plotted.
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


Analyzing Sentiment
-------------------

Download Hu & Liu’s opinion lexicon:
site - http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
files - http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar (downloaded Nov. 8, 2013)


```r
pos.words = scan("positive-words.txt", what = "character", comment.char = ";")
neg.words = scan("negative-words.txt", what = "character", comment.char = ";")
```


### Score sentiment function per Jeffrey Breen


```r
score.sentiment = function(sentences, pos.words, neg.words, .progress = "none") {
    # we got a vector of sentences. plyr will handle a list or a vector as an
    # 'l' for us we want a simple array of scores back, so we use 'l' + 'a' +
    # 'ply' = laply:
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
        
        # clean up sentences with R's regex-driven global substitute, gsub():
        sentence = gsub("[[:punct:]]", "", sentence)
        sentence = gsub("[[:cntrl:]]", "", sentence)
        sentence = gsub("\\d+", "", sentence)
        # and convert to lower case:
        sentence = tolower(sentence)
        # split into words. str_split is in the stringr package
        word.list = str_split(sentence, "\\s+")
        # sometimes a list() is one level of hierarchy too much
        words = unlist(word.list)
        # compare our words to the dictionaries of positive & negative terms
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        
        # match() returns the position of the matched term or NA we just want a
        # TRUE/FALSE:
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
        score = sum(pos.matches) - sum(neg.matches)
        return(score)
    }, pos.words, neg.words, .progress = .progress)
    scores.df = data.frame(score = scores, text = sentences)
    return(scores.df)
}
```


### Example / Sanity check


```r
sample = c("You're awesome and I love you", "I hate and hate and hate. So angry. Die!", 
    "Impressed and amazed: you are peerless in your achievement of unparalleled mediocrity.")

result = score.sentiment(sample, pos.words, neg.words)

```


### Score Obamacare Tweets


```r
result <- score.sentiment(obamacaretweets_text, pos.words, neg.words)

# quick visualization of result
hist(result$score)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



Plotting on maps
----------------

Lots of good info here: https://dl.dropboxusercontent.com/u/24648660/ggmap%20useR%202012.pdf

### Partial example from above source:


```r
houston <- get_map("houston", zoom = 14)
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=houston&zoom=14&size=%20640x640&scale=%202&maptype=terrain&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=houston&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
```

```r
HoustonMap <- ggmap(houston, extent = "device", legend = "topleft")
HoustonMap
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


### Map of USA


```r
usa <- get_map("usa", zoom = 4)
```

```
## Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=usa&zoom=4&size=%20640x640&scale=%202&maptype=terrain&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
## Information from URL : http://maps.googleapis.com/maps/api/geocode/json?address=usa&sensor=false
## Google Maps API Terms of Service : http://developers.google.com/maps/terms
```

```r
usaMap <- ggmap(usa, extent = "device", legend = "topleft")
usaMap
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 


Loading JSON Data
-----------------

Looks like there is twitter data saved in output.json

Issues:
Does not seem to read the whole file. Why?


```r
json_file <- "output.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = ""))
```


