if (file.exists("filtered_nyc_tweets")){
  load("filtered_nyc_tweets")
} else {
  # Sign into twitter
  load("twitteR_credentials")
  registerTwitterOAuth(cred)
  
  # Bounding box for NYC
  min_lon_nyc <- -74
  min_lat_nyc <- 40
  max_lon_nyc <- -73
  max_lat_nyc <- 41
  
  filterStream(file="obama_nyc.json", track="#obamacare", locations=c(min_lon_nyc, min_lat_nyc, max_lon_nyc, max_lat_nyc), timeout=60, oauth=cred)
  obama_tweet_stream <- parseTweets("obama_nyc.json")
  
  # Convert long and lat columns to decimals
  obama_tweet_stream$lon <- as.numeric(obama_tweet_stream$lon)
  obama_tweet_stream$lat <- as.numeric(obama_tweet_stream$lat)
  
  # Filter tweets that have geo information
  filtered_nyc_tweets <- obama_tweet_stream[!is.na(obama_tweet_stream$lon) & 
                                             !is.na(obama_tweet_stream$lat) &
                                             obama_tweet_stream$lon >= min_lon_nyc &
                                             obama_tweet_stream$lon <= max_lon_nyc & 
                                             obama_tweet_stream$lat >= min_lat_nyc &
                                             obama_tweet_stream$lat <= max_lat_nyc, ]
  

#   num_tweets <- nrow(filtered_nyc_tweets)
#   filtered_nyc_tweets$sentiment <- rep(0, num_tweets)
#   filtered_nyc_tweets$score <- rep(0, num_tweets)
#   
#   start.time <- Sys.time()
#   for (i in 1:num_tweets){
#     tmp <- getSentiment(filtered_nyc_tweets[i, 'text'], "6LUTxvF9YYYjKWEZZ7X")
#     filtered_nyc_tweets$sentiment[i] <- tmp$mood
#     filtered_nyc_tweets$score[i] <- tmp$score
#     if (i %% 20 == 0){
#       print(c(i, Sys.time() - start.time))
#     }
#   }
  
  save(filtered_nyc_tweets, file="filtered_nyc_tweets")
}

lon_stats = boxplot(filtered_nyc_tweets$lon, plot=FALSE)$stats
min_lon_data = lon_stats[2]
max_lon_data = lon_stats[4]

lat_stats = boxplot(filtered_nyc_tweets$lat, plot=FALSE)$stats
min_lat_data = lat_stats[2]
max_lat_data = lat_stats[4]

nycMap <- qmap(location = c(min_lon_data, min_lat_data, max_lon_data, max_lat_data))
nycMap
nycMap + geom_point(aes(x=lon, y=lat), data=filtered_nyc_tweets)