library(rmongodb)
library(jsonlite)
mongo <- mongo.create()

if (mongo.is.connected(mongo)) {
  p1 <- mongo.bson.from.JSON('{ "$match": { "lang" : "en" } }')
  p2 <- mongo.bson.from.JSON('
    {
       "$group" : {
         "_id" : "$user.screen_name",
         "tweets" : { "$push": "$text" },
         "places" : { "$push": "$place.name" }
       }
     }
  ')
  cmd_list <- list(p1, p2)
  res <- mongo.aggregation(mongo, "guzzler.history", cmd_list, allowDiskUse = TRUE)

  tweets <- mongo.bson.to.list(res)

  tweets_as_frame <- data.frame(
    user = character(),
    tweet = character(),
    tourist = logical(),
    places = character(),
    stringsAsFactors = FALSE)
  
  counter = 0
  for (tweets_by_name in tweets$result) {
    for (tweet in tweets_by_name$`tweets`) {
      counter <- counter + 1
      places <- unique(unlist(tweets_by_name$`places`, use.names = FALSE))

      tweets_as_frame[counter,] <- list(
        tweets_by_name$`_id`,
        tweet,
        length(places) > 1,
        paste(places, collapse = ", ")
      )
    }
  }
}

write.csv(tweets_as_frame, file = "tourists_residents_eng.csv")

mongo.destroy(mongo)
