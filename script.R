library(RSQLite)
library(dplyr)
library(ggplot2)
library(stringr)
library(maps)
library(gganimate)
library(lubridate)
library(usmap)
library(ggthemes)

db_path <- "2010_tweets.db"
dcon <- dbConnect(SQLite(), dbname = db_path)
query <- "SELECT * FROM tweets"
res <- dbSendQuery(dcon, query)
comments <- dbFetch(res, -1)
dbClearResult(res)
tweets <- comments

write.csv(users, "2010_users.csv")

db_path <- "2010_tweets.db"
dcon <- dbConnect(SQLite(), dbname = db_path)
query <- "SELECT * FROM users"
res <- dbSendQuery(dcon, query)
comments <- dbFetch(res, -1)
dbClearResult(res)
users <- comments

if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

## load rtweet
library(rtweet)

## store api keys (these are fake example values; replace with your own keys)
api_key <- "vNjxGpVQ8HmCCQs2OLhic6mlA"
api_secret_key <- "FtGOLCajn2mH8a7IR0lBUX9ek0papFws0N8R1u01yYLYBVGTHp"
access_token <- "1487230370395398144-2Vm4SJ7YztCMnIkSu8U4NXqbOaZSrC"
access_token_secret <- "h6rD1qYCyyhxkjJtSplMDe67xNRP7UqHJp6paDercQPke"

## authenticate via web browser
token <- create_token(
  app = "Raahim",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

token
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)

unique(users$location)

swine <- subset(joined, str_detect(tweet, "swine flu"))

swine <- swine %>% group_by(longitude, latitude)

swine_freq <- swine %>% summarise(
  freq = n(),
)

options(stringsAsFactors = FALSE)

MainStates <- map_data("state")

world_basemap <- ggplot() +
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="black", fill="yellow1") +
  labs(title = "US Counties") +
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

p <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point()

plot(p)

anim <- p + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

anim

tweet_locations_grp <- swine %>%
  mutate(day = day(timestamp),
         long_round = round(longitude, 2),
         lat_round = round(latitude, 2)) %>%
  group_by(day, long_round, lat_round) %>%
  summarise(total_count = n())

tweet_locations_grp5 <- tweet_locations_grp %>% mutate(day = day %/% 5)

grouped_tweet_map <- world_basemap + geom_point(data = tweet_locations_grp,
                                                aes(long_round, lat_round, frame = day, size = total_count),
                                                color = "purple", alpha = .8) + coord_fixed() +
  labs(title = "Twitter Mentions Of Swine Flu During The 2009-2010 Outbreak") +
  transition_states(day, transition_length = 10, state_length = 5, wrap = TRUE)

grouped_tweet_map

grouped_tweet_map2 <- world_basemap + geom_point(data = tweet_locations_grp5,
                                                aes(long_round, lat_round, frame = day, size = total_count * 5),
                                                color = "purple", show.legend = FALSE) + coord_fixed() +
  labs(title = 'Twitter Mentions Of Swine Flu During The 2009-2010 Outbreak') +
  xlab("Longitude") + ylab("Latitude") +
  transition_states(day, transition_length = 500, state_length = 10)

grouped_tweet_map2

sum(tweet_locations_grp$total_count)

swine_symptoms <- subset(joined, str_detect(tweet, "sore throat|runny nose|bodyache|body ache|headache|fatigue"))

symptoms_df <- swine %>%
  mutate(day = day(timestamp) %/% 5,
         long_round = round(longitude, 2),
         lat_round = round(latitude, 2)) %>%
  group_by(day, long_round, lat_round) %>%
  summarise(total_count = n())

symptoms_map <- world_basemap + geom_point(data = symptoms_df,
                                                 aes(long_round, lat_round, frame = day, size = total_count * 5),
                                                 color = "purple", alpha = .8, show.legend = FALSE) + coord_fixed() +
  labs(title = 'Twitter Mentions Of Swine Flu Symptoms During The 2009-2010 Outbreak') + xlab("Longitude") + ylab("Latitude")
  transition_states(day, transition_length = 500, state_length = 10)

symptoms_map

anim1 <- animate(tweet_locations_grp5)

anim_save("swine.gif", grouped_tweet_map2)

anim_save("swine_symptoms.gif", symptoms_map)
