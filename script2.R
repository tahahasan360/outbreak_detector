get_city = function (str){
  extracted <- str_extract(str, ".*,")
  return (ifelse( str_detect(str, ","), substr(extracted,1,nchar(extracted)-1), str))
}
users3 <- mutate(users, city = get_city(location))

get_city_index <- function(city){
  res <- us.cities$lat[which(us_cities == city)]
  return (ifelse(length(res) == 0, 1.0, res))
}

get_city_long <- function(city){
  res <- us.cities$long[which(us_cities == city)]
  return (ifelse(length(res) == 0, 1.0, res))
}

users4 <- mutate(users3, latitude = 0)

remove_last <- function(str, n){
  substr(str,1,nchar(str)-n)
}
us_cities <- remove_last(us.cities$name, 3)

users4 <- mutate(users4, longitude = 0)

for (i in 1:nrow(users4)){
  users4[i, 4] = get_city_index(users4[i, 3])
}

for (i in 1:nrow(users4)){
  users4[i, 5] = get_city_long(users4[i, 3])
}

write.csv(users4, "good.csv")

final_users <- subset(users4, latitude != 1.0)

write.csv(final_users, "users_final.csv")

joined <- merge(final_users, tweets, by = "userID")

write.csv(joined, "2010_joined.csv")

joined <- read.csv("2010_joined.csv")

joined2 <- joined %>% group_by(city)

joined3 <- joined2 %>% summarise(
  freq = n(),
)