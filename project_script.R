#radius of the earth in meters.  This is used in the function to calculate distance based on 
#longitude and latitude of the starting and ending station of a trip
EARTH_RADIUS <- 6371000

#convert radians to degrees
rad2deg <- function(rad) {(rad * 180) / (pi)}

#convert degrees to radians
deg2rad <- function(deg) {(deg * pi) / (180)}

#the haversine function is used to give the distance in meters between a starting and ending
#longitude and latitude.  See https://en.wikipedia.org/wiki/Haversine_formula.
haversine <- function(lat1,long1,lat2,long2){
  phi1 <- deg2rad(lat1)
  
  phi2 <- deg2rad(lat2)
  
  delta_phi <- deg2rad(lat2 - lat1)
  
  delta_lambda <- deg2rad(long2 - long1)
  
  a <- (sin(delta_phi/2))^2 + 
    cos(phi1)*cos(phi2) *
    (sin(delta_lambda/2))^2
  
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  EARTH_RADIUS * c
}

library(plyr)

library(dplyr)

library(tidyr)

#observations of all trips taken
hubway_trips <- read.csv("C:/Users/Josh/Desktop/CS/NU/CS7280/hubway_2011_07_through_2013_11/hubway_trips.csv")

#station information, including station name and number, longitude and latitude
hubway_stations <- read.csv("C:/Users/Josh/Desktop/CS/NU/CS7280/hubway_2011_07_through_2013_11/hubway_stations.csv")

#filter out trips lasting longer than 12 hours and remove any trips that have na values
hubway_trips <- hubway_trips %>% na.omit() %>% filter(duration < 43200)

summary(hubway_trips)

#Add distance between starting and ending station as a new column

#Couldn't get join to work with columns of different names so create two new data frames from the hubway_stations data
#and join on those new column names
strt_hubway_stations <- plyr::rename(hubway_stations, replace = c("id" = "strt_statn", "lat" = "strt_lat", "lng" = "strt_lng")) %>%
  select(-station, -terminal,-municipal,-status)

end_hubway_stations <- plyr::rename(hubway_stations, replace = c("id" = "end_statn", "lat" = "end_lat", "lng" = "end_lng")) %>%
  select(-station, -terminal,-municipal,-status)

hubway_trips <- join(hubway_trips, strt_hubway_stations)

hubway_trips <- join(hubway_trips, end_hubway_stations)

#add column with trip distance in meters
hubway_trips <- mutate(hubway_trips, dist = haversine(strt_lat, strt_lng, end_lat, end_lng))

#remove all observations that are duplicates of earlier observations based on zip code, gender and birth year.
#As described in the methods section, this is to ensure the independence of the data
deduped.hubway_trips <- hubway_trips[!duplicated(hubway_trips[,c('zip_code','birth_date','gender')]),]

#recode male and female as 1 and 0 to simplify analysis
deduped.hubway_trips$scode <- revalue(deduped.hubway_trips$gender, c("Male" = 1, "Female" = 0))

#begin process of adding new column with rider age by first splitting start_date into the day, month and
#year in one column and time in another column
temporary_data_1 <- deduped.hubway_trips %>% tidyr::separate(start_date, into=c("start_day_mon_year","start_time")," ")

#continue by splitting the day month and year into 3 separate columns
temporary_data_2 <- temporary_data_1 %>% tidyr::separate(start_day_mon_year, into=c("start_day", "start_month", "start_year"), "/")

#calculate the appoximate age of the rider by subtracting the year in which the observation was recorded
#from the birth year of the rider.  There is some subtlety here since, for example someone born in 1988 could
#be 3 different possible ages in 2012 depending on when their birthday is.  Since we don't have this information
#we just subtract the two years from one another.  This will likely even out on average.
hubway_model_data <- temporary_data_2 %>% dplyr::mutate(rider_age = as.numeric(start_year) - birth_date)

library(hexbin)

#create regression model with trip duration as response variabel and rider age, trip distance
#and gender as predictor variables
hubway_fit <- lm(duration ~ dist+rider_age+scode, data = hubway_model_data)

#create regression model with the log of trip duration as response variabel and rider age, trip distance
#and gender as predictor variables
log_hubway_fit <- lm(log(duration)~dist+rider_age+scode,data=hubway_model_data)

#plot residuals vs fitted values to check for non-constant error variance
bin <- hexbin(hubway_fit$fitted.values,hubway_fit$residuals, xbins=50)

#check for normality of residuals
qqnorm(hubway_fit$residuals)
qqline(hubway_fit$residuals)

summary(hubway_fit)

hexbinplot(hubway_fit$residuals~hubway_fit$fitted.values, aspect=1, bins=50, xlab = "Fitted Values", ylab="Residuals", style="lattice")

qqnorm(hubway_fit$residuals)
qqline(hubway_fit$residuals)

summary(log_hubway_fit)

#create a model where log of duration is response and rider age is the single predictor
fit_rider_age <- lm(log(duration) ~ rider_age, data = hubway_model_data)

summary(fit_rider_age)$r.squared

#create model where log of duration is response and rider age and gender are predictors
fit_rider_age_gender <- lm(log(duration)~rider_age+scode, data = hubway_model_data)

#calculate the partial coefficient of determination for model already including rider age when rider gender
#is included as well
(anova(fit_rider_age)$'Sum Sq'[2] - anova(fit_rider_age_gender)$`Sum Sq`[3])/ anova(fit_rider_age)$'Sum Sq'[2]

#calculate partial coefficient of determination for model already including rider age and gender when trip
#duration is included
(anova(fit_rider_age_gender)$`Sum Sq`[3] - anova(log_hubway_fit)$'Sum Sq'[4])/ anova(fit_rider_age_gender)$`Sum Sq`[3]

hexbinplot(log_hubway_fit$residuals~hubway_fit$fitted.values, aspect=1, bins=50, xlab = "Fitted Values", ylab="Residuals", style="lattice")

qqnorm(log_hubway_fit$residuals)
qqline(log_hubway_fit$residuals)

summary(log_hubway_fit)$fstatistic

cor(select(hubway_model_data, dist, rider_age))

new_data_point <- data.frame(rider_age = c(35), dist = c(1000), scode = as.factor(c(1)))

exp(predict.lm(log_hubway_fit, newdata = new_data_point, interval="confidence"))

#select all trips beginning at station #38 and ending at #22
data.38_to_22_trips <- hubway_trips %>% filter(strt_statn == 38) %>% filter(end_statn ==22)

#remove any observations possibly from the same rider to ensure independence of the data
deduped.38_to_22_trips <- data.38_to_22_trips[!duplicated(data.38_to_22_trips[,c('zip_code','birth_date','gender')]),]

deduped.38_to_22_trips$scode <- revalue(deduped.38_to_22_trips$gender, c("Male" = 1, "Female" = 0))

#begin process of adding new column with rider age by first splitting start_date into the day, month and
#year in one column and time in another column
temporary_data_3 <- deduped.38_to_22_trips %>% tidyr::separate(start_date, into=c("start_day_mon_year","start_time")," ")

#continue by splitting the day month and year into 3 separate columns
temporary_data_4 <- temporary_data_3 %>% tidyr::separate(start_day_mon_year, into=c("start_day", "start_month", "start_year"), "/")

hubway_model_data_38_to_22 <- temporary_data_4 %>% dplyr::mutate(rider_age = as.numeric(start_year) - birth_date)

#hubway_plotting_d <- select(hubway_model_data_38_to_22, duration,dist,scode,rider_age)

library(ggplot2)

ggplot(select(hubway_model_data_38_to_22, duration,dist,scode,rider_age), aes(x=rider_age, y=duration, color=scode)) + geom_point(shape=1) + geom_smooth(method=lm)

library(ggmap)

citation('ggmap')

map <- get_map(location = "Boston", zoom = 12)

ggmap(map) + geom_point(aes(x = lng, y = lat), data = hubway_stations, alpha=.5)

start_hubway_trips <- hubway_trips %>% group_by(strt_statn, strt_lat, strt_lng) %>% summarise(station_count = n())

ggmap(map) + geom_point(aes(x = strt_lng, y = strt_lat, size=station_count), data = start_hubway_trips, alpha=.5, col="blue")

end_hubway_trips <- hubway_trips %>% group_by(end_statn, end_lat, end_lng) %>% summarise(station_count = n())

ggmap(map) + geom_point(aes(x = end_lng, y = end_lat, size=station_count), data = end_hubway_trips, alpha=.5, col="red")

