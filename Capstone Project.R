setwd("C:/Users/becke/OneDrive/Desktop/Portfolio/Coursera Capstone 1")
library(tidyverse)
library(lubridate)
library(geosphere)

##Aggregate Data

#read in data
Q1_2019 <- read.csv("Divvy_Trips_2019_Q1.csv")
Q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
Q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
Q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
Q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")

#determine structure of data
str(Q1_2019)
str(Q2_2019)
str(Q3_2019)
str(Q4_2019)
str(Q1_2020)

#match column names to Q1_2020
Q1_2019 <- rename(Q1_2019,
                  ride_id = trip_id,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

Q2_2019 <- rename(Q2_2019,
                  ride_id = X01...Rental.Details.Rental.ID,
                  started_at = X01...Rental.Details.Local.Start.Time,
                  ended_at = X01...Rental.Details.Local.End.Time,
                  start_station_name = X03...Rental.Start.Station.Name,
                  start_station_id = X03...Rental.Start.Station.ID,
                  end_station_name = X02...Rental.End.Station.Name,
                  end_station_id = X02...Rental.End.Station.ID,
                  member_casual = User.Type)

Q3_2019 <- rename(Q3_2019,
                  ride_id = trip_id,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

Q4_2019 <- rename(Q4_2019,
                  ride_id = trip_id,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

#match column structure to Q1_2020
Q1_2019 <- mutate(Q1_2019, ride_id = as.character(ride_id))
Q2_2019 <- mutate(Q2_2019, ride_id = as.character(ride_id))
Q3_2019 <- mutate(Q3_2019, ride_id = as.character(ride_id))
Q4_2019 <- mutate(Q4_2019, ride_id = as.character(ride_id))

#join tables into one
all_trips <- bind_rows(Q1_2019, Q2_2019, Q3_2019, Q4_2019, Q1_2020)

##Clean Data

#remove unwanted columns
column_names <- colnames(Q1_2020)
all_trips <- all_trips %>%
  select(-!all_of(column_names), -rideable_type)

##Extrapolate lat-long to 2019 Data

#find all unique station ids for both start and end
start_stations <- na.omit(unique(all_trips[c("start_station_id","start_lat", "start_lng")]))
end_stations <- na.omit(unique(all_trips[c("end_station_id","end_lat", "end_lng")]))

#combine start and end stations into one category: "stations"
start_stations <- rename(start_stations, station_id = start_station_id,
                lat = start_lat,
                lng = start_lng)
end_stations <- rename(end_stations, station_id = end_station_id,
                lat = end_lat,
                lng = end_lng)
stations <- bind_rows(start_stations, end_stations)
stations <- unique(stations)

#reformat stations to rejoin with the original table
start_stations <- rename(stations, start_station_id = station_id,
                         start_lat = lat,
                         start_lng = lng)
end_stations <- rename(stations, end_station_id = station_id,
                       end_lat = lat,
                       end_lng = lng)

#replace lat-long columns with new data
all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng))
all_trips <- merge(all_trips, start_stations, by = "start_station_id", all.x = TRUE)
all_trips <- merge(all_trips, end_stations, by = "end_station_id", all.x = TRUE)

#determine if there is any missing data for station lat-long
all_trips_start_na <- all_trips %>% filter(is.na(start_lat))
start_station_na <- unique(all_trips_start_na[c("start_station_id", "start_station_name")])
all_trips_end_na <- all_trips %>% filter(is.na(end_lat))
end_station_na <- unique(all_trips_end_na[c("end_station_id", "end_station_name")])

#missing stations - more information needed for lat-long
start_station_na <- rename(start_station_na, station_id = start_station_id,
                           station_name = start_station_name)
end_station_na <- rename(end_station_na, station_id = end_station_id,
                           station_name = end_station_name)
stations_na <- na.omit(unique(bind_rows(start_station_na, end_station_na)))

#import additional lat-long data
additional_stations <- data.frame("station_id" = c(561,652,565,568,665,564,363),
                                     "lat" = c(41.78586107,41.766,41.927666,41.909414,41.743916,41.775393,41.860777),
                                     "lng" = c(-87.67414391,-87.611726,-87.775682,-87.765817,-87.598786,-87.654308,-87.7313639))
stations <- bind_rows(stations, additional_stations)

#reformat stations to rejoin with the original table
start_stations <- rename(stations, start_station_id = station_id,
                         start_lat = lat,
                         start_lng = lng)
end_stations <- rename(stations, end_station_id = station_id,
                       end_lat = lat,
                       end_lng = lng)

#replace lat-long columns with new data
all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng))
all_trips <- merge(all_trips, start_stations, by = "start_station_id", all.x = TRUE)
all_trips <- merge(all_trips, end_stations, by = "end_station_id", all.x = TRUE)


##More Cleaning and Data Preparation

#re-label member_casual
all_trips$member_casual <- gsub("Subscriber", "member", all_trips$member_casual)
all_trips$member_casual <- gsub("Customer", "casual", all_trips$member_casual)

#set start and end times to date-time format
all_trips$started_at <- as.POSIXct(all_trips$started_at, format = "%Y-%m-%d %H:%M:%S")
all_trips$ended_at <- as.POSIXct(all_trips$ended_at, format = "%Y-%m-%d %H:%M:%S")

#Display Date as three columns for day, month, and year
all_trips$date <- as.Date(all_trips$started_at)
all_trips$day_of_week <- format(all_trips$started_at, "%A")
all_trips$day <- format(all_trips$started_at, "%d")
all_trips$month <- format(all_trips$started_at, "%m")
all_trips$year <- format(all_trips$started_at, "%Y")

#find trip duration in seconds and set as numeric
all_trips <- all_trips %>% mutate(trip_duration = ended_at - started_at)
all_trips$trip_duration <- gsub(" secs", "", all_trips$trip_duration)
all_trips$trip_duration <- as.numeric(all_trips$trip_duration)

#add dummy column for counting purposes
all_trips$ride_count <- 1

#Remove data from bikes getting repaired and bad data where start time is after end time
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR"|all_trips$trip_duration < 0),]
avg_trip_duration_v2 <- all_trips_v2 %>% group_by(member_casual) %>% summarise("avg_trip_duration" = mean(trip_duration))

##Analyze Data

#Re-order by day and analyze by member status and day of the week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
trips_by_day <- aggregate(all_trips_v2$trip_duration ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#Analyze data by user type and month
all_trips_month <- all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarize(number_of_rides = n(), avg_duration = mean(trip_duration)) %>%
  arrange(member_casual, month)

#number of trips by start station and user type
start_station_trips <- all_trips_v2 %>%
  group_by(member_casual, "station_id" = start_station_id, "station_name" = start_station_name, "lat" = start_lat, "lng" = start_lng, month) %>%
  summarize("start_ride_count" = sum(ride_count), "start_trip_duration" = mean(trip_duration))
end_station_trips <- all_trips_v2 %>%
  group_by(member_casual, "station_id" = end_station_id, "station_name" = end_station_name, month) %>%
  summarize("end_ride_count" = sum(ride_count), "end_trip_duration" = mean(trip_duration))

#merge trip data by station
station_trips_month <- merge(start_station_trips, end_station_trips, by = c("station_id", "station_name", "member_casual", "month"))
station_trips_month$total_ride_count <- station_trips_month$start_ride_count + station_trips_month$end_ride_count
station_trips_month$total_avg_trip_duration <- (station_trips_month$start_trip_duration * station_trips_month$start_ride_count + station_trips_month$end_trip_duration * station_trips_month$end_ride_count)/(station_trips_month$start_ride_count + station_trips_month$end_ride_count)

#Analyze data by user type and weekday
all_trips_weekday <- all_trips_v2 %>%
  mutate(weekday = wday(date, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarize(number_of_rides = n(), avg_duration = mean(trip_duration)) %>%
  arrange(member_casual, weekday)

#number of trips by start station and user type
start_station_trips <- all_trips_v2 %>%
  group_by(member_casual, "station_id" = start_station_id, "station_name" = start_station_name, "lat" = start_lat, "lng" = start_lng, day_of_week) %>%
  summarize("start_ride_count" = sum(ride_count), "start_trip_duration" = mean(trip_duration))
end_station_trips <- all_trips_v2 %>%
  group_by(member_casual, "station_id" = end_station_id, "station_name" = end_station_name, day_of_week) %>%
  summarize("end_ride_count" = sum(ride_count), "end_trip_duration" = mean(trip_duration))

#merge trip data by station
station_trips_day <- merge(start_station_trips, end_station_trips, by = c("station_id", "station_name", "member_casual", "day_of_week"))
station_trips_day$total_ride_count <- station_trips_day$start_ride_count + station_trips_day$end_ride_count
station_trips_day$total_avg_trip_duration <- (station_trips_day$start_trip_duration * station_trips_day$start_ride_count + station_trips_day$end_trip_duration * station_trips_day$end_ride_count)/(station_trips_day$start_ride_count + station_trips_day$end_ride_count)

all_trips_v3 <- all_trips_v2 %>% select(ride_id, date, day_of_week, day, month, year, trip_duration)

#Export to CSV
write.csv(station_trips_day, file = 'station_trips_day.csv')
write.csv(station_trips_month, file = 'station_trips_month.csv')
write.csv(all_trips_v3, file = 'all_trips.csv')

