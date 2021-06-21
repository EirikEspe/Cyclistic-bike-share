#####################################################
#
# Cyclistic bike-share
# Initial exploration
#
#####################################################

# Calling on libraries
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)


# Loading the bike-sharing data
cyclistic_raw <- readRDS("cyclistic_bike_sharing_data.rds")


# Look at the data
head(cyclistic_raw)
glimpse(cyclistic_raw)


# Rename member_casual column to rider_type and save to a new dataframe
cyclistic <- cyclistic_raw %>% rename(rider_type = member_casual)


# Look for missing values
anyNA(cyclistic)
## Yes, there are missing values

# Missing values per column
colSums(is.na(cyclistic))

# Consider to impute missing values with tidyr::fill

# Saved because of code used in Pivot longer
union(
  cyclistic %>% filter(!is.na(start_station_name) & !is.na(end_station_name) & rider_type == "casual") %>% count(start_station_name, end_station_name, sort = TRUE) %>% slice(1:20) %>% mutate(rider_type = "casual", .before = start_station_name),
  cyclistic %>% filter(!is.na(start_station_name) & !is.na(end_station_name) & rider_type == "member") %>% count(start_station_name, end_station_name, sort = TRUE) %>% slice(1:20) %>% mutate(rider_type = "member", .before = start_station_name)) %>% tidyr::pivot_longer(cols = ends_with("name"), names_to = c("boundary", ".value"), names_pattern = "([:alpha:]+)_(station)_name") %>% ggplot(aes(x = n, y = station)) + geom_col() + facet_wrap(~rider_type, scales = "free_y") + theme_light()


# Check that the start time of rides happen before the stop time
any(cyclistic$started_at > cyclistic$ended_at)
## Some start times are registered as later than the stop times


# How many rides have registered a stop time happening later than start time
length(which(cyclistic$started_at > cyclistic$ended_at))
## 10506 rides


# Check for trips with start time equal to stop time
cyclistic %>% filter(started_at == ended_at) %>% View("zeroTrips")
## This happens in 411 records


# Find test rides. Check if station columns include the word test.
cyclistic %>% 
  filter(if_any(contains("station"),  
                ~str_detect(.x, fixed(pattern = "test", ignore_case = TRUE)))) %>% 
  View("testrides")

# Removing the test rides as they do not represent how the customers use the bikes
cyclistic <- cyclistic %>% 
  filter(!if_any(contains("station"),  
                 ~str_detect(.x, fixed(pattern = "test", ignore_case = TRUE))))
## Removed 3815 records


# Check for duplicated ride_ids
length(which(duplicated(cyclistic$ride_id)))
## 209 duplicate rows


# Looking at duplicated ride_ids
dupl_ride_id <- cyclistic %>% filter(ride_id %in% ride_id[duplicated(ride_id)])


# Check dates for these duplicated records
dupl_ride_id %>% distinct(date(started_at))
## Two dates: 2020-11-25 and 2020-12-15
dupl_ride_id %>% distinct(date(ended_at))
## Two dates: 2020-11-25 and 2020-11-26


### Appendix
# Check for number of distinct start times and stop times for each ride_id
dupl_rides %>% 
  group_by(ride_id) %>% 
  summarise(distinct_start_times = n_distinct(started_at), 
            distinct_stop_times = n_distinct(ended_at)) %>% View("duplTest")
## There are two started times and one ended time for each duplicate ride_id


# Count number of occurrences for these dates
dupl_ride_id %>% count(started_at = date(started_at), ended_at = date(ended_at))
## Notice the two last records. The ended date is earlier than the started date
## I assume this is a bug and find it appropriate to remove these records

# Identify duplicates we want to remove and add to dataframe
duplicate_ids <- dupl_ride_id %>% 
  filter(date(started_at) == ymd("2020-12-15")) %>% 
  select(ride_id, started_at)


# Create a dataframe without the duplicates
cyclistic <- cyclistic %>% 
  filter(!(ride_id %in% duplicate_ids$ride_id & 
             started_at %in% duplicate_ids$started_at))


# Check for duplicates in the new dataframe
length(which(duplicated(cyclistic$ride_id)))


# Adding a column for trip duration
cyclistic <- cyclistic %>% 
  mutate(trip_duration = seconds_to_period(ended_at - started_at))


# Remove trip durations below 60 seconds
cyclistic <- cyclistic %>% filter(trip_duration >= seconds(60))


cyclistic %>% 
  mutate(trip_duration = seconds_to_period(ended_at - started_at)) %>% 
  ggplot(aes(x = trip_duration)) + geom_density() + theme_light()


# Most popular starting stations
cyclistic %>% count(start_station_name, sort = TRUE) %>% slice(1:10)


# Graph of proportion of trips per time of day
cyclistic %>% 
  group_by(time = hour(started_at), rider_type) %>% 
  summarise(trips = n()) %>% 
  group_by(rider_type) %>% 
  mutate(prop_trips = prop.table(trips)) %>% ungroup() %>% 
  
  ggplot(aes(x = time, y = prop_trips)) + 
  geom_line(aes(colour = rider_type)) +
  scale_x_continuous(labels = function(x) 
    if_else(x < 10, paste0("0", x, ":00"), paste0(x, ":00"))) +
  scale_y_continuous(labels = scales::percent) + 
  scale_colour_grey() + 
  labs(title = "Proportion of trips per time of day", 
       x = "Time of day", y = NULL, colour = "Rider type") + 
  theme_light()


# Graph of proportions of trips per weekday
cyclistic %>% 
  group_by(weekday = wday(started_at, label = TRUE, week_start = 1), rider_type) %>% 
  summarise(trips = n()) %>% 
  group_by(rider_type) %>% 
  mutate(prop_trips = prop.table(trips)) %>% ungroup() %>%
  
  ggplot(aes(x = weekday, y = prop_trips)) + 
  geom_line(aes(colour = rider_type, group = rider_type)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_grey() +
  expand_limits(y = 0) +
  labs(title = "Proportion of trips per weekday", 
       x = NULL, y = NULL, colour = "Rider type") + 
  theme_light()
## More than 23% of the trips of casual riders happen on Saturdays, and second most
## trips on Sundays. For members the number of trips per weekday is more even, with
## a low on Sundays.


# Graph of proportions of trips per week
cyclistic %>% 
  group_by(week = floor_date(started_at, "1 week", week_start = 1), rider_type) %>% 
  summarise(trips = n()) %>% 
  group_by(rider_type) %>% 
  mutate(prop_trips = prop.table(trips)) %>% ungroup() %>%
  
  ggplot(aes(x = week, y = prop_trips)) + 
  geom_line(aes(colour = rider_type)) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_grey() +
  expand_limits(y = 0) +
  labs(title = "Proportion of trips per week", 
       x = NULL, y = NULL, colour = "Rider type") + 
  theme_light()


# Graph of 10 most popular trips
cyclistic %>% 
  filter(!is.na(start_station_name) & !is.na(end_station_name)) %>% 
  count(start_station_name, end_station_name, rider_type, sort = TRUE) %>% 
  slice(1:10) %>% 
  
  ggplot(aes(x = n, y = reorder(start_station_name, n))) + 
  geom_col() + 
  geom_text(aes(label = end_station_name), x = 7200, hjust = 0, 
            size = 3, colour = "grey30") + 
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) + 
  coord_cartesian(clip = "off") + 
  labs(title = "Top 10 most popular trips among casual riders", 
       x = "Number of trips", y = NULL) + 
  theme_light() + 
  theme(plot.margin = unit(c(1, 8, 1, 1), "lines"), 
        plot.title.position = "plot")
