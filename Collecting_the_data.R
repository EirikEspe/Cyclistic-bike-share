#####################################################
#
# Cyclistic bike-share
# Collecting the data
#
#####################################################

# Calling on libraries
library(readr)
library(purrr)
library(dplyr)


#--- Loading the bike-sharing data ----

# Create a character vector of the names of the csv files we want to load
# (represented as 6 digits followed by -divvy-tripdata.csv)
files <- list.files(pattern = "[0-9]{6}-divvy-tripdata.csv")

# Read the csv files into a list
cyclistic <- purrr::map(files, read_csv)


# Look at the column names for all the elements in the list
purrr::map(cyclistic, names)

# Check that the column names of the list elements are identical
# (looping through list element 1-11 and check if the column names are equal to 
# elements 2:12)
purrr::map_lgl(1:11, function(i)
  identical(names(cyclistic[[i]]), names(cyclistic[[i+1]])))
## All TRUE



# Look at the data types of the "start_station_id" column
purrr::map_chr(1:12, function(i) 
  cyclistic %>% purrr::pluck(i, "start_station_id") %>% class)

# Look at the data types of the "end_station_id" column
purrr::map_chr(1:12, function(i) 
  cyclistic %>% purrr::pluck(i, "end_station_id") %>% class)
## From December 2020, the start and end_station_id had some listings that included
## letters.


# Reading the csv files with column specifications for start_station_id and
# end_station_id, in order to get one dataframe
cyclistic <- purrr::map(files, read_csv, 
                        col_types = cols(start_station_id = col_character(), 
                                         end_station_id = col_character())) %>% 
  bind_rows()


# Look at the data
head(cyclistic)
glimpse(cyclistic)


# Save the data file
saveRDS(cyclistic, "cyclistic_bike_sharing_data.rds")
