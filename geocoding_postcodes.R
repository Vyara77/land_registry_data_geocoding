### loading libraries 
library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)
library(geosphere)
library(microbenchmark)

# Setting directory and defining file names ----
dir_master <- getwd()
data_folder <- paste(dir_master, "data", sep = "//")

base_data_filename <- "ppd_data.csv"
postcode_lookup_filename <- "NSPL21_AUG_2023_UK.csv"
stops_filename <- "Stops.csv"

# Importing price data, postcode lookup and transport links data ----

## Importing and cleaning property price data ----
  
base_data <- read.csv(paste(data_folder, base_data_filename, sep = "//"))

#remove unncessary columns
base_data <- base_data[, 1:16]

# trimming base_data for joining 
base_data_trimmed <- base_data %>% 
  select(unique_id, postcode) %>%
  mutate(postcode2 = gsub(" ", "", postcode))

## Importing and cleaning postcode lookup table ----

postcode_lookup <-read.csv(paste(data_folder, postcode_lookup_filename, sep = "//"))

# Selecting necessary variables - postcode, latitude, longitude, LSOA and constituency
postcode_lookup <- postcode_lookup %>%
  select(pcd, lat, long, lsoa21, pcon) %>%
  mutate(pcd = gsub(" ", "", pcd))


## Importing transport links dataset ----

stops <- read.csv(paste(data_folder, stops_filename, sep = "//"))

newcastle_stops <- stops %>% 
  filter(grepl("Newcastle upon Tyne",ParentLocalityName))


# Joining postcode lookup to base data ----

# joining postcode lookup to base data on postcode data to get latitude and longitude

df <- base_data_trimmed %>%
  left_join(postcode_lookup, by = c("postcode2" = "pcd")) %>% 
  filter(postcode !="")

# Calculating distances between properties and transport links ---- 

# Creating a distance matrix to store distances between properties and transport links

distance_matrix <- matrix(nrow = nrow(df), ncol = nrow(newcastle_stops))
#names(distance_matrix) <-  newcastle_stops$ATCOCode

# calculate the distance between each address to each transport stop
# and store it in the distance matrix 
# THINK ABOUT HOW YOU CAN AVOID THE NESTED FOR LOOPS as this is bad practice 

# for(i in 1:nrow(df)){
#   
#   for(k in 1:nrow(newcastle_stops)){
#     
#     dist <- distHaversine(c(df$long[i], df$lat[i]), c(newcastle_stops$Longitude[k], newcastle_stops$Latitude[k]))
#     distance_matrix[i,k] <- dist
#     
#   }
#  
# }




property_coordinates <- df %>% 
  select(long, lat)

stops_coordinates <- newcastle_stops %>% 
  select(Longitude, Latitude)

distance_matrix <- distm(property_coordinates, stops_coordinates)


names(distance_matrix) <-  newcastle_stops$ATCOCode
distance_matrix$property_id <- df$unique_id




#### trying to speed up the code 

# distance_matrix_dummy <- matrix(nrow = 5, ncol = 10)
#   
# microbenchmark::microbenchmark(
#   
#   
# for(i in 1:5){
#   
#   for(k in 1:10){
#     
#     dist <- distHaversine(c(df$long[i], df$lat[i]), c(newcastle_stops$Longitude[k], newcastle_stops$Latitude[k]))
#     distance_matrix_dummy[i,k] <- dist
#     
#   }
#   
# }
# )
# 
# # changing [] to [[]] to check impact on speed
# 
# microbenchmark::microbenchmark(
#   for(i in 1:5){
#     
#     for(k in 1:10){
#       
#       dist <- distHaversine(c(df$long[[i]], df$lat[[i]]), c(newcastle_stops$Longitude[[k]], newcastle_stops$Latitude[[k]]))
#       distance_matrix_dummy[i,k] <- dist
#       
#     }
#     
#   }
# )
# 
# # attempt to change to lapply
# 
# 
# 
# xy <- rbind(c(0,0),c(90,90),c(10,10),c(-120,-45))
# distm(xy)
# xy2 <- rbind(c(0,0),c(10,-10))
# distm(xy, xy2)
# 
# property_coordinates <- df %>% select(long, lat)
# property_coordinates <- as.matrix(property_coordinates)
# property_coordinates <- property_coordinates[1:5, ]
# 
# stops_coordinates <- newcastle_stops %>% select(Longitude, Latitude)
# stops_coordinates <- as.matrix(stops_coordinates)
# stops_coordinates <- stops_coordinates[1:10, ]
# 
# microbenchmark::microbenchmark(
# distance_matrix_dummy_2 <- distm(property_coordinates, stops_coordinates),
# unit = "milliseconds")
# 
# distHaversine(c(property_coordinates$long[1], property_coordinates$lat[1]), c(stops_coordinates$Longitude[1], stops_coordinates$Latitude[1]))
