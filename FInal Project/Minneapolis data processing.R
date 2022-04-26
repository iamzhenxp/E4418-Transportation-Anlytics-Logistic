# goal: to find the latitude and longitude of the centerline that the scooter trip is matched to
# this data will be more accurate than just a TAZ match
library(dplyr)
library(sf)
library(readr)
library(lubridate)

# Import data on scooter rides from CSV
Motorized_Foot_Scooter_Trips_2021 <- read_csv("/Users/zhendongyang/Documents/GitHub/E4418-Transportation-Anlytics-Logistic/FInal Project/Scooter Data.csv", 
                                              col_types = cols(EndTime = col_datetime(format = "/Users/zhendongyang/Documents/GitHub/E4418-Transportation-Anlytics-Logistic/FInal Project/Scooter Data.csv"), 
                                                               StartTime = col_datetime(format = "")))
scooter_data <-  Motorized_Foot_Scooter_Trips_2021

# fix timezones
scooter_data$StartTime = with_tz(scooter_data$StartTime, tzone = "America/Chicago")
scooter_data$EndTime = with_tz(scooter_data$EndTime, tzone = "America/Chicago")

mpls_centerlines <- st_read("/Users/zhendongyang/Documents/GitHub/E4418-Transportation-Anlytics-Logistic/FInal Project/streetcenterline.shp")
mpls_centerline_points = st_centroid(mpls_centerlines)

sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

scooter_data_start_points <- scooter_data %>%
  left_join(mpls_centerline_points, by = c('StartCenterlineID' = 'GBSID')) %>%
  select(TripID,TripDuration,TripDistance,StartTime,EndTime,geometry) %>%
  st_sf()

scooter_data_end_points <- scooter_data %>%
  left_join(mpls_centerline_points, by = c('EndCenterlineID' = 'GBSID')) %>%
  select(TripID,TripDuration,TripDistance,StartTime,EndTime,geometry) %>%
  st_sf()

start_data <- scooter_data_start_points %>%
  sfc_as_cols() %>%
  dplyr::rename(start_lon = lon) %>%
  dplyr::rename(start_lat = lat)

end_data <- scooter_data_end_points %>%
  sfc_as_cols() %>%
  dplyr::rename(end_lon = lon) %>%
  dplyr::rename(end_lat = lat)

start_data$geometry <- NULL
end_data$geometry <- NULL

combined_data <- start_data %>%
  left_join(end_data, by = c('TripID' = 'TripID')) %>%
  select(TripID,TripDuration.x,TripDistance.x,StartTime.x,EndTime.x,start_lon,start_lat,end_lon,end_lat) %>%
  rename(TripDistance = TripDistance.x) %>%
  rename(TripDuration = TripDuration.x) %>%
  rename(StartTime = StartTime.x) %>%
  rename(EndTime = EndTime.x)

write.csv(combined_data,"scooter_trips_with_coordinates.csv")