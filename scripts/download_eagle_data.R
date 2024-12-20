#Libraries used
library(sf)
library(httr2)
library(parallel)
library(furrr)
library(tidyverse)

#Where should the data be save?
# Examples: "C:/Users/<user>/Downloads"
wd <- ""

#Number of CPU cores to use to speed up the download
workers <- detectCores(logical = TRUE) - 4

#Set the working directory
setwd(wd)

#WFS service location
wfs_url <- paste0("https://eagle.abrinc.com/cgi-bin/mapserv?",
  "map=/var/www/gis/eagle.map&SERVICE=WFS&VERSION=1.1.0&",
  "REQUEST=GetFeature&typeName=eagle&srsName=EPSG:4326&outputFormat=geojson")

#Bounding box for Alaska
bbox <- c(-179.148611, 51.209722, -129.979444, 71.538611)
bbox_param <- paste(bbox, collapse = ",")

#Append the bbox parameter to the URL
full_url <- paste0(wfs_url, "&bbox=", bbox_param)

#Get the data and convert to a spatial features object
eagle_ids <- full_url |>
  request() |>
  req_perform() |>
  resp_body_string() |>
  st_read()

#See this site for GET methods
#https://eagle.abrinc.com/wildlife_eagle_nest_usfws/v1/

#Base URL for GET methods
base_url <- "https://eagle.abrinc.com/wildlife_eagle_nest_usfws/v1/"

#Info about survey
nest_lva <- paste0(base_url, "nest_location_visit_architecture/nest_id/")

#Function to get data
# nest_id: The id of an individual nest
# url: Base URL containing GET information
get_data <- function(nest_id, url) {
  #Construct URL and get data
  resp <- paste0(url, nest_id, "/") |>
    request() |>
    req_perform()

  #Check data, otherwise convert to dataframe
  if (resp_status(resp) == 200) {
    data <- resp_body_json(resp)
  } else {
    return(NULL)
  }
  data |>
    map_dfr(as_tibble)
}

#Create a little wrapper around the get_data function
# Makes it easier to use with parallel download
get_data_wrapper <- function(x, y) {
  url <- x[[1]]
  map_dfr(url, get_data, y)
}

#Setup the parallel processing
plan(multisession, workers = workers)

#Download the data
eagle_data <- eagle_ids |>
  as_tibble() |>
  select(nest_id) |>
  #make chunks of 100
  mutate(n = ceiling(row_number() / 500)) |>
  group_by(n) |>
  nest() |>
  ungroup() |>
  mutate(lva = future_map(
    data,
    ~ get_data_wrapper(.x, nest_lva),
    .progress = TRUE)) |>
  select(-c(n, data)) |>
  unnest(lva)

#Release extra CPU cores/threads
plan(sequential)

#Take a quick look at the data in tabular form
eagle_data

#Plot the data
eagle_data |>
  st_as_sf(coords = c("longitude_nad83", "latitude_nad83"), crs = 4326) |>
  ggplot() +
  geom_sf()

#Save it to the working directory
write_csv(eagle_data, file = "eagle_data.csv", na = "")
