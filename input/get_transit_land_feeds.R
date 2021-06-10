##################################################
## Brief: Downloads GTFS feeds from Transit Land API v1
## Date: 05/13/2021
## Author: Eric Chandler <echandler@uchicago.edu>
##################################################

library(tigris)
library(dplyr)
library(sf)
library(jsonlite)
library(purrr)
library(glue)

# Step 1: Get Basic Geographic Bounds  ---------------------------------------

# Load Transit-Land API key from .Renviron file at root of project directory
# To get an API key, visit https://www.transit.land/documentation#signing-up-for-an-api-token
# API key not needed for TransitLand v1
# api_key <- Sys.getenv("TRANSIT_LAND_API_KEY")

# Load lower 48 states' geographies
us_states <- tigris::states(cb=TRUE) %>%
  filter(!(STUSPS %in% c('HI','AK','AS','GU','MP','PR','VI'))) %>%
  st_transform(crs = st_crs(4326)) %>% st_as_sf()

# Create bounding box per state
us_states <- cbind(us_states, sapply(us_states$geometry, st_bbox) %>% t())

# Step 2: Query Available Feeds  ---------------------------------------

#' Gets gtfs feeds inside a state's bounding box from TransitLand
query_gtfs_urls <- function(xmin, ymin, xmax, ymax) {
  feed_locations_url <- "https://transit.land/api/v1/feeds?per_page=5"
  bbox_querystring <- paste0("bbox=",
                             xmin, ",", ymin, ",",
                             xmax, ",", ymax)
  query_url <- paste0(feed_locations_url, "&", bbox_querystring)

    feed_gtfs_urls <- c()
  # Feeds results are paginated so we need to make multiple API requests
  while (!is.null(query_url)) {
    # Wait a few seconds to avoid rate limits?
    Sys.sleep(runif(1)*2)
    print(paste("Querying API: ", query_url))
    # Server may refuse connection
    feed_locations_json <- tryCatch(jsonlite::read_json(query_url),
                                    error=function(e){print(e);NULL})
    if (is.null(feed_locations_json)){
      return(feed_gtfs_urls);
    }
    for (feed in feed_locations_json$feeds){
      inner_feed <- feed %>% flatten()
      if (inner_feed$feed_format == "gtfs") {
        feed_gtfs_urls <- append(feed_gtfs_urls, inner_feed$url)
      }
    }
    query_url <- feed_locations_json$meta[['next']]
  }
  return(feed_gtfs_urls)
}

# Step 3: Download Feeds  ---------------------------------------

# Make directory for new feeds
R.utils::mkdirs("input/shared/feeds/transitland")

for (idx in 1:nrow(us_states)) {
  xmin <- us_states$xmin[idx]
  ymin <- us_states$ymin[idx]
  xmax <- us_states$xmax[idx]
  ymax <- us_states$ymax[idx]
  state_path <- file.path("input/shared/feeds/transitland",
                          us_states$STUSPS[idx])
  if (!file.exists(state_path)) {
    R.utils::mkdirs(state_path)
  }

  url_list_path <- file.path(state_path, "gtfs_urls.txt")
  if (file.exists(url_list_path)) {
    print(paste("Reading cached state", us_states$STUSPS[idx]))
    state_gtfs_urls <- readr::read_lines(url_list_path)
  } else {
    print(paste("Querying for state", us_states$STUSPS[idx]))
    state_gtfs_urls <- query_gtfs_urls(xmin,ymin,xmax,ymax)
    readr::write_lines(state_gtfs_urls, url_list_path)
  }
  for (gtfs_url in state_gtfs_urls) {
    if (is.null(gtfs_url)) {next;}
    feed_path <- file.path(state_path,basename(gtfs_url))
    if (!file.exists(feed_path)) {
      print(paste("Downloading", basename(gtfs_url)))
      tryCatch(curl::curl_download(gtfs_url, feed_path),
               error=function(e){print(e);NULL})
    }
  }
}

## TODO: Replace TransitFeeds file locations
## TODO: Link files to shared folder
