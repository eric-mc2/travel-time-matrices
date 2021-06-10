##################################################
## Brief: Validates GTFS feeds sourced from Transit Land API v1
## Date: 06/10/2021
## Author: Eric Chandler <echandler@uchicago.edu>
##################################################

library(tidytransit)
library(dplyr)

blacklist <- tibble(feed=character(), reason=character())
feeds_path <- file.path("input/shared/feeds")
feed_names <- list.files(feeds_path, pattern='.*.zip')
for (feed_name in feed_names) {
  # Try reading feed into data frame
  feed_path <- file.path(feeds_path, feed_name)
  feed <- tryCatch(read_gtfs(feed_path), error=function(e){print(e);NULL})
  if (is.null(feed)) {
    blacklist <- blacklist %>% add_row(feed=feed_name, reason="cant read")
    next;
  }

  # Try converting feed data frame into sf
  feed_obj <- tryCatch(gtfs_as_sf(feed), error=function(e){
    print(paste("ERROR:", feed_path)); print(e);NULL})
  if (is.null(feed_obj)) {
    blacklist <- blacklist %>% add_row(feed=feed_name, reason="cant convert sf")
    next;
  }

  # Try extracting route geometries
  geoms <- tryCatch(suppressWarnings(get_route_geometry(feed_obj)), error=function(e){
    print(paste("ERROR:", feed_path)); print(e);NULL})
  if (is.null(geoms)) {
    blacklist <- blacklist %>% add_row(feed=feed_name, reason="cant construct geom")
    next;
  }

  # Check validity of route geometries
  for (route_idx in 1:nrow(geoms)) {
    geom <- geoms$geometry[route_idx]
    route_id <- geoms$route_id[route_idx]
    if (!st_is_valid(geom)) {
      blacklist <- blacklist %>% add_row(feed=feed_name, reason="invalid route geom")
      next;
    }
  }
}
print("List of invalid geos:")
print(blacklist)
write.csv(blacklist,"input/shared/feeds/blacklist2.csv", row.names = FALSE)
