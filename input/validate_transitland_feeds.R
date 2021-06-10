##################################################
## Brief: Validates GTFS feeds sourced from Transit Land API v1
## Date: 06/10/2021
## Author: Eric Chandler <echandler@uchicago.edu>
##################################################

library(tidytransit)
library(dplyr)

state_dirs <- list.files("input/shared/feeds/transitland", pattern='[A-Z]{2}')
blacklist <- tibble(state=character(), feed=character(), reason=character())
for (state_abbr in state_dirs) {
  state_path <- file.path("input/shared/feeds/transitland", state_abbr)
  feed_names <- list.files(state_path, pattern='.*.zip')
  print(paste("Checking state", state_abbr))
  for (feed_name in feed_names) {
    feed_path <- file.path(state_path, feed_name)
    feed <- tryCatch(read_gtfs(feed_path), error=function(e){print(e);NULL})
    if (is.null(feed)) {
      blacklist <- blacklist %>% add_row(state=state_abbr, feed=feed_name, reason="cant read")
      next;
    }
    feed_obj <- tryCatch(gtfs_as_sf(feed), error=function(e){
      print(paste("ERROR:", feed_path)); print(e);NULL})
    if (is.null(feed_obj)) {
      blacklist <- blacklist %>% add_row(state=state_abbr, feed=feed_name, reason="cant convert sf")
      next;
    }
    geoms <- tryCatch(suppressWarnings(get_route_geometry(feed_obj)), error=function(e){
      print(paste("ERROR:", feed_path)); print(e);NULL})
    if (is.null(geoms)) {
      blacklist <- blacklist %>% add_row(state=state_abbr, feed=feed_name, reason="cant construct geom")
      next;
    }
    for (route_idx in 1:nrow(geoms)) {
      geom <- geoms$geometry[route_idx]
      route_id <- geoms$route_id[route_idx]
      if (!st_is_valid(geom)) {
        blacklist <- blacklist %>% add_row(state=state_abbr, feed=feed_name, reason="invalid route geom")
        next;
      }
    }
  }
}
print("List of invalid geos:")
print(blacklist)
write.csv(blacklist,"input/shared/feeds/transitland/blacklist.csv", row.names = FALSE)
