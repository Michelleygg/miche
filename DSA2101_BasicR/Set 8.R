library(jsonlite)
library(sf)
get_taxi_data <- function(tstamp) {
  url_tmp <- paste0("https://api.data.gov.sg/v1/transport/taxi-availability?date_time=",
                    format(tstamp, "%Y-%m-%dT%H:%M:%S"))
  data_tmp <- fromJSON(url_tmp)
  class(data_tmp) <- "taxi_json"
  data_tmp
}

summary.taxi_json <- function(object, ...) {
  cat(paste("Taxi count:", object$features$properties$taxi_count), "\n")
  cat(paste("Actual timestamp:", object$features$properties$timestamp), "\n")
}
as.data.frame.taxi_json <- function(x, row.names = NULL, optional = FALSE, ...){
  t_coords <- as.data.frame(x$features$geometry$coordinates[[1]])
  taxis <- st_as_sf(t_coords, coords=c(1,2))
  st_crs(taxis) <- 4326
  taxis <- st_transform(taxis, 3414)
  taxis
}

dt1 <- as.POSIXct("2021-01-10 09:00:00")
wet_day_locs <- get_taxi_data(dt1)

pln_areas <- readRDS("C:/Users/admin/Downloads/data/sg_planning_areas.rds")
wet_day_sf <- as.data.frame(wet_day_locs)

dt2 <- as.POSIXct("2021-01-17 09:00:00")
dry_day_locs <- get_taxi_data(dt2)
dry_day_sf <- as.data.frame(dry_day_locs)

opar <- par(mfrow=c(1,2))
plot(st_geometry(pln_areas), axes=TRUE, main="Wet Day")
plot(wet_day_sf, col=gray(0.3, 0.4), pch="+", add=TRUE, cex=0.6)

plot(st_geometry(pln_areas), axes=TRUE, main="Dry Day")
plot(dry_day_sf, col=gray(0.3, 0.4), pch="+", add=TRUE, cex=0.6)
par(opar)

wet_count <- st_contains(pln_areas, wet_day_sf) %>% sapply( FUN=length)
dry_count <- st_contains(pln_areas, dry_day_sf) %>% sapply( FUN=length)

plot(x=wet_count, y=dry_count, col=gray(0.1, 0.4), xlab="Wet Count", 
     pch=19, ylab="Dry Count", main="Dry vs. Wet Day")
text(x=wet_count, y=dry_count-5, labels = pln_areas$PLN_AREA_C, cex = 0.6)
abline(a=0, b=1, lty=2, col="red")