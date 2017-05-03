library(RJSONIO)
library(ggplot2)
library(magrittr)
library(maps)
library(ggrepel)

loc_file <- "data/loc_history.json"
cities_file <- "data/cities.csv"

gmaps_json_to_latlong <- function(loc_file) {
    ## Read and convert JSON location file to a dataframe of the form
    ## timestamp (date) | lat (string) | long (string)

    ## Parse JSON
    loc <- RJSONIO::fromJSON(loc_file)

    ## Drop everything except timestamp, lat, long
    dropped <- lapply(loc[[1]], function(a) a[1:3])

    loc_df <- do.call(rbind.data.frame, c(dropped, stringsAsFactors=FALSE))

    ## Create date column
    loc_df$date <-
        loc_df$timestampMs %>%
        as.numeric %>%
        {./1000} %>% ## Because we have miliseconds
        as.POSIXct(origin="1970-01-01") %>%
        as.Date

    return(loc_df)
}

ldf <- gmaps_json_to_latlong(loc_file)

## Filter only days in trip
trip <- ldf[ldf$date > as.Date("2016-07-28") &
            ldf$date < as.Date("2016-08-15"),]

## Units
trip$long <- trip$longitudeE7 / 10^7
trip$lat <- trip$latitudeE7 / 10^7

## Remove outliers
trip <- trip[trip$long < -50,]
trip <- trip[trip$lat > 33,]

## Additional map data
states <- map_data("state")
cities <- read.csv(cities_file)

trip_map <- ggplot()+
         ggtitle("Roadtrip 2016", subtitle="NYC to YVR")+
         geom_map(data=states, map=states,
                  aes(x=long, y=lat, map_id=region),
                  fill="#ffffff", color="grey70", size=0.4)+
         geom_path(data=trip, aes(x=long, y=lat), linetype=1)+
         geom_point(data=cities, color="red",
                   aes(x=long, y=lat, group=NULL))+
         geom_label_repel(data=cities, aes(x=long, y=lat, label=city),
                          label.size=0, size=3,
                          fill=rgb(1,1,1,0.6),
                          label.padding=unit(0, "lines"),
                          box.padding=unit(0.15, "lines"),
                          point.padding=unit(0.15, "lines"))+
         coord_map()+
         theme_void()+
         theme(plot.title = element_text(hjust = 0.5),
               plot.subtitle = element_text(hjust = 0.5))
