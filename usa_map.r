library(RJSONIO)
library(ggplot2)
library(maps)

loc_file <- "data/loc_history.json"

gmaps_json_to_latlong <- function(loc_file) {
    ## Read and convert JSON location file to a dataframe of the form
    ## timestamp (date) | lat (string) | long (string)
    
    ## Parse JSON 
    loc <- RJSONIO::fromJSON(loc_file)
    print(paste(Sys.time(), "JSON loaded to memory"))

    ## Drop everything except timestamp, lat, long
    dropped <- lapply(loc[[1]], function(a) a[1:3])
    print(paste(Sys.time(), "All data dropped except timestamp, lat, and long"))
    
    loc_df <- do.call(rbind.data.frame, dropped)
    print(paste(Sys.time(), "List converted to data frame"))

    ## Create date and posix columns from timestamp
    loc_df$posix <- as.character(loc_df$timestampMs)
    loc_df$posix <- as.numeric(loc_df$posix)
    loc_df$posix <- as.POSIXct((loc_df$posix+0.1)/1000, origin="1970-01-01")
    loc_df$date <- as.Date(loc_df$posix)
    print(paste(Sys.time(), "Date and Posix columns created from timestamp"))
    
    print(paste(Sys.time(), "All done"))
    return(loc_df)
}

ldf <- gmaps_json_to_latlong(loc_file)

## Filter only days in trip
trip <- ldf[ldf$date > as.Date("2016-07-28"),]
trip$long <- trip$longitudeE7 / 10^7
trip$lat <- trip$latitudeE7 / 10^7

## Map stuff
states <- map_data("state")
trip_map <- ggplot()+
         geom_map(data=states, map=states, aes(x=long, y=lat, map_id=region),
                  fill="#ffffff", color="grey70", size=0.15)+
         geom_path(data=trip, aes(x=long, y=lat))
