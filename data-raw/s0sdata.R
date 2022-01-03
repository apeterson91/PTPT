## code to prepare `s0sdata` dataset goes here

library(tidyverse)
library(sf)
library(osmdata)
library(osrm)
library(stplanr)
library(tidytransit)
library(gtfsrouter)


## use one destination (unif random) to calculate
## trip times to grocery stores

## use tidytransit/gtfsrouter + osrmrouter
## to calculate shortest distance to store from transit stop
## and shortest distance from house to transit (foot) for transit time
## may be better to implement map w/o transit in stage 1

q <- opq("Pittsburgh PA") %>%
    add_osm_feature(key = "shop", 
                    value = "supermarket") %>% 
    osmdata_sf()

## destinations
groceries <- q$osm_polygons %>% 
    st_centroid() %>% 
    select(osm_id,name) %>% 
    rbind(.,q$osm_points %>% 
              filter(!is.na(name)) %>% 
              select(osm_id,name)) %>% 
    mutate(grocery_ix = 1:n())

hoods <-  read_sf("~/Google Drive/CityData/Burgh/Neighborhoods/Neighborhoods_.shp")

streets <- sf::read_sf("~/Google Drive/CityData/Burgh/alleghenycounty_streetcenterlines202107/AlleghenyCounty_StreetCenterlines202107.shp") %>%
    sf::st_transform(sf::st_crs(hoods)) %>%
    sf::st_filter(hoods) %>%
    sf::st_join(hoods %>% select(hood)) %>%
    select(OBJECTID,FULL_NAME,hood,geometry)

## sample parcels in residential areas as origins
resid <- read_sf("~/Documents/CityData/Burgh/Zoning/Zoning.shp") %>% 
    filter(str_detect(legendtype,"Neighborhood|Residential|Public"))


pcls <- read_sf("~/Google Drive/CityData/Burgh/AlleghenyCounty_Parcels202111/AlleghenyCounty_Parcels202111.shp") %>% 
    mutate(valid = map_lgl(geometry,st_is_valid)) %>% 
    filter(valid == TRUE) %>% 
    select(PIN) %>% 
    st_filter(resid,join = st_contains) %>% 
    st_transform(st_crs(hoods)) %>% 
    st_join(hoods) %>% 
    st_centroid() %>% 
    st_transform(4326) %>% 
    mutate(origin_ix = 1:n())

dups <- map_dbl(st_equals(pcls,pcls),function(x) x[[1]])

pcls <- pcls %>% 
    mutate(dups = dups) %>% 
    filter(origin_ix == dups) %>% 
    mutate(origin_ix = 1:n())

pcl_samples <- pcls %>% 
    select(PIN,hood,origin_ix) %>% 
    group_by(hood) %>% 
    sample_n(100,replace = TRUE) %>% 
    ungroup() 

## set up matrix of origins/destinations using top 3 closest (by euclidean distance)
## 
odf <- pcl_samples %>% 
    distinct(origin_ix) %>% 
    left_join(pcls %>% mutate(origin_ix = 1:n()) %>% 
                  select(origin_ix)) %>% 
    st_as_sf() %>% 
    st_distance(groceries) %>% 
    as_tibble() %>% 
    mutate(origin_ix = pcl_samples %>% 
               distinct(origin_ix) %>% 
               pull(origin_ix)) %>% 
    gather(everything(),
           -origin_ix,
           key="grocery_ix",
           value="Distance") %>% 
    mutate(Distance = as.numeric(Distance)/1E3) %>% 
    arrange(origin_ix,Distance) %>% 
    group_by(origin_ix) %>% 
    ## take five closest grocery stores and randomly pick one
    ## Reasoning: We're not as interested in **which** grocery store
    ## is picked (provided it's relatively close) so much as 
    ## how someone gets there
    filter(row_number() <= 5) %>% 
    sample_n(1) %>% 
    ungroup() %>% 
    mutate(grocery_ix = as.numeric(str_remove(grocery_ix,"V"))) 

gorigins <- odf %>% 
    left_join(pcls %>% 
                  mutate(origin_ix=1:n())) %>% 
    st_as_sf() %>% 
    st_coordinates()

gdests <- odf %>% 
    left_join(groceries) %>%
    st_as_sf() %>% 
    st_coordinates()
    
create_tripdf <- function(odf,mode,origins,destinations){
    tdf <- map_dfr(mode,function(md){
        trips <- stplanr::route(from = origins,
                                to = destinations,
                                route_fun = osrm::osrmRoute,
                                returnclass = "sf",
                                osrm.profile = md)
        out <- trips %>% 
            mutate(mode = md,
                   origin_ix = odf$origin_ix,
                   grocery_ix = odf$grocery_ix) %>% 
            st_as_sf()
        return(out)
    })
    return(tdf)
}

gtdf <- create_tripdf(odf,c("car","bike","foot"),gorigins,gdests)



usethis::use_data(gtdf,overwrite = TRUE)

## snap trips to street segments


foo <- gtdf[1,] %>% st_transform(st_crs(streets))

bar <- foo %>% st_segmentize(dfMaxLength = 50) %>% st_cast(.,"POINT")

ics <- st_nearest_feature(bar,streets)

mapview::mapview(streets[ics,])

## Current method for determining street *segments*
## associated with each trip -- this is not 
## a perfect method. As street segments not 
## associated with the actual route are sometimes 
## included because of proximity
## I had an idea to fix this but 
## have forgot it at present. When I remember 
## I will return to fix this. 
## Given that this is already "simulated data"
## I proceed with the current method in 
## spirit of not letting the perfect be the 
## enemy of the good (enough).
gtpdf <- gtdf %>% 
    mutate(trip_ix = 1:n()) %>% 
    st_transform(st_crs(streets)) %>% 
    st_segmentize(100) %>% 
    st_cast(.,"POINT") %>% 
    group_by(trip_ix) %>% 
    split(.$trip_ix) %>% 
    map_dfr(.,function(x){ 
        tibble(trip_ix = unique(x$trip_ix),
               street_ix = st_nearest_feature(x,streets)) %>% 
            distinct(trip_ix,street_ix) %>% 
            mutate(order_ix = 1:n())
    })


## join subject ix to trip ix
# s0sdata <- 

usethis::use_data(s0sdata, overwrite = TRUE)
