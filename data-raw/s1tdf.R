## code to prepare `s1tdf` dataset goes here

library(tidyverse)
library(sf)
library(osmdata)
library(osrm)
library(stplanr)
library(tidytransit)
library(gtfsrouter)
set.seed(3431)

## use one destination (unif random) to calculate
## trip times to grocery stores

## use tidytransit/gtfsrouter + osrmrouter
## to calculate shortest distance to store from transit stop
## and shortest distance from house to transit (foot) for transit time
## may be better to implement map w/o transit in stage 1



# Groceries ---------------------------------------------------------------

q <- opq("Pittsburgh PA") %>%
    add_osm_feature(key = "shop", 
                    value = "supermarket") %>% 
    osmdata_sf()

## destinations
## 58 groceries when queried 1/3/22
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
    select(OBJECTID,FULL_NAME,hood,geometry) %>% 
    mutate(street_ix = 1:n())

## set up matrix of origins/destinations using top 3 closest (by euclidean distance)
## 
godf <- s1hdata %>% 
    distinct(origin_ix,geometry) %>% 
    st_as_sf() %>% 
    st_distance(groceries) %>% 
    as_tibble() %>% 
    mutate(origin_ix = s1hdata %>% 
               st_drop_geometry() %>% 
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
    ## how someone gets there conditional on the distance, etc.
    filter(row_number() <= 5) %>% 
    sample_n(1) %>% 
    ungroup() %>% 
    mutate(grocery_ix = as.numeric(str_remove(grocery_ix,"V"))) 

gorigins <- godf %>% 
    left_join(s1hdata %>% 
                  distinct(origin_ix,geometry)) %>% 
    st_as_sf() %>% 
    st_coordinates()

gdests <- godf %>% 
    left_join(groceries) %>%
    st_as_sf() %>% 
    st_coordinates()
    
create_tripdf <- function(odf,mode,origins,destinations,triptype){
    tdf <- map_dfr(mode,function(md){
        trips <- stplanr::route(from = origins,
                                to = destinations,
                                route_fun = osrm::osrmRoute,
                                returnclass = "sf",
                                osrm.profile = md)
        out <- trips %>% 
            mutate(mode = md,
                   triptype = triptype,
                   origin_ix = odf$origin_ix,
                   grocery_ix = odf$grocery_ix) %>% 
            st_as_sf()
        return(out)
    })
    return(tdf)
}

gtdf <- create_tripdf(godf,
                      c("car","bike","foot"),
                      gorigins,gdests,
                      triptype = "Grocery")



# Commute Trips -----------------------------------------------------------


## join subject ix to trip ix
commute_zns <- read_sf("~/Documents/CityData/Burgh/Zoning/Zoning.shp") %>% 
    filter(str_detect(legendtype,"Industrial|Medical|Educational|Commercial|Office")) 


commute_pcls <- read_sf("~/Google Drive/CityData/Burgh/AlleghenyCounty_Parcels202111/AlleghenyCounty_Parcels202111.shp") %>% 
    mutate(valid = map_lgl(geometry,st_is_valid)) %>% 
    filter(valid == TRUE) %>% 
    select(PIN) %>% 
    st_filter(commute_zns,join = st_contains) %>% 
    st_transform(st_crs(hoods)) %>% 
    st_join(hoods) %>% 
    st_centroid() %>% 
    st_transform(4326) %>% 
    mutate(commute_ix = 1:n())


dups <- map_dbl(st_equals(commute_pcls,commute_pcls),function(x) x[[1]])

commute_pcls <- commute_pcls %>% 
    mutate(dups = dups) %>% 
    filter(commute_ix == dups) %>% 
    mutate(commute_ix = 1:n()) %>% 
    select(PIN,hood,commute_ix)

commute_samples <- commute_pcls %>% 
    select(hood,commute_ix) %>% 
    sample_n(9E3,replace = TRUE) %>% 
    arrange(commute_ix) %>% 
    mutate(subject_id = 1:n()) %>% 
    right_join(s1hdata %>% 
                   st_drop_geometry() %>% 
                   filter(has_commute == 1) %>%
                   select(subject_id,origin_ix)) %>% 
    select(subject_id,origin_ix,commute_ix) %>% 
    arrange(subject_id,origin_ix,commute_ix) 

## take origin destinations from gtdf and use those to compute 
## trips to commute destinations amongst subsample of individuals 
## who do commute

corigins <- s1hdata %>%
    filter(has_commute == 1) %>%
    left_join(commute_samples %>% 
                  st_drop_geometry() %>% 
                  select(subject_id,origin_ix,commute_ix)) %>% 
    arrange(subject_id,origin_ix) %>% 
    distinct(origin_ix,commute_ix,geometry) %>% 
    st_coordinates()

cdestinations <- commute_samples %>% 
    distinct(origin_ix,commute_ix,geometry) %>% 
    st_coordinates()

create_tripdf <- function(odf,mode,origins,destinations,tripttype){
    tdf <- map_dfr(mode,function(md){
        trips <- stplanr::route(from = origins,
                                to = destinations,
                                route_fun = osrm::osrmRoute,
                                returnclass = "sf",
                                osrm.profile = md)
        out <- trips %>% 
            mutate(mode = md,
                   origin_ix = odf$origin_ix,
                   grocery_ix = odf$commute_ix,
                   triptype = tripttype) %>% 
            st_as_sf()
        return(out)
    })
    return(tdf)
}

ctdf <- create_tripdf(commute_samples %>% 
                          distinct(origin_ix,commute_ix,geometry),
                      mode = c("car","bike","foot"),
                      corigins,
                      cdestinations,
                      "Commute")



# Social Trips ------------------------------------------------------------



## somewhat "ad hoc" choice for arriving at social destinations
## may return to this later
szns <- read_sf("~/Documents/CityData/Burgh/Zoning/Zoning.shp") %>% 
    filter(str_detect(legendtype,"Parks|Riverfront|Educational|Commercial|Public|Triangle"),
           str_detect(legendtype,"Highway",negate = TRUE)) %>% 
    st_centroid() %>% 
    st_filter(hoods) %>% 
    transmute(social_ix = 1:n())

ssample <- sample(szns$social_ix,size = 9E3,replace = TRUE)


sorigins <- s1hdata %>%
    arrange(subject_id, origin_ix) %>% 
    left_join(szns[ssample,] %>% 
                  st_drop_geometry() %>% 
                  mutate(subject_id = 1:n())) %>% 
    distinct(origin_ix,social_ix,geometry) %>% 
    st_coordinates()

sdests <- szns[ssample,] %>% 
    mutate(subject_id = 1:n()) %>% 
    left_join(s1hdata %>% 
                  st_drop_geometry()) %>% 
    distinct(origin_ix,social_ix,geometry) %>% 
    st_transform(4326) %>% 
    st_coordinates()

odf <- szns[ssample,] %>% 
    mutate(subject_id = 1:n()) %>% 
    left_join(s1hdata %>% 
                  st_drop_geometry()) %>% 
    distinct(origin_ix,social_ix,geometry) %>% 
    st_drop_geometry()

create_tripdf <- function(odf,mode,origins,destinations,tripttype){
    tdf <- map_dfr(mode,function(md){
        trips <- stplanr::route(from = origins,
                                to = destinations,
                                route_fun = osrm::osrmRoute,
                                returnclass = "sf",
                                osrm.profile = md)
        out <- trips %>% 
            mutate(mode = md,
                   origin_ix = odf$origin_ix,
                   social_ix = odf$social_ix,
                   triptype = tripttype) %>% 
            st_as_sf()
        return(out)
    })
    return(tdf)
}

stdf <- create_tripdf(odf,
                      mode = c("car","bike","foot"),
                      sorigins,
                      sdests,
                      "Social")


# Process / Save Trip Data -----------------------------------------------------


s1tdf <- rbind(gtdf %>% 
                 mutate(triptype = "groceries") %>% 
                 rename(trip_ix = grocery_ix) %>% 
                 right_join(s1hdata %>% 
                                st_drop_geometry() %>% 
                                select(origin_ix,subject_id)),
             ctdf %>% 
                 rename(commute_ix = grocery_ix) %>% 
                 left_join(commute_samples %>% 
                               st_drop_geometry()) %>% 
                 rename(trip_ix = commute_ix),
             stdf %>% 
                 right_join(szns[ssample,] %>% 
                                mutate(subject_id = 1:n()) %>%  
                                st_drop_geometry() %>% 
                            left_join(s1hdata %>% 
                                        st_drop_geometry() %>% 
                                        select(origin_ix,subject_id)) 
                            ) %>% 
                 rename(trip_ix = social_ix)
             ) %>% 
    mutate(purpose = str_to_lower(triptype))

usethis::use_data(s1tdf,overwrite = TRUE)
