## code to prepare `streets` dataset goes here

library(dplyr)

hoods <- sf::read_sf("~/Google Drive/CityData/Burgh/Neighborhoods/Neighborhoods_.shp") %>%
    select(objectid,hood) %>%
    sf::st_transform(4326) %>%
    ## prob taken from https://bikeleague.org/sites/default/files/LAB_Where_We_Ride_2016.pdf
    ## used in Stage 0
    mutate(cycles = rbeta(n = n(), shape1 =  26 , shape2 = 1000 ))

streets <- sf::read_sf("~/Google Drive/CityData/Burgh/alleghenycounty_streetcenterlines202107/AlleghenyCounty_StreetCenterlines202107.shp") %>%
  sf::st_transform(sf::st_crs(hoods)) %>%
  sf::st_filter(hoods) %>%
  sf::st_join(hoods %>% select(hood)) %>%
  select(OBJECTID,FULL_NAME,hood,geometry)

usethis::use_data(streets, overwrite = TRUE)
