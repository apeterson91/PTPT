## code to prepare `streets` dataset goes here


hoods <- sf::read_sf("~/Google Drive/CityData/Burgh/Neighborhoods/Neighborhoods_.shp") %>%
    dplyr::select(objectid,hood) %>%
    sf::st_transform(4326) %>%
    ## prob taken from https://bikeleague.org/sites/default/files/LAB_Where_We_Ride_2016.pdf
    ## used in Stage 0
    dplyr::mutate(cycles = rbeta(n = dplyr::n(), shape1 =  26 , shape2 = 1000 ))

streets <- sf::read_sf("~/Google Drive/CityData/Burgh/alleghenycounty_streetcenterlines202107/AlleghenyCounty_StreetCenterlines202107.shp") %>%
  sf::st_transform(sf::st_crs(hoods)) %>%
  sf::st_filter(hoods) %>%
  sf::st_join(hoods %>% dplyr::select(hood)) %>%
  dplyr::select(OBJECTID,FULL_NAME,hood,geometry) %>% 
  ## TODO: replace street ix with random numbers
  ## for privacy 
  dplyr::mutate(street_ix = 1:dplyr::n())

usethis::use_data(streets, overwrite = TRUE)
