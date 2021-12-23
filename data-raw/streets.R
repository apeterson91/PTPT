## code to prepare `streets` dataset goes here

hoods <- read_sf("~/Google Drive/CityData/Burgh/Neighborhoods/Neighborhoods_.shp") %>%
    select(objectid,hood) %>%
    st_transform(4326) %>%
    left_join(hood_pop) %>%
    ## prob taken from https://bikeleague.org/sites/default/files/LAB_Where_We_Ride_2016.pdf
    ## used in Stage 0
    mutate(cycles = rbeta(n = n(), shape1 =  26 , shape2 = 1000 ))

streets <- read_sf("~/Documents/CityData/Burgh/alleghenycounty_streetcenterlines202107/AlleghenyCounty_StreetCenterlines202107.shp") %>%
  st_transform(st_crs(hoods)) %>%
  st_filter(hoods) %>%
  st_join(hoods %>% select(hood)) %>%
  select(OBJECTID,FULL_NAME,hood,geometry)

usethis::use_data(streets, overwrite = TRUE)
