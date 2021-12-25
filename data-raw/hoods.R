## code to prepare `hoods` dataset goes here

library(dplyr)

## Taken from Univ. of Pittsburgh 
#hood_pop <- vroom::vroom("~/Google Drive/CityData/Burgh/hood_population.csv")


## data taken from https://data.wprdc.org/dataset/neighborhoods2
hoods <- sf::read_sf("~/Google Drive/CityData/Burgh/Neighborhoods/Neighborhoods_.shp") %>%
    select(objectid,hood) %>%
    sf::st_transform(4326) %>%
    #left_join(hood_pop) %>%
    ## prob taken from https://bikeleague.org/sites/default/files/LAB_Where_We_Ride_2016.pdf
    ## used in Stage 0
    ##TODO(petersonadam): Need to update proportion below to be counts with weights
    ## to better mimic survey data
    mutate(work_cycles = rbeta(n = n(), shape1 =  26 , shape2 = 1000 ))

usethis::use_data(hoods, overwrite = TRUE)
