## code to prepare `s0hdata` dataset goes here

library(tidyverse)
library(sf)

set.seed(34103)
s0hdata <- read_sf("~/Google Drive/CityData/Burgh/Neighborhoods/Neighborhoods_.shp") %>%
    st_drop_geometry() %>% 
    distinct(hood) %>% 
    pull(hood) %>% 
    map2_dfr(.,1:90,function(x,y){
       tibble(hood = x,
              subject_id = ((y-1)*100) + 1:100,
              Female = rbinom(100,size = 1,prob = .5),
              AgeCat = sample(c("0-17","18-24","25-34","35-40",
                                "41-50","51-60","61-70",">70"),
                              size = 100, replace = TRUE,
                              prob = c(.05,.25,.25,.20,.20,.5,.025,.025)),
              Race = sample(c("Caucasian","African American","Asian","Other"),
                            size = 100,
                            replace = TRUE,
                            prob = c(.75,.15,.06,.04)),
              IncomeCat = sample(c("0-20","20-40","41-60",
                                   "61-80","81-100","100-150",">150"),
                                 size = 100,
                                 replace = TRUE,
                                 prob = c(.05,.25,.325,.20,.10,.05,.025)
                                ),
              HouseHoldSize = sample(1:5,
                                     size = 100,
                                     replace = TRUE,
                                     prob = c(.1,.35,.35,.15,.05))
              ) 
    })
    
    

usethis::use_data(s0hdata, overwrite = TRUE)
