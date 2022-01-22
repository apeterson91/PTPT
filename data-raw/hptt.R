## code to prepare `hptt` dataset goes here

library(tidyverse)
library(sf)

diffdf <- s1tdf %>% 
    st_drop_geometry() %>% 
    st_as_sf(.,coords=c("tx","ty")) %>% 
    st_set_crs(4326) %>% 
    st_join(hoods %>% 
                select(hood) %>% 
                rename(dest_hood = hood) %>% 
                st_transform(4326),
            join = st_nearest_feature
            ) %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    st_as_sf(.,coords=c("fx","fy")) %>% 
    st_set_crs(4326) %>% 
    st_join(hoods %>% 
                select(hood) %>% 
                rename(origin_hood = hood) %>% 
                st_transform(4326)) %>% 
    select(subject_id,origin_ix,trip_ix,origin_hood,
           dest_hood,purpose,mode,duration) %>% 
    st_drop_geometry() %>% 
    arrange(subject_id,origin_ix,trip_ix,
            origin_hood,dest_hood,purpose,mode) %>% 
    spread(mode,duration) %>% 
    mutate(fcdiff = foot - car,
           fcpdiff = (fcdiff / car)*100,
           bcdiff = bike - car,
           bcpdiff = (bcdiff / car )*100) %>% 
    select(-bike,-car,-foot)


    

hptt <- diffdf %>%  
    left_join(s1hdata %>% 
                  select(-hood) %>% 
                  st_drop_geometry(),
              by = "subject_id") %>% 
    as_tibble() %>% 
    mutate(
           bodds = -5 + 
               .5* (IncomeCat == "(0,20]") + 
               .25 *(IncomeCat == "(20,40]") -
               .5 * Female + 
               3*exp(- (bcdiff>0)*bcdiff / 3),
           fodds =  -3 +
               .5* (IncomeCat == "(0,20]") + 
               .25 *(IncomeCat == "(20,40]") -
               .5 * Female + 
               3*exp(- (fcdiff>0)*fcdiff / 3),
           bodds = binomial()$linkinv(bodds),
           fodds = binomial()$linkinv(fodds),
           mode = map2_chr(bodds,fodds,function(x,y){
               sample(x = c("bike","foot","car"),
                         replace = TRUE,
                      size = 1,
                         prob = c(x / (1 + x + y),
                                  y / (1 + x + y),
                                  1 / (1 + x + y) ))
           })
    ) 

usethis::use_data(hptt, overwrite = TRUE)
