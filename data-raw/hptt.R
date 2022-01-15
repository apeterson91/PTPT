## code to prepare `hptt` dataset goes here

library(tidyverse)
library(sf)

diffdf <- gtdf %>% 
    st_drop_geometry() %>% 
    as_tibble() %>% 
    select(origin_ix,grocery_ix,subject_hood_id,hood,mode,duration) %>% 
    arrange(origin_ix,grocery_ix) %>% 
    spread(mode,duration) %>% 
    mutate(fcdiff = foot - car,
           fcpdiff = (fcdiff / car)*100,
           bcdiff = bike - car,
           bcpdiff = (bcdiff / car )*100) %>% 
    select(-bike,-car,-foot)


    

hptt <- s1sdata %>% 
    distinct(subject_hood_id,
             origin_ix,grocery_ix,
             origin_hood,dest_hood) %>% 
    left_join(s1hdata,by = c("origin_hood"="hood",
                             "subject_hood_id"="subject_hood_id")) %>% 
    left_join(diffdf) %>% 
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
    ) %>% 
    arrange(subject_id) %>% 
    select(subject_id,everything()) %>% 
    crossing(tibble(purpose = c("groceries","social","commute")))

usethis::use_data(hptt, overwrite = TRUE)
