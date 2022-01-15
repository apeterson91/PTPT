## code to prepare `s1hdata` dataset goes here

library(tidyverse)
library(sf)


tmp <- pghcs %>% 
    st_drop_geometry() %>% 
    pivot_wider(id_cols = c("hood","num_tracts"),
                names_from = c("Q"),
                values_from = c("estimate","moe")) %>% 
    mutate(prop_Female = estimate_Female / (`estimate_Total Population`),
           prop_Asian = estimate_Asian / (`estimate_Total Population`),
           prop_Black = estimate_Black / (`estimate_Total Population`),
           prop_White = estimate_White / (`estimate_Total Population`),
           prop_Other = `estimate_>= 2 Races` /(`estimate_Total Population`)
           )

set.seed(34103)
s1hdata <- read_sf("~/Google Drive/CityData/Burgh/Neighborhoods/Neighborhoods_.shp") %>%
    st_drop_geometry() %>% 
    distinct(hood) %>% 
    pull(hood) %>% 
    map2_dfr(.,1:90,function(x,y){
        nt <- tmp %>% filter(hood == {{x}}) %>% pull(num_tracts)
        pf <- tmp %>% filter(hood == {{x}}) %>% pull(prop_Female)
        pa <- tmp %>% filter(hood == {{x}}) %>% pull(prop_Asian)
        pb <- tmp %>% filter(hood == {{x}}) %>% pull(prop_Black)
        pw <- tmp %>% filter(hood == {{x}}) %>% pull(prop_White)
        po <- tmp %>% filter(hood == {{x}}) %>% pull(prop_Other)
        medAge <- tmp %>% 
            filter(hood == {{x}}) %>% 
            pull(`estimate_Median Age`)
        medAge <- medAge / nt 
        moeAge <- tmp %>% 
            filter(hood == {{x}}) %>% 
            pull(`moe_Median Age`)
        moeAge <- moeAge / nt
        medInc <- tmp %>% 
            filter(hood == {{x}}) %>% 
            pull(`estimate_Median Income`)
        medInc <- medInc / nt
        moeInc <- tmp %>% filter(hood == {{x}}) %>% 
            pull(`moe_Median Income`)
        moeInc <- moeInc / nt
        
       tibble(hood = x,
              subject_id = ((y-1)*100) + 1:100,
              subject_hood_id = 1:100,
              Female = rbinom(100,size = 1,prob = pf),
              Age = truncnorm::rtruncnorm(100,a = 18,b = 100,mean = medAge,sd = moeAge),
              AgeCat = cut(Age, breaks = c(18,24,34,40,50,60,70,80,90,100)),
              Income = truncnorm::rtruncnorm(100,
                                             a = 0,
                                             mean = medInc/1E3, 
                                             ## add noise for zero cell entries
                                             sd = moeInc/1E3 + 1E-6),
              Race = sample(c("White","Black","Asian","Other"),
                            size = 100,
                            replace = TRUE,
                            prob = c(pw,pb,pa,po)),
              IncomeCat = cut(Income, breaks = c(0,20,40,60,
                                                 80,100,150,500)),
              ) 
    })
    
    

usethis::use_data(s1hdata, overwrite = TRUE)
