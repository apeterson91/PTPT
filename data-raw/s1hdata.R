## code to prepare `s1hdata` dataset goes here

library(tidyverse)
library(sf)


## TODO: add in education
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
              Education = sample(c("Some High School","High School",
                                   "Some College/Technical School",
                                   "Technical Certification", 
                                   "2-year","4-year","Post-Grad"),
                                 size = 100,
                                 replace = TRUE, prob = c(.01,.15,
                                                          .15,.15,.15,.25,.15)),
              household_indiv = rep(1,100),
              BusComfort = sample(c("Don't","Do"), size = 100 , replace = TRUE,
                                  prob = c(.85,.15)),
              RailComfort = sample(c("Don't","Do"), 
                                   size = 100, prob = c(.9,.1),
                                   replace = TRUE),
              BikeComfort = sample(c("Don't","Do"),size = 100,replace = TRUE),
              BikeInfra = sample(0:5,size = 100, replace = TRUE),
              ) 
    })

## add fictious residences

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

## some geometries include duplicates
dups <- map_dbl(st_equals(pcls,pcls),function(x) x[[1]])

pcls <- pcls %>% 
    mutate(dups = dups) %>% 
    filter(origin_ix == dups) %>% 
    mutate(origin_ix = 1:n())

pcl_samples <- pcls %>% 
    select(hood,origin_ix) %>% 
    group_by(hood) %>% 
    sample_n(100,replace = TRUE) %>% 
    mutate(subject_hood_id = 1:n()) %>% 
    ungroup() 

usethis::use_data(pcl_samples, overwrite = TRUE)

s1hdata <- s1hdata %>% 
    left_join(pcl_samples) %>% 
    mutate(has_commute = map_int(Age,function(x){
                        case_when(x <= 65  ~ rbinom(1,1,0.7),
                                  TRUE ~ 0L)
                        })
          ) %>% 
    st_as_sf()

## fill in North/South Shore residential places with random sample 
sscords <- hoods %>% 
    filter(hood == "South Shore") %>% 
    st_sample(100) %>% 
    st_transform(4326) %>% 
    st_coordinates()

sscords <- hoods %>% 
    filter(hood == "South Shore") %>% 
    select(hood) %>% 
    st_drop_geometry() %>% 
    crossing(tibble(origin_ix = seq(from = max(pcls$origin_ix)+1,
                                    to = (max(pcls$origin_ix)+100),
                                    by = 1)))  %>% 
    mutate(fx = sscords[,1],fy = sscords[,2])
    
nscords <- hoods %>% 
    filter(hood == "North Shore") %>% 
    st_sample(100) %>% 
    st_transform(4326) %>% 
    st_coordinates()

nscords <- hoods %>% 
    filter(hood == "North Shore") %>% 
    select(hood) %>% 
    st_drop_geometry() %>% 
    crossing(tibble(origin_ix = seq(from = max(pcls$origin_ix)+101,
                                    to = (max(pcls$origin_ix)+200),
                                    by = 1)))  %>% 
    mutate(fx = nscords[,1],fy = nscords[,2])

nsshdf <- rbind(nscords,sscords) %>% 
    mutate(subject_id = s1hdata %>% 
               filter(hood == "North Shore" | hood == "South Shore") %>% 
               pull(subject_id))

s1hdata <- s1hdata %>% 
    filter(str_detect(hood,"[North|South] Shore", negate = TRUE)) %>% 
    rbind(.,
        s1hdata %>% 
        filter(str_detect(hood,"[North|South] Shore")) %>% 
        select(-origin_ix) %>% 
        st_drop_geometry() %>% 
        left_join(nsshdf) %>% 
        st_as_sf(coords = c("fx","fy")) %>% 
        st_set_crs(4326)
    )
    


usethis::use_data(s1hdata, overwrite = TRUE)
