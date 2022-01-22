## code to prepare `s1sdata` dataset goes here


## snap trips to street segments

foo <- s1tdf[1,] %>% 
    st_transform(st_crs(streets))

bar <- foo %>% 
    st_segmentize(dfMaxLength = 50) %>% 
    st_cast(.,"POINT")

ics <- st_nearest_feature(bar,streets)

mapview::mapview(streets[ics,])

## Current method for determining street *segments*
## associated with each trip -- this is not 
## a perfect method. As street segments not 
## associated with the actual route are sometimes 
## included because of proximity
## I had an idea to fix this but 
## have forgot it at present. When I remember 
## I will return to fix this. 
## Given that this is already "simulated data"
## I proceed with the current method in 
## spirit of not letting the perfect be the 
## enemy of the good (enough).
gtpdf <- gtdf %>% 
    mutate(trip_ix = 1:n()) %>% 
    st_transform(st_crs(streets)) %>% 
    st_segmentize(100) %>% 
    st_cast(.,"POINT") %>% 
    group_by(trip_ix) %>% 
    split(.$trip_ix) %>% 
    map_dfr(.,function(x){ 
        tibble(trip_ix = unique(x$trip_ix),
               street_ix = st_nearest_feature(x,streets)) %>% 
            distinct(trip_ix,street_ix) %>% 
            mutate(order_ix = 1:n())
    })

gteldf <- gtdf %>% 
    mutate(trip_ix = 1:n()) %>% 
    st_segmentize(100) %>% 
    st_cast(.,"POINT") %>% 
    mutate(elevation = elevatr::get_elev_point(., src= "aws") %>% 
                                   pull(elevation))

gteldf <- gteldf %>% 
    mutate(ptdst = map2_dbl(row_number(),row_number()+1,function(x,y){
        if(x == n()) return(NA)
       st_distance(.[x,],.[y,])
       })
    ) 

s1eldf <- gteldf %>% 
    mutate(lagelev = lag(elevation),
           elevdiff = (lagelev - elevation) / ptdst) %>% 
    group_by(trip_ix) %>% 
    summarise(mngrad = round(100*mean(elevdiff,na.rm=T)))

usethis::use_data(s1eldf)

s1sdata <- gtdf %>% 
    mutate(trip_ix = 1:n()) %>% 
    st_drop_geometry() %>% 
    distinct(hood,subject_hood_id,
             origin_ix,grocery_ix,
             trip_ix,tx,ty) %>%   
    left_join(gtpdf) %>%
    rename(origin_hood = hood) %>% 
    st_as_sf(coords=c("tx","ty")) %>% 
    st_set_crs(4326) %>% 
    st_join(hoods %>% 
                select(hood) %>%
                st_transform(4326) %>% 
                rename(dest_hood = hood)) %>% 
    ## one off replacements - may fix more holistically later
    mutate(dest_hood = replace_na(dest_hood,"Mount Oliver"),
           origin_hood = replace_na(origin_hood, "Banksville")) %>% 
    st_drop_geometry()




usethis::use_data(s1sdata, overwrite = TRUE)