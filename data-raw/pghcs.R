## code to prepare `pghcs` dataset goes here

library(tidycensus)

## need census api key
# census_api_key("YOUR KEY HERE")
pgh_wealth <- get_acs(state = "PA", 
                      county = "Allegheny",
                      geography = "tract",
                      variables = c("B19013_001", #Income
                                    "B01001_001", ## Total
                                    "B01001_002", ## Male
                                    "B01001_026", ## Female
                                    "B02001_002", ## White
                                    "B02001_003", ## Black
                                    "B02001_005",## Asian
                                    "B02001_008", ## >=2 Races
                                    "B01002_001", # Median Age
                                    ),
                      geometry = TRUE)

hoods <- sf::read_sf("~/Google Drive/CityData/Burgh/Neighborhoods/Neighborhoods_.shp") %>%
    select(objectid,hood) %>%
    sf::st_transform(st_crs(pgh_wealth))

hoods <- hoods %>% 
    mutate(area = st_area(hoods)/1E3)

csgeos <- pgh_wealth %>% st_filter(hoods) %>% 
    st_drop_geometry() %>% 
    distinct(GEOID) %>% as_tibble() %>% 
    left_join(pgh_wealth %>% 
                  filter(variable == "B19013_001") %>% 
                  select(GEOID)) %>% 
    st_as_sf()

cshood_ovl <- csgeos %>% 
    split(.$GEOID) %>% 
    map_dfr(.,function(x){
        foo <- x %>% 
            st_intersection(hoods)
        tibble(GEOID = x$GEOID,
               hood = foo$hood,
               ## km^2
               ovlarea = as.numeric(st_area(foo)/1E3),
               pctarea = as.numeric((ovlarea/foo$area)*100)
               )
    }
        )

## Census tracts do not overlap completely with pittsburgh neighborhoods
## here I use a simple heuristic to find which combination of census tracts 
## collectively overlap with the neighborhood with the smallest 
## tract included contributing greater than 1% of area.
cshdf <- cshood_ovl %>% 
    arrange(hood,desc(pctarea)) %>% 
    group_by(hood) %>% 
    mutate(CumulativeArea = cumsum(pctarea),
           LagCumArea = lag(CumulativeArea), 
           LagDiff = CumulativeArea - LagCumArea,
           gix = row_number(),
           endpoint = case_when(CumulativeArea > 90 & pctarea > 1  ~ as.numeric(gix))
    ) %>% 
    fill(endpoint,.direction = c("downup")) %>% 
    filter(gix<=endpoint) %>% 
    select(GEOID,hood) %>% 
    ungroup()
    

## Aggregate statistics to neighborhood level
## take mean of medians 
pghcs <- pgh_wealth %>% 
    right_join(cshdf) %>% 
    mutate(Q = case_when(variable == "B19013_001" ~ "Median Income",
                         variable == "B01001_001" ~ "Total Population",
                         variable == "B01001_002" ~ "Male",
                         variable == "B01001_026" ~ "Female",
                         variable == "B02001_002" ~ "White",
                         variable == "B02001_003" ~ "Black",
                         variable == "B02001_005" ~ "Asian",
                         variable == "B02001_008" ~ ">= 2 Races",
                         variable == "B01002_001" ~ "Median Age"
                         )) %>% 
    group_by(hood,Q) %>% 
    summarise(estimate = sum(estimate, na.rm = TRUE),
              ## note that these moes are better but not perfect
              ## see https://walker-data.com/tidycensus/articles/margins-of-error.html
              moe = moe_sum(moe,estimate,na.rm=TRUE),
              num_tracts = n()) %>% 
    ungroup()



usethis::use_data(pghcs, overwrite = TRUE)
