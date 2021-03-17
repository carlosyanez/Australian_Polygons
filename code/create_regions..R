library(tidyverse)
library(readxl)

locations.table <- readRDS("locations.rds")
aussie_maps_data <- "../aussiemaps/inst/extdata/"



# Official Regions 

region_names <- locations.table %>% select(State.Region) %>% unique(.) %>% pull(.)

regions <- map_df(region_names, function(x,loc.table){
                  locations.table %>%
                  filter(State.Region==x) %>% 
                  select(LGA_PID,State) %>% unique(.) %>% mutate(Region=x)  
        
},locations.table)
  

extra_regions_source <- readxl::read_xlsx("Regions.xlsx") 
extra_regions_source <- extra_regions_source %>% mutate(LGA=str_trim(LGA))

extra_regions<- extra_regions_source %>% inner_join(locations.table %>% select(LGA,State,LGA_PID) %>% unique(.), by=c("LGA","State"))

extra_regions_source %>% filter(!(LGA %in% extra_regions$LGA))

regions <- rbind(regions,extra_regions %>% select(-LGA))

save(regions,file=str_c(aussie_maps_data,"regions.rda"))

