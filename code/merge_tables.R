library(tidyverse)
library(sf)

states <- c("vic","nsw","act","qld","nt","wa","sa","tas")
#states <-c("vic")
aussie_maps_data <- "../aussiemaps/inst/extdata/"
aussie_file_data <- "../aussiemaps/data/"


sa1.locations.table <- map_dfr(states, function(x){
                    filename <- "sa1_table.csv"
                    read_csv(str_c(x,"/",filename),col_types="ccccccccccccccccccd")

})

#locations.table <- sa1.locations.table %>% select(-ends_with("_2016"),-AREA) %>% unique(.)
save(sa1.locations.table,file=str_c(aussie_file_data,"sa1.locations.rda"))
#save(locations.table,file=str_c(aussie_file_data,"locations.rda"))

locations.table.list <- list()

for(i in states){
  
          files <- dir(str_c(i,"/"),pattern=".rds")
          files <- str_remove(files,".rds")

          for(j in files){
          object<-readRDS(str_c(i,"/",j,".rds")) %>% st_collection_extract("POLYGON")
          save(object,file=str_c(aussie_maps_data,i,"_",j,".rda"))
          
          locations.table.list[[i]] <- as.data.frame(object) %>% select(-geometry)

          }
}

locations.table <- bind_rows(locations.table.list)
saveRDS(locations.table,"locations.rds")
save(locations.table,file=str_c(aussie_file_data,"locations.rda"))


state_names<- tribble(~State_short,~State,
                      "VIC","Victoria",
                      "NSW","New South Wales",
                      "ACT","Australian Capital Territory",
                      "QLD","Queensland",
                      "NT","Northern Territory",
                      "WA","Western Australia",
                      "SA","South Australia",
                      "TAS","Tasmania")

save(state_names,file=str_c(aussie_file_data,"state_names.rda"))


