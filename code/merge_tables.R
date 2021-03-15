library(tidyverse)
library(sf)

states <- c("vic","nsw","act","qld","nt","wa","sa","tas")
aussie_maps_data <- "../aussiemaps/inst/extdata/"
aussie_file_data <- "../aussiemaps/data/"


sa1.locations.table <- map_dfr(states, function(x){
                    filename <- "sa1_table.csv"
                    read_csv(str_c(x,"/",filename),col_types="ccccccccccccccccccd")

})

locations.table <- sa1.locations.table %>% select(-ends_with("_2016"),-AREA) %>% unique(.)

save(sa1.locations.table,file=str_c(aussie_file_data,"sa1.locations.rda"))
save(locations.table,file=str_c(aussie_file_data,"locations.rda"))


for(i in states){
  
          files <- dir(str_c(i,"/"),pattern=".rds")
          files <- str_remove(files,".rds")

          for(j in files){
          object<-readRDS(str_c(i,"/",j,".rds")) %>% st_collection_extract("POLYGON")
          save(object,file=str_c(aussie_maps_data,i,"_",j,".rda"))
          }
}