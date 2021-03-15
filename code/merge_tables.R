library(tidyverse)
states <- c("vic","nsw","act","qld","nt","wa","sa","tas")
aussie_maps_data <- "../aussiemaps/inst/extdata/"

sa1_table <- map_dfr(states, function(x){
                    filename <- "sa1_table.csv"
                    read_csv(str_c(x,"/",filename),col_types="ccccccccccccccccccd")

})

write_csv(sa1_table,str_c(aussie_maps_data,"aus_sa1_table.csv"))

for(i in states){
  
          files <- dir(str_c(i,"/"),pattern=".rds")
          files <- str_remove(files,".rds")

          for(j in files){
          object<-readRDS(str_c(i,"/",j,".rds"))
          save(object,file=str_c(aussie_maps_data,i,"_",j,".rda"))
          }
}