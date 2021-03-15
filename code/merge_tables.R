library(tidyverse)
states <- c("vic","nsw","act","qld","nt","wa","sa","tas")

sa1_table <- map_dfr(states, function(x){
                    filename <- "sa1_table.csv"
                    read_csv(str_c(x,"/",filename),col_types="ccccccccccccccccccd")

})

write_csv(sa1_table,"sa1_table.csv")