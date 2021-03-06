### Create spatial polygons for different divisions in Victoria, Australia
# Load/Install pacman
if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")

#use pacman to install all other packages
pacman::p_load("tidyverse","rgdal","sf","lwgeom","spdep","geojsonsf","rgeos","smoothr",
               "rvest","xml2","stringi")




## get list of  councils across state
area_tolerance <-10^4
area_tolerance2 <-10^6


regions <- c("Greater Metropolitan Melbourne","Barwon South West","Grampians",
             "Gippsland","Hume","Loddon Mallee")
wiki_page <- read_html("https://en.wikipedia.org/wiki/Local_government_areas_of_Victoria")

clean_lga <-function(df){
  
  df %>%   mutate(LGA=stri_trans_totitle(tolower(LGA)),
                  LGA=str_remove(LGA,"City of"),
                  LGA=str_remove(LGA,"Shire of"),
                  LGA=str_remove(LGA,"Shire"),
                  LGA=str_remove(LGA,"City"),
                  LGA=str_remove(LGA,"Rural"),
                  LGA=str_remove(LGA,"Borough of"),
                  LGA=str_remove(LGA,"Borough"),
                  LGA=str_remove(LGA,"\\(Unincorporated\\)"),
                  LGA=str_remove(LGA,"\\(Uninc\\)"),
                  LGA=str_squish(LGA),
                  LGA=str_trim(LGA)) 
}
#https://gis.stackexchange.com/questions/321281/using-sf-to-combine-polygons-that-share-borders



##special areas - unincorporated regions

uninc_areas <- tribble(~LGA,~State.Region,
                       "Mount Buller Alpine Resort","Hume",
                       "Mount Stirling Alpine Resort","Hume",
                       "Falls Creek Alpine Resort","Hume",
                       "Lake Mountain Alpine Resort","Hume",
                       "Mount Hotham Alpine Resort ","Hume",
                       "Mount Baw Baw Alpine Resort","Gippsland",
                       "French-Elizabeth-Sandstone Islands","Gippsland"
                       ) %>% 
                mutate(Metro.Region="Unincorporated",ABB_NAME=toupper(LGA))

### economic areas




#table for melbourne is different....

melb_lgas_list <- wiki_page %>%
  html_node(xpath="/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>%
  html_table(fill = TRUE) %>% 
  rename(LGA=`Local government area`,
         Metro.Region=Region) %>%
  clean_lga() %>%
  dplyr::select(LGA,Metro.Region) %>%
  mutate(ABB_NAME=toupper(LGA),
         State.Region=regions[1])

# other regions

lgas_list <- map_dfr(2:6, function(x,regions,wiki_page){
  
  wiki_page %>%
  html_node(xpath=str_c("/html/body/div[3]/div[3]/div[5]/div[1]/table[",x,"]")) %>%
  html_table(fill = TRUE) %>% 
  .[!duplicated(as.list(.))] %>%
  slice(-1) %>%
  rename(LGA=`Local government area`) %>%
  clean_lga() %>%
  dplyr::select(LGA) %>%
  mutate(ABB_NAME=toupper(LGA),
         State.Region=regions[x],
         Metro.Region="")
},regions,wiki_page)

# put together and clean up

lgas_list <- rbind(lgas_list,melb_lgas_list) %>% rbind(uninc_areas)

rm(regions,melb_lgas_list,wiki_page,uninc_areas)

#Download all shapefiles

## download victorian LGAs

if(!file.exists("VIC_LGA_POLYGON_SHP.shp")){
  download.file("https://data.gov.au/data/dataset/bdf92691-c6fe-42b9-a0e2-a4cd716fa811/resource/7b6043d1-76b8-4ea9-b36b-51c61aa740d0/download/vic_lga_polygon_shp.zip",
                "LGAs.zip")
  unzip("LGAs.zip")
  file.remove("LGAs.zip")
}

vic_lga_polygon <- st_read("VIC_LGA_POLYGON_SHP.shp")


## get suburb polygons

if(!file.exists("VIC_LOCALITY_POLYGON_SHP.shp")){
  download.file("https://data.gov.au/data/dataset/af33dd8c-0534-4e18-9245-fc64440f742e/resource/3b946968-319e-4125-8971-2a33d5bf000c/download/vic_locality_polygon_shp.zip",
                "LGAs.zip")
  unzip("LGAs.zip")
  file.remove("LGAs.zip")
}

vic_suburb_polygon <- st_read("VIC_LOCALITY_POLYGON_SHP.shp")


#postal areas from ABS
 
if(!file.exists("POA_2016_AUST.shp")){
  download.file("https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_poa_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&4FB811FA48EECA7ACA25802C001432D0&0&July%202016&13.09.2016&Previous","pc.zip")
  unzip("pc.zip")
  file.remove("pc.zip")
}  
  
aus_poas_polygon <- st_read("POA_2016_AUST.shp")

# check suburb/lga crossover
# check suburbs fully inside LGAs

sl_fully_covered <- st_covered_by(vic_suburb_polygon,vic_lga_polygon)

names(sl_fully_covered) <- vic_suburb_polygon$NAME

sl_fc <- tibble(LOC_PID=character(),LGA_PID=character(),LGA=character())
for(i in 1:length(sl_fully_covered)){
  if(length(sl_fully_covered[i])>0)
  sl_fc <- sl_fc %>% add_row(LOC_PID=vic_suburb_polygon[i,]$LOC_PID,
                             LGA_PID=vic_lga_polygon[sl_fully_covered[[i]],]$LGA_PID,
                             LGA=vic_lga_polygon[sl_fully_covered[[i]],]$LGA_NAME)
}
sl_fully_covered <- sl_fc %>% clean_lga()
rm(i,sl_fc)

#check intersections between suburb LGa for remnant

sl_in <- st_intersection(vic_suburb_polygon,vic_lga_polygon) %>%
                         mutate(LGA=LGA_NAME) %>% clean_lga() %>% select(-LGA_NAME) %>%
        filter(!(LOC_PID %in% sl_fully_covered$LOC_PID))

sl_in <- sl_in %>%   st_collection_extract("POLYGON")               #keep polygons only


sl_in$area <- sl_in %>% st_area()
attributes(sl_in$area) <- NULL
sl_in <- sl_in %>% filter(!(area==0)) %>% unique(.)

# calculate area and filter small pieces (a block)

sl_in_count<- sl_in %>%  filter(area>area_tolerance) 
class(sl_in_count) <- "data.frame"
sl_in_count <- sl_in_count %>% select(-geometry) %>% select(LGA_PID,LOC_PID,LGA)
sl_in_count2 <- sl_in_count %>% count(LOC_PID) %>% filter(n==1) 

# check which suburbs are (almost) fully contain in one LGA and add to list

sl_fully_covered <- rbind(sl_fully_covered,
                      sl_in_count %>% filter(LOC_PID %in% sl_in_count2$LOC_PID)) %>% unique(.)
rm(sl_in_count,sl_in_count2)

# determine remnant and tag large pieces

sl_in_remnant <-sl_in %>% filter(!(LOC_PID %in% sl_fully_covered)) %>%
                          mutate(relevant=(area>area_tolerance2))

# check where all areas are relevant

sl_all_revelant <- as.data.frame(sl_in_remnant) %>% 
  group_by(LOC_PID) %>%
  summarise(n=n(),relevant=sum(relevant)) %>%
  mutate(diff=n-relevant) %>% filter(diff==0)

# if all areas are relevant, extract (nothing else to do)
sl_mixed <- sl_in_remnant %>% filter(LOC_PID %in% sl_all_revelant$LOC_PID & relevant)
rm(sl_all_revelant)
# new remnant

plot(sl_mixed %>% filter(LOC_PID=="VIC500" & LGA_PID=="VIC224") %>% select(area))

##consolidate all areas and filter what's left

sl_mixed <-rbind(sl_mixed,
                 sl_in_remnant %>% filter(relevant)) %>% unique(.)

vic_loc_lga <-sl_mixed %>% clean_lga() %>%
  mutate(PID=str_c(LGA_PID,LOC_PID,sep="-")) %>%
  select(PID,LGA_PID,LOC_PID,NAME,LGA) %>% u

vic_loc_lga <-rbind(sl_mixed %>% select(LGA_PID,LOC_PID,NAME,LGA_NAME),
                    vic_suburb_polygon %>% 
                      left_join(sl_fully_covered,by="LOC_PID") %>% 
                      filter(!is.na(LGA_PID)) %>%
                      select(LGA_PID,LOC_PID,NAME,LGA_PID,LGA_NAME)) %>%
              mutate(PID=str_c(LGA_PID,LOC_PID,sep="-"))

sl_in_remnant <- sl_in %>% mutate(PID=str_c(LGA_PID,LOC_PID,sep="-")) %>%
                 filter(!(PID %in% vic_loc_lga$PID)) %>%
                 select(LGA_PID,LOC_PID,NAME,LGA,PID)

sl_in_remnant$area <- sl_in_remnant %>% st_area()
attributes(sl_in_remnant$area) <- NULL
sl_in_remnant <- sl_in_remnant %>% filter(!(area==0))
sl_in_remnant <- sl_in_remnant %>% mutate(relevant=(area>area_tolerance))

#move relevant to main map

vic_loc_lga <- rbind(vic_loc_lga,
                     sl_in_remnant %>% filter(relevant) %>% select(-relevant,-area))

sl_in_remnant <- sl_in_remnant %>% filter(!relevant) %>% select(-relevant,-area)

##check
#datax=tibble(datax=sl_in_remnant %>% st_area())
#attributes(datax$datax) <- NULL
#ggplot(data=datax,aes(x=datax)) + geom_histogram()

vic_loc_lga <- vic_loc_lga %>% mutate(LGA=stri_trans_totitle(tolower(LGA)),
                       Locality=stri_trans_totitle(tolower(NAME))) %>%
                clean_lga() %>% select(-LGA_NAME) %>%
                left_join(lgas_list,by="LGA") %>% unique(.)
                group_by(LGA_PID,LOC_PID,NAME,PID,LGA,Locality,ABB_NAME,State.Region,Metro.Region) %>%
                summarise() %>%
                st_union() %>%
                mutate(area=st_area(.))  
                vic_loc_lga <- unique(vic_loc_lga)

               arrange(desc(area)) %>%
                slice_head(n=1) %>%
                mutate(State="VIC")
   
               sl_mixed %>% count(LOC_PID,LGA_PID) %>% arrange(desc(n))        

plot(vic_loc_lga %>% filter(PID=="VIC218-VIC1772")%>% select(area))
#rm(list=ls()[! ls() %in% c("vic_loc_lga","aus_poas_polygon","clean_lga")])


##postcodes
vic_lga <- vic_loc_lga %>%
  group_by(LGA,State,State.Region,Metro.Region,LGA_PID) %>% 
  summarise()

vic_region <- vic_lga %>%
  group_by(State,State.Region) %>% 
  summarise()

vic_loc <- vic_loc_lga %>%
  group_by(NAME,State,State.Region,Metro.Region,LOC_PID) %>% 
  summarise()

vic_state <- vic_region %>%
             group_by(State) %>%
             summarise()

plot(vic_state %>% select(State))


#sf_geojson(vic_loc_lga)
saveRDS(vic_loc_lga,"victoria/vic_loc_lga.rds")
#sample plot
ggsave("victoria/vic_loc_lga.png",plot(vic_loc_lga %>% 
                                         filter(State.Region=="Greater Metropolitan Melbourne") %>%
                                         select(LGA)))
saveRDS(vic_loc_lga,"victoria/vic_lga.rds")
saveRDS(vic_loc_lga,"victoria/vic_lga.rds")


#state electorates

#federal electorates

