### Create spatial polygons for different divisions in Queensland, Australia
# Load/Install pacman
if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")
if(!require(absmapsdata)) devtools::install_github("wfmackey/absmapsdata")

#use pacman to install all other packages
pacman::p_load("tidyverse","rgdal","sf","lwgeom","spdep","geojsonsf","rgeos","smoothr",
               "rvest","xml2","stringi","units","absmapsdata")

source("code/clean_lga.R")



## get list of  councils across state
area_tolerance <-set_units(10^4,m^2)
area_tolerance2 <-set_units(10^4,m^2)
area_tolerance3 <-set_units(0.3,1)

State <-"SA"
State_folder <- "sa/"
State_poa <- c(as.character(5000:5999),"0872")



wiki_page <- read_html("https://en.wikipedia.org/wiki/Local_government_areas_of_South_Australia")

regions <- c("Metropolitan Adelaide",
             "Eyre Peninsula",
             "Central",
             "Southern and Hills",
             "Murray Mallee",
             "Southeast",
             "Outback")

lgas_list <- map_dfr(1:7, function(x,regions,wiki_page){
  
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

uninc_areas <- tribble(~LGA,~State.Region,
                       "Torrens Island","Greater Adelaide",
                       "Riverland UIA","Murray Mallee") %>% 
  mutate(Metro.Region="Unincorporated",ABB_NAME=toupper(LGA))

lgas_list <- rbind(lgas_list,uninc_areas)

rm(wiki_page,regions)

#Download all shapefiles

shp_files <- tribble(~name,~filename,~url,
                     "LGA","SA_LGA_POLYGON_SHP_GDA2020.shp","https://data.gov.au/data/dataset/b71223e0-2235-4973-90e9-d279687b7ef8/resource/82af0b12-d124-4926-b345-8474b0c5ff70/download/sa_lga_polygon_shp_gda2020.zip",
                     "LOC","SA_LOC_POLYGON_shp_GDA2020/SA_LOC_POLYGON_shp_GDA2020.shp","https://data.gov.au/data/dataset/bcfcfc9a-7c8d-479a-9bdf-b95ca66ad29a/resource/8937ec5b-69d5-4d8c-86e0-687e61a02903/download/sa_loc_polygon_shp_gda2020.zip",
                     "POA","POA_2016_AUST.shp","https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_poa_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&4FB811FA48EECA7ACA25802C001432D0&0&July%202016&13.09.2016&Previous"
                     )


## download satorian shapfiles

shapes <- map(1:nrow(shp_files), function(x,shp_files){
           if(!file.exists(shp_files[x,]$filename)){
                  download.file(shp_files[x,]$url,"temp.zip")
                  unzip("temp.zip")
                  file.remove("temp.zip")
            }
             st_read(shp_files[x,]$filename)
            },shp_files)

names(shapes) <- shp_files %>% pull(name)

#filter POAS

shapes$POA <- shapes$POA %>% filter(POA_CODE16 %in% State_poa)
#shapes$LGA <- shapes$LGA %>% rename(LGA_NAME=SA_LGA__2)
shapes$LOC <- shapes$LOC %>% rename(NAME=SA_LOCAL_2)

loc_lga_1 <- map_df(1:nrow(shapes$LGA),function(x,suburb_polygon,lga_polygon){
                                   #message(x)
                                     a <- st_intersection(suburb_polygon,lga_polygon[x,]) %>%
                                          st_collection_extract("POLYGON") 
                                    #message(nrow(a))
                                     if(nrow(a)==0){
                                       lga_polygon[x,] %>% rename(LGA=LGA_NAME) %>% 
                                                           clean_lga() %>%
                                                           mutate(LOC_PID=LGA_PID,
                                                                  LOCALITY=LGA,
                                                                  ROW_ID=row_number(),
                                                                  AREA=st_area(.),
                                                                  RELEVANT=TRUE) %>%
                                                                  select(ROW_ID,LOC_PID,LGA_PID,LGA,LOCALITY,AREA,RELEVANT)
                                             
                                     }else{
                                        a %>% st_cast("MULTILINESTRING") %>% st_cast("LINESTRING") %>%
                                              st_collection_extract("LINESTRING") %>%
                                              st_polygonize() %>%
                                              mutate(AREA=st_area(.), RELEVANT=(AREA>area_tolerance)) %>%
                                              rename(LGA=LGA_NAME,LOCALITY=NAME) %>% 
                                              clean_lga() %>%
                                              mutate(ROW_ID=row_number(), LOCALITY=stri_trans_totitle(tolower(LOCALITY))) %>%
                                              select(ROW_ID,LOC_PID,LGA_PID,LGA,LOCALITY,AREA,RELEVANT)   
                                     }
                        },shapes$LOC,shapes$LGA)
                      

loc_count <- as.data.frame(loc_lga_1) %>% select(-geometry) %>% 
             group_by(LOC_PID,LOCALITY) %>%
             summarise(n=n(),relevant=sum(RELEVANT),.groups="drop") %>%
             mutate(single_rel=(relevant==1),
                    diff=n-relevant) %>%
             mutate(group=if_else(single_rel & (diff==0), "a","c"),    #ready to use
                    group=if_else(single_rel & !(diff==0), "b",group), #merge small pieces
                    group=if_else(!single_rel & (diff==0), "a",group)) #ready to use  # group c needs work
                   


loc_lga_1 <- loc_lga_1 %>% left_join(loc_count %>% select(LOC_PID,group),by="LOC_PID")

#filter out a, filter and consolidate c
loc_lga_a <- loc_lga_1 %>% filter(group=="a") %>% select(-ROW_ID,-group,-RELEVANT,-AREA) %>%
                                  group_by(LOC_PID,LGA_PID,LGA,LOCALITY) %>%
                                  summarise(.groups="drop") 
loc_lga_b <- loc_lga_1 %>% filter(group=="b") %>%
                 group_by(LOC_PID,LGA_PID,LGA,LOCALITY) %>%
                summarise(.groups="drop") 


loc_lga_c <-  loc_lga_1 %>% filter(group=="c") %>% 
                  filter(AREA>area_tolerance2) %>%
                  select(-group,-AREA)
  
loc_lga_c_rel <- loc_lga_c %>% filter(RELEVANT) %>%
                      group_by(LOC_PID,LGA_PID,LGA,LOCALITY) %>%
                      summarise(.groups="drop")  %>%
                      mutate(ROW_ID=10^4*row_number())
  
loc_lga_c_irrel <- loc_lga_c %>% 
                       filter(!RELEVANT) %>% 
                       mutate(ROW_ID=10^4*row_number()) %>%
                       select(-RELEVANT)

#nrow(loc_lga_c_irrel) # equals 0, nothing else to do here

#left ROW_ID in case it is needed in other states
loc_lga <- rbind(loc_lga_a,loc_lga_b) %>% 
               rbind(loc_lga_c_rel %>% select(-ROW_ID)) %>%
               left_join(lgas_list,by="LGA") %>%
               mutate(State=State) 

loc_lga$AREA <-st_area(loc_lga)

loc_lga <- loc_lga %>%
  mutate(RELEVANT=(AREA>area_tolerance)) %>%
  filter(RELEVANT) %>%
  group_by(LOC_PID,LGA_PID,LGA,LOCALITY,State.Region,ABB_NAME,Metro.Region,State) %>%
  summarise(.groups="drop")    

a<- loc_lga %>% filter(grepl("Murray Mallee",State.Region)) 
a$area <- st_area(a)
plot(a %>% select(area))

rm(list=ls()[! ls() %in% c("loc_lga","shapes","clean_lga","area_tolerance","area_tolerance2","State","State_folder","State_poa")])

#merge with poa

loc_lga<-st_transform(loc_lga,st_crs(shapes$POA))


#state <- loc_lga %>%
#  group_by(State) %>%
#  summarise(.groups="drop") 

#poa <- st_intersection(shapes$POA,state) 
#poa <- shapes$POA %>% filter(POA_CODE16 %in% poa$POA_CODE16)

loc_lga_poa1 <- map_df(1:nrow(loc_lga),function(x,lga_loc_polygon,poa_polygon){
  #message(x)
  a <- st_intersection(poa_polygon,lga_loc_polygon[x,]) %>%
       st_collection_extract("POLYGON") 
  if(nrow(a)==0){
    lga_loc_polygon[x,] %>% mutate(POA_CODE16="none",
                                   AREA=st_area(.),
                                   RELEVANT=TRUE)
  }else{
  #message(nrow(a))
      a %>% st_cast("MULTILINESTRING") %>% st_cast("LINESTRING") %>%
      st_collection_extract("LINESTRING") %>%
      st_polygonize() %>%
      mutate(AREA=st_area(.), RELEVANT=(AREA>area_tolerance)) %>%
      select(POA_CODE16,colnames(lga_loc_polygon),AREA,RELEVANT)   
  }
},loc_lga, shapes$POA)


#fill missing POAS

missing_poas <- tribble(~LOC_PID,~missingPOA,
                        "SA210006256","5540")

loc_lga_poa1 <- loc_lga_poa1  %>% 
                    left_join(missing_poas,by="LOC_PID") %>%
                    mutate(POA_CODE16=if_else(POA_CODE16=="none",missingPOA,POA_CODE16)) %>%
                    select(-missingPOA) 

#loc_lga_poa1 %>% filter(POA_CODE16=="3000")

#relevance

loc_lga_poa_table <- as.data.frame(loc_lga_poa1) %>% select(-geometry) %>% 
  filter(RELEVANT) %>% 
  select(LOC_PID,LGA_PID,LOCALITY,LGA,POA_CODE16,AREA) %>%
  group_by(LOC_PID,LGA_PID,LOCALITY,LGA) %>%
  mutate(AREA_REL=AREA/sum(AREA)) %>%
  slice_max(order_by=AREA_REL,n=1) %>%
  #filter(AREA_REL>=area_tolerance3) %>%
  #filter(AREA_REL==max(AREA_REL)) %>%
  ungroup() 

loc_lga_poa_a <-  loc_lga %>% 
                        left_join (loc_lga_poa_table %>% 
                                     select(LOC_PID,LGA_PID,POA_CODE16),
                                   by=c("LOC_PID","LGA_PID")) %>%
                        left_join(missing_poas,by="LOC_PID") %>%
                        mutate(POA_CODE16=if_else(POA_CODE16=="none",missingPOA,POA_CODE16)) %>%
                        select(-missingPOA) %>%
                        filter(!is.na(POA_CODE16))

#plot(loc_lga_poa_a %>% select(POA_CODE16))

## special areas in missing_poas

#loc_lga_remnant <- loc_lga %>% filter(LOC_PID %in% double_pas_loc)
#poa_remnant <- shapes$POA %>% filter(POA_CODE16 %in% double_poas)

#loc_lga_poa_b <- st_intersection(loc_lga_remnant,poa_remnant) %>% select(colnames(loc_lga_poa_a))


###join and clean up
#loc_lga_poa <- rbind(loc_lga_poa_a,loc_lga_poa_b)
loc_lga_poa <-loc_lga_poa_a
loc_lga_poa %>% filter(POA_CODE16=="none") 

rm(list=ls()[! ls() %in% c("loc_lga_poa","shapes","area_tolerance","area_tolerance2","State_folder","missing_poas")])

saveRDS(loc_lga_poa,str_c(State_folder,"lga_loc_poa.rds"))
#plot(loc_lga_poa %>% filter(LGA %in% c("Sydney")) %>% select(POA_CODE16))


#get sa1 

sa1 <- sa12016 %>% filter(state_name_2016=="South Australia")

sa1_poa_loc_lga <- map_df(1:nrow(loc_lga_poa),function(x,lga_loc_polygon,poa_polygon){
  #message(x)
  a <- st_intersection(poa_polygon,lga_loc_polygon[x,]) %>%
    st_collection_extract("POLYGON") 
  if(nrow(a)==0){
    lga_loc_polygon[x,] %>% mutate(POA_CODE16="none",
                                   AREA=st_area(.),
                                   RELEVANT=TRUE) %>%  filter(!(POA_CODE16=="none"))
  }else{
    #message(nrow(a))
    a %>% st_cast("MULTILINESTRING") %>% st_cast("LINESTRING") %>%
      st_collection_extract("LINESTRING") %>%
      st_polygonize() %>%
      mutate(AREA=st_area(.), RELEVANT=(AREA>area_tolerance)) %>%
      select(POA_CODE16,LOC_PID,LGA_PID,LOCALITY,LGA,State,colnames(sa1),AREA,RELEVANT)   
  }
},loc_lga_poa,sa1)

sa1_poa_loc_lga_table <- sa1_poa_loc_lga %>% 
  mutate(AREA=st_area(.)) %>%
  as.data.frame()  %>% 
  select(-geometry) %>%
  select(POA_CODE16,LOC_PID,LGA_PID,LOCALITY,LGA,State,
         sa1_main_2016,sa1_7dig_2016,sa2_main_2016,sa2_5dig_2016,sa2_name_2016,sa3_code_2016,
         sa3_name_2016,sa4_code_2016,sa4_name_2016,gcc_code_2016,gcc_name_2016,state_code_2016,AREA) %>%
  group_by(sa1_main_2016) %>%
  slice_max(order_by=AREA,n=1) %>%
  ungroup() %>%
  unique(.)

### check missing SAs -- in SA there are four SAs for offshore and no usual address
a<- sa1_poa_loc_lga_table %>% count(sa1_main_2016)
b<-sa1 %>% filter(!(sa1_main_2016 %in% a$sa1_main_2016))

plot(b)
write_csv(sa1_poa_loc_lga_table,str_c(State_folder,"sa1_table.csv"))

## Re-aggregate LGAs
##colnames(loc_lga_poa %>% select(-POA_CODE16,-LOC_PID,-LOCALITY)

lgas <- loc_lga_poa %>% 
        group_by(LGA_PID,LGA,State.Region,Metro.Region,State) %>%
        summarise(.groups="drop") 

locs <- loc_lga_poa %>% 
  group_by(LOC_PID,LOCALITY,State.Region,Metro.Region,State) %>%
  summarise(.groups="drop") 

poas <- loc_lga_poa %>% 
  group_by(POA_CODE16,State.Region,Metro.Region,State) %>%
  summarise(.groups="drop")  

regions <-lgas %>%
  group_by(State,State.Region) %>%
  summarise(.groups="drop")  %>%
  st_cast("MULTILINESTRING") %>% st_cast("LINESTRING") %>%
  st_collection_extract("LINESTRING") %>%
  st_polygonize() %>%
  mutate(AREA=st_area(.), RELEVANT=(AREA>area_tolerance)) %>%
  filter(RELEVANT) %>%
  group_by(State,State.Region) %>%
  summarise(.groups="drop") 

state <- regions %>%
         group_by(State) %>%
         summarise(.groups="drop")  %>%
         st_cast("MULTILINESTRING") %>% st_cast("LINESTRING") %>%
         st_collection_extract("LINESTRING") %>%
         st_polygonize() %>%
         mutate(AREA=st_area(.), RELEVANT=(AREA>area_tolerance)) %>%
         filter(RELEVANT) %>%
         group_by(State) %>%
         summarise(.groups="drop") 

plot(state %>% select(State))


saveRDS(lgas,str_c(State_folder,"lgas.rds"))
saveRDS(locs,str_c(State_folder,"locs.rds"))
saveRDS(poas,str_c(State_folder,"poas.rds"))
saveRDS(state,str_c(State_folder,"state.rds"))
