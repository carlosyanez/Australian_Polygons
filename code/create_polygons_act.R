### Create spatial polygons for different divisions in ACT, Australia
# Load/Install pacman
if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")
if(!require(absmapsdata)) devtools::install_github("wfmackey/absmapsdata")

devtools::install_github("wfmackey/absmaps")
#use pacman to install all other packages
pacman::p_load("tidyverse","rgdal","sf","lwgeom","spdep","geojsonsf","rgeos","smoothr",
               "rvest","xml2","stringi","units","absmapsdata")

source("code/clean_lga.R")


## get list of  councils across state
area_tolerance <-set_units(10^4,m^2)
area_tolerance2 <-set_units(10^4,m^2)
area_tolerance3 <-set_units(0.3,1)

State <-"ACT"
State_folder <- "act/"
State_poa <-  as.character(c(2600:2618,2900:2920,2620))


rm(nt_lgas_list,wiki_page)

#Download all shapefiles

shp_files <- tribble(~name,~filename,~url,
                     "LOC","ACT_LOCALITY_POLYGON_SHP-GDA2020.shp","https://data.gov.au/data/dataset/0257a9da-b558-4d86-a987-535c775cf8d8/resource/b91e5877-5426-416d-99c6-355d15d2c461/download/act_locality_polygon_shp-gda2020.zip",
                     "POA","POA_2016_AUST.shp","https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_poa_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&4FB811FA48EECA7ACA25802C001432D0&0&July%202016&13.09.2016&Previous"
                     )


## download act shapfiles

shapes <- map(1:nrow(shp_files), function(x,shp_files){
           if(!file.exists(shp_files[x,]$filename)){
                  download.file(shp_files[x,]$url,"temp.zip")
                  unzip("temp.zip")
                  file.remove("temp.zip")
            }
             st_read(shp_files[x,]$filename)
            },shp_files)

names(shapes) <- shp_files %>% pull(name)

#filter POAS , standardise names

shapes$POA <- shapes$POA %>% filter(POA_CODE16 %in% State_poa)
shapes$LOC <- st_buffer(shapes$LOC,0)
#shapes$LOC <- shapes$LOC %>% rename(NAME=TAS_LOCA_2)


loc_lga <- shapes$LOC %>%
           rename(LOCALITY=NAME) %>% 
           mutate(ROW_ID=row_number(), LOCALITY=stri_trans_totitle(tolower(LOCALITY))) %>%
           select(ROW_ID,LOC_PID,LOCALITY)   %>%
           mutate(LGA_PID="none",LGA="Canberra",State.Region="",Metro.Region="",State="Australian Capital Territory")

#plot(loc_lga %>%  select(LGA))

rm(list=ls()[! ls() %in% c("loc_lga","shapes","clean_lga","area_tolerance","area_tolerance2","State","State_folder","State_poa")])

#merge with poa

loc_lga<-st_transform(loc_lga,st_crs(shapes$POA))


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


#fill missing POAS - not relevant for tassie

#loc_lga_poa1 <- loc_lga_poa1 %>% filter((POA_CODE16=="none"))

#missing<-as.data.frame(loc_lga_poa1) %>% filter(POA_CODE16=="none" & RELEVANT) %>% select(LOC_PID,LOCALITY,AREA,-geometry) %>% unique(.)
missing_poas <- tribble(~LOC_PID,~missingPOA,
                        "ACT707","2914",
                        "ACT614","2906",
                        "ACT905","2620",
                        "ACT719","2914",
                        "ACT619","2905",
                        "ACT925","2620",
                        "ACT611","2905",
                        "ACT903","2913",
                        "ACT609","2905",
                        "ACT613","2906",
                        "ACT605","2904",
                        "ACT716","2914",
                        "ACT902","2913",
                        "ACT608","2905",
                        "ACT616","2906",
                        "ACT604","2904",
                        "ACT615","2900",
                        "ACT710","2912",
                        "ACT711","2914",
                        "ACT612","2905",
                        "ACT928","2914",
                        "ACT601","2902",
                        "ACT606","2904",
                        "ACT603","2904",
                        "ACT932","2914",
                        "ACT920","2620",
                        "ACT706","2913",
                        "ACT708","2913",
                        "ACT623","2903",
                        "ACT709","2913",
                        "ACT922","2620",
                        "ACT607","2905",
                        "ACT934","2913",
                        "ACT659","2620",
                        "ACT610","2905",
                        "ACT933","2914",
                        "ACT602","2903",
                        "ACT701","2911",
                        "ACT924","2620",
                        "ACT913","2900",
                        "ACT801","2620",
                        "ACT802","2620"
)

#double_poas <- c("3000","3001","3004")
#double_pas_loc <-c("TAS1634")

loc_lga_poa1 <- loc_lga_poa1 %>% 
                    left_join(missing_poas,by="LOC_PID") %>%
                    mutate(POA_CODE16=str_trim(POA_CODE16)) %>%
                    mutate(POA_CODE16=if_else(POA_CODE16=="none",missingPOA,str_trim(POA_CODE16))) %>%
#                    filter(!(POA_CODE16 %in% double_poas)) %>%
#                    filter(!(LOC_PID %in% double_pas_loc)) %>%
                     select(-missingPOA) 

#loc_lga_poa11 %>% filter(POA_CODE16=="7000")

#relevance

loc_lga_poa_table <- as.data.frame(loc_lga_poa1) %>% select(-geometry) %>% 
  #filter(RELEVANT) %>% 
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
                        filter(!is.na(POA_CODE16))

#plot(loc_lga_poa_a %>% select(POA_CODE16))

## special areas in missing_poas

#loc_lga_remnant <- loc_lga %>% filter(LOC_PID %in% double_pas_loc)
#poa_remnant <- shapes$POA %>% filter(POA_CODE16 %in% double_poas)

#loc_lga_poa_b <- st_intersection(loc_lga_remnant,poa_remnant) %>% select(colnames(loc_lga_poa_a))


###join and clean up

#loc_lga_poa <- rbind(loc_lga_poa_a,loc_lga_poa_b)
loc_lga_poa <-loc_lga_poa_a %>% select(-ROW_ID) %>% mutate(ABB_NAME=toupper(LGA))

rm(list=ls()[! ls() %in% c("loc_lga_poa","shapes","area_tolerance","area_tolerance2","State_folder")])

saveRDS(loc_lga_poa,str_c(State_folder,"lga_loc_poa.rds"))
#plot(loc_lga_poa %>% filter(State.Region %in% c("Greater Darwin")) %>% select(POA_CODE16))


#get sa1 

sa1 <- sa12016 %>%  filter(state_name_2016=="Australian Capital Territory")

sa1_poa_loc_lga <- map_df(1:nrow(loc_lga_poa),function(x,lga_loc_polygon,poa_polygon){
  #message(x)
  a <- st_intersection(poa_polygon,lga_loc_polygon[x,]) %>%
    st_collection_extract("POLYGON") 
  if(nrow(a)==0){
    lga_loc_polygon[x,] %>% mutate(POA_CODE16="none",
                                   AREA=st_area(.),
                                   RELEVANT=TRUE) %>% filter(!(POA_CODE16=="none"))
  }else{
    #message(nrow(a))
    a %>% st_cast("MULTILINESTRING") %>% st_cast("LINESTRING") %>%
      st_collection_extract("LINESTRING") %>%
      st_polygonize() %>%
      mutate(AREA=st_area(.), RELEVANT=(AREA>area_tolerance)) %>%
      select(POA_CODE16,LOC_PID,LGA_PID,LOCALITY,LGA,State,colnames(sa1),AREA,RELEVANT)   
  }
},loc_lga_poa,sa1)

sa1_poa_loc_lga$AREA <- st_area(sa1_poa_loc_lga)

sa1_poa_loc_lga_table <- sa1_poa_loc_lga %>% 
  as.data.frame()  %>% 
  select(-geometry) %>%
  select(POA_CODE16,LOC_PID,LGA_PID,LOCALITY,LGA,State,
         sa1_main_2016,sa1_7dig_2016,sa2_main_2016,sa2_5dig_2016,sa2_name_2016,sa3_code_2016,
         sa3_name_2016,sa4_code_2016,sa4_name_2016,gcc_code_2016,gcc_name_2016,state_code_2016,AREA) %>%
  group_by(sa1_main_2016) %>%
  slice_max(order_by=AREA,n=1) %>%
  ungroup() %>%
  unique(.)

### check missing SAs -- in TAS there are four SAs for offshore and no usual address
#a<- sa1_poa_loc_lga_table %>% count(sa1_main_2016)
#sa1 %>% filter(!(sa1_main_2016 %in% a$sa1_main_2016))
#b <- sa1_poa_loc_lga_table %>% filter(sa1_main_2016 %in% (a %>% filter(n>1) %>% pull(sa1_main_2016)))

#replace LGAs with Districts
sa1_poa_loc_lga_table <- sa1_poa_loc_lga_table %>% mutate(LGA=sa3_name_2016)

loc_lga_poa <-loc_lga_poa %>% 
              select(-LGA) %>%
              left_join(sa1_poa_loc_lga_table %>% select(LOC_PID,LGA) %>% unique(.),
                        by="LOC_PID") %>%
              mutate(LGA=if_else(is.na(LGA),"NA",LGA))


saveRDS(loc_lga_poa,str_c(State_folder,"lga_loc_poa.rds"))
write_csv(sa1_poa_loc_lga_table,str_c(State_folder,"sa1_table.csv"))

## Re-aggregate LGAs
##colnames(loc_lga_poa %>% select(-POA_CODE16,-LOC_PID,-LOCALITY)

lgas <- loc_lga_poa %>% 
        group_by(LGA_PID,LGA,State.Region,Metro.Region,State) %>%
        summarise(.groups="drop")
#plot(lgas %>% select(LGA))

locs <- loc_lga_poa %>% 
  group_by(LOC_PID,LOCALITY,State.Region,Metro.Region,State) %>%
  summarise(.groups="drop")
#plot(locs %>% select(LGA))


poas <- loc_lga_poa %>% 
  group_by(POA_CODE16,State.Region,Metro.Region,State) %>%
  summarise(.groups="drop") 

#plot(poas %>% select(State.Region))

state <- lgas %>%
         group_by(State) %>%
         summarise(.groups="drop") %>%
         st_cast("MULTILINESTRING") %>% st_cast("LINESTRING") %>%
         st_collection_extract("LINESTRING") %>%
         st_polygonize() %>%
         mutate(AREA=st_area(.), RELEVANT=(AREA>area_tolerance)) %>%
         filter(RELEVANT) %>%
         group_by(State) %>%
         summarise(.groups="drop")

plot(loc_lga_poa %>% select(State))


saveRDS(lgas,str_c(State_folder,"lgas.rds"))
saveRDS(locs,str_c(State_folder,"locs.rds"))
saveRDS(poas,str_c(State_folder,"poas.rds"))
saveRDS(state,str_c(State_folder,"state.rds"))
