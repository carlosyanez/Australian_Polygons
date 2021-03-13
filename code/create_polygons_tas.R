### Create spatial polygons for different divisions in Tasmania, Australia
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

State <-"TAS"
State_folder <- "tas/"
State_poa <- as.character(7000:7999)

wiki_page <- read_html("https://en.wikipedia.org/wiki/Local_government_areas_of_Tasmania")



#table for TAs is different....

tas_lgas_list <- wiki_page %>%
  html_node(xpath="/html/body/div[3]/div[3]/div[5]/div[1]/table") %>%
  html_table(fill = TRUE) %>% 
  .[!duplicated(as.list(.))] %>%
  slice(-1) %>%
  rename(LGA=`Local government area`,
         State.Region=Region) %>%
  clean_lga() %>%
  dplyr::select(LGA,State.Region) %>%
  mutate(ABB_NAME=toupper(LGA),
         Metro.Region="")


# put together and clean up

lgas_list <- tas_lgas_list

rm(tas_lgas_list,wiki_page)

#Download all shapefiles

shp_files <- tribble(~name,~filename,~url,
                     "LGA","TAS_LGA_POLYGON_SHP_GDA2020.shp","https://data.gov.au/data/dataset/02565094-6b52-4d51-8a35-2273c711d60d/resource/0045e427-75b5-4764-8d63-ffbc51c4f9bd/download/tas_lga_polygon_shp_gda2020.zip",
                     "LOC","TAS_LOC_POLYGON_shp_GDA2020/TAS_LOC_POLYGON_shp_GDA2020.shp","https://data.gov.au/data/dataset/8bd7b6c1-1258-4df5-a98f-b6706e87de1e/resource/c0e8ebd4-2be7-414e-956e-6ed25a1c11a4/download/tas_loc_polygon_shp_gda2020.zip",
                     "POA","POA_2016_AUST.shp","https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_poa_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&4FB811FA48EECA7ACA25802C001432D0&0&July%202016&13.09.2016&Previous"
                     )


## download tastorian shapfiles

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
shapes$LOC <- shapes$LOC %>% rename(NAME=TAS_LOCA_2)

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
                                  summarise()
loc_lga_b <- loc_lga_1 %>% filter(group=="b") %>%
                 group_by(LOC_PID,LGA_PID,LGA,LOCALITY) %>%
                summarise()


loc_lga_c <-  loc_lga_1 %>% filter(group=="c") %>% 
                  filter(AREA>area_tolerance2) %>%
                  select(-group,-AREA)
  
loc_lga_c_rel <- loc_lga_c %>% filter(RELEVANT) %>%
                      group_by(LOC_PID,LGA_PID,LGA,LOCALITY) %>%
                      summarise(.groups="drop")%>%
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
               mutate(State=State) %>%
               st_cast("MULTILINESTRING") %>% st_cast("LINESTRING") %>%
               st_collection_extract("LINESTRING") %>%
               st_polygonize() %>%
               st_collection_extract("POLYGON") 

loc_lga$AREA <-st_area(loc_lga)

loc_lga <- loc_lga %>%
           mutate(RELEVANT=(AREA>area_tolerance)) %>%
           filter(RELEVANT) %>%
           group_by(LOC_PID,LGA_PID,LGA,LOCALITY,State.Region,ABB_NAME,Metro.Region,State) %>%
           summarise(.groups="drop")   

#plot(loc_lga %>%  select(AREA))

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

loc_lga_poa1 <- loc_lga_poa1 %>%filter(!(POA_CODE16=="none"))

#missing<-as.data.frame(loc_lga_poa1) %>% filter(POA_CODE16=="none") %>% select(LOC_PID,LOCALITY,AREA) %>% unique(.)
#missing_poas <- tribble(~LOC_PID,~missingPOA,
#                        "TAS68","7330",
#                        "TAS71","3971",
#                        "TAS72","3966",
#                        "TAS74","",
#                        "TAS75","",
#                        "TAS78","",
#                        "TAS85","",
#                        "TAS86","",
#                        "TAS87"""
#)
#double_poas <- c("3000","3001","3004")
#double_pas_loc <-c("TAS1634")

#loc_lga_poa1 <- loc_lga_poa1 %>% 
#                    left_join(missing_poas,by="LOC_PID") %>%
#                    mutate(POA_CODE16=if_else(POA_CODE16=="none",missingPOA,POA_CODE16)) %>%
#                    filter(!(POA_CODE16 %in% double_poas)) %>%
#                    filter(!(LOC_PID %in% double_pas_loc)) %>%
#                    select(-missingPOA) 

#loc_lga_poa11 %>% filter(POA_CODE16=="7000")

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
                        filter(!is.na(POA_CODE16))

#plot(loc_lga_poa_a %>% select(POA_CODE16))

## special areas in missing_poas

#loc_lga_remnant <- loc_lga %>% filter(LOC_PID %in% double_pas_loc)
#poa_remnant <- shapes$POA %>% filter(POA_CODE16 %in% double_poas)

#loc_lga_poa_b <- st_intersection(loc_lga_remnant,poa_remnant) %>% select(colnames(loc_lga_poa_a))


###join and clean up

#loc_lga_poa <- rbind(loc_lga_poa_a,loc_lga_poa_b)
loc_lga_poa <-loc_lga_poa_a

rm(list=ls()[! ls() %in% c("loc_lga_poa","shapes","area_tolerance","area_tolerance2","State_folder")])

saveRDS(loc_lga_poa,str_c(State_folder,"lga_loc_poa.rds"))
#plot(loc_lga_poa %>% filter(State.Region %in% c("Hobart")) %>% select(POA_CODE16))


#get sa1 

sa1 <- sa12016 %>%  filter(state_name_2016=="Tasmania")


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

#Tassie only , remove West Coast (wilderness area??) from SA1 mapping

sa1_poa_loc_lga_table <- sa1_poa_loc_lga_table %>% filter(!(LOC_PID=="TAS727" & LOCALITY=="West Coast"))

write_csv(sa1_poa_loc_lga_table,str_c(State_folder,"sa1_table.csv"))

## Re-aggregate LGAs
##colnames(loc_lga_poa %>% select(-POA_CODE16,-LOC_PID,-LOCALITY)

lgas <- loc_lga_poa %>% 
        group_by(LGA_PID,LGA,State.Region,Metro.Region,State) %>%
        summarise()
#plot(lgas %>% select(LGA))

locs <- loc_lga_poa %>% 
  group_by(LOC_PID,LOCALITY,State.Region,Metro.Region,State) %>%
  summarise()
#plot(locs %>% select(LGA))


poas <- loc_lga_poa %>% 
  group_by(POA_CODE16,State.Region,Metro.Region,State) %>%
  summarise(.groups="drop")

#plot(poas %>% select(State.Region))

state <- lgas %>%
         group_by(State) %>%
         summarise(.groups="drop")%>%
         st_cast("MULTILINESTRING") %>% st_cast("LINESTRING") %>%
         st_collection_extract("LINESTRING") %>%
         st_polygonize() %>%
         mutate(AREA=st_area(.), RELEVANT=(AREA>area_tolerance)) %>%
         filter(RELEVANT) %>%
  
         group_by(State) %>%
         summarise()

plot(state %>% select(State))


saveRDS(lgas,str_c(State_folder,"lgas.rds"))
saveRDS(locs,str_c(State_folder,"locs.rds"))
saveRDS(poas,str_c(State_folder,"poas.rds"))
saveRDS(state,str_c(State_folder,"state.rds"))
