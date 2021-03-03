### Create spatial polygons for different divisions in Victoria, Australia
# Load/Install pacman
if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")

#use pacman to install all other packages
pacman::p_load("tidyverse","spdplyr","sp","rgdal","raster","maptools","geojsonio","rgeos","sf",
               "rvest","xml2","lwgeom")




## get list of  councils across state

regions <- c("Greater Metropolitan Melbourne","Barwon South West","Grampians",
             "Gippsland","Hume","Loddon Mallee")
wiki_page <- read_html("https://en.wikipedia.org/wiki/Local_government_areas_of_Victoria")

clean_lga <-function(df){
  
  df %>%   mutate(LGA=str_remove(LGA,"City of"),
                  LGA=str_remove(LGA,"Shire of"),
                  LGA=str_remove(LGA,"Shire"),
                  LGA=str_remove(LGA,"Borough of"),
                  LGA=str_squish(LGA),
                  LGA=str_trim(LGA))
}

#table for melbourne is different....

melb_lgas_list <- wiki_page %>%
  html_node(xpath="/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>%
  html_table(fill = TRUE) %>% 
  rename(LGA=`Local government area`,
         Population=`Population (2018)[1][2]`,
         Metro.Region=Region) %>%
  clean_lga() %>%
  dplyr::select(LGA,Population,Metro.Region) %>%
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
  dplyr::select(LGA,Population) %>%
  mutate(ABB_NAME=toupper(LGA),
         State.Region=regions[x],
         Metro.Region="")
},regions,wiki_page)

# put together and clean up

lgas_list <- rbind(lgas_list,melb_lgas_list)
rm(regions,melb_lgas_list,wiki_page,clean_lga)

#Download all shapefiles

## download victorian LGAs

if(!file.exists("VIC_LGA_POLYGON_SHP.shp")){
  download.file("https://data.gov.au/data/dataset/bdf92691-c6fe-42b9-a0e2-a4cd716fa811/resource/7b6043d1-76b8-4ea9-b36b-51c61aa740d0/download/vic_lga_polygon_shp.zip",
                "LGAs.zip")
  unzip("LGAs.zip")
  file.remove("LGAs.zip")
}

vic_lga_polygon <- readOGR( 
  dsn=path.expand("./VIC_LGA_POLYGON_SHP.shp"), 
  layer="VIC_LGA_POLYGON_SHP"
)


## get suburb polygons

if(!file.exists("VIC_LOCALITY_POLYGON_SHP.shp")){
  download.file("https://data.gov.au/data/dataset/af33dd8c-0534-4e18-9245-fc64440f742e/resource/3b946968-319e-4125-8971-2a33d5bf000c/download/vic_locality_polygon_shp.zip",
                "LGAs.zip")
  unzip("LGAs.zip")
  file.remove("LGAs.zip")
}

vic_suburb_polygon <- readOGR( 
  dsn="./VIC_LOCALITY_POLYGON_SHP.shp" , 
  layer="VIC_LOCALITY_POLYGON_SHP"
)


#postal areas from ABS
 
if(!file.exists("POA_2016_AUST.shp")){
  download.file("https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&1270055003_poa_2016_aust_shape.zip&1270.0.55.003&Data%20Cubes&4FB811FA48EECA7ACA25802C001432D0&0&July%202016&13.09.2016&Previous","pc.zip")
  unzip("pc.zip")
  file.remove("pc.zip")
}  
  
aus_poas_polygon <- readOGR( 
  dsn="./POA_2016_AUST.shp" , 
  layer="POA_2016_AUST"
)

# convert for sf objects

vic_lga_polygon <- sf::st_as_sf(vic_lga_polygon)
vic_suburb_polygon <- sf::st_as_sf(vic_suburb_polygon)
aus_poas_polygon   <-  sf::st_as_sf(aus_poas_polygon)

# check suburb/lga crossover

sl_fully_covered <- st_covered_by(vic_suburb_polygon,vic_lga_polygon)

names(sl_fully_covered) <- vic_suburb_polygon$NAME
#sl_fully_covered<-sl_fully_covered[lengths(sl_fully_covered) > 0L]

sl_fc <- tibble(suburb=character(),LGA=character())
for(i in 1:length(sl_fully_covered)){
  if(length(sl_fully_covered[i])>0)
  sl_fc <- sl_fc %>% add_row(suburb=vic_suburb_polygon[i,]$NAME,
                             LGA=vic_lga_polygon[sl_fully_covered[[i]],]$LGA_NAME)
}
sl_fully_covered <- sl_fc
rm(i,sl_fc)

sl_in <- st_intersection(vic_suburb_polygon %>% filter(!(NAME %in% sl_fully_covered$suburb)),
                      vic_lga_polygon) %>% st_area()

sl_in$area <- sl_in %>% st_area()
attributes(sl_in$area) <- NULL

a<- sl_in %>% dplyr::select(-geometry) %>% filter(area>10^4) %>% count(NAME) 

sl_in %>% filter(NAME=="BUNDOORA")
               
#state electorates

#federal electorates

### Export 
melb_boundary_geojson <- geojson_json(melb_boundary)
geojson_write(melb_boundary_geojson,file="melbourne/melb_metro_boundary.geojson")
saveRDS(melb_boundary,"melbourne/melb_metro_boundary.rds")
ggsave("melbourne/melb_metro_boundary.png",plot(melb_boundary))

melb_lga_polygon_geojson <- geojson_json(melb_lga_polygon)
geojson_write(melb_lga_polygon_geojson,file="melbourne/melb_lgas.geojson")
saveRDS(melb_lga_polygon,"melbourne/melb_lgas.rds")
ggsave("melbourne/melb_lga.png",plot(melb_lga_polygon))


melb_suburb_polygon_geojson <- geojson_json(melb_suburb_polygon)
geojson_write(melb_suburb_polygon_geojson,file="melbourne/melb_suburbs.geojson")
saveRDS(melb_suburb_polygon,"melbourne/melb_suburbs.rds")
ggsave("melbourne/melb_suburb.png",plot(melb_suburb_polygon))
