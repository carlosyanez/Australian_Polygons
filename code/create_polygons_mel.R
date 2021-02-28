### Create spatial polygons for different divisions in Metropolitan Melbourne


# Load/Install pacman
if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")

#use pacman to install all other packages
pacman::p_load("tidyverse","spdplyr","sp","rgdal","raster","maptools","geojsonio",
               "rvest","xml2")


## get list of metropolitan area councils

melb_lgas_list <- read_html("https://en.wikipedia.org/wiki/Local_government_areas_of_Victoria#Municipalities_of_Greater_Melbourne") %>%
  html_node(xpath="/html/body/div[3]/div[3]/div[5]/div[1]/table[1]") %>%
  html_table(fill = TRUE) %>% 
  mutate(LGA=str_remove(`Local government area`,"City of"),
         LGA=str_remove(LGA,"Shire of"),
         LGA=str_squish(LGA),
         LGA=str_trim(LGA)) %>%
  rename(Population.2018=`Population (2018)[1][2]`) %>%
  dplyr::select(LGA,Population.2018,Region) %>%
  mutate(ABB_NAME=toupper(LGA))


## download victorian LGAs

if(!file.exists("VIC_LGA_POLYGON_SHP.shp")){
  download.file("https://data.gov.au/data/dataset/bdf92691-c6fe-42b9-a0e2-a4cd716fa811/resource/7b6043d1-76b8-4ea9-b36b-51c61aa740d0/download/vic_lga_polygon_shp.zip",
                "LGAs.zip")
  unzip("LGAs.zip")
  file.remove("LGAs.zip")
}

vic_lga_polygon <- readOGR( 
  dsn="./" , 
  layer="VIC_LGA_POLYGON_SHP"
)

##filter non metropolitan councils and create bounding box

melb_lga_polygon <- vic_lga_polygon %>% filter(ABB_NAME %in% melb_lgas_list$ABB_NAME)
state_pid <- melb_lga_polygon@data$STATE_PID
melb_boundary <-maptools::unionSpatialPolygons(melb_lga_polygon,state_pid)
rm("vic_lga_polygon","state_pid")


## get suburb polygons

if(!file.exists("VIC_LOCALITY_POLYGON_SHP.shp")){
  download.file("https://data.gov.au/data/dataset/af33dd8c-0534-4e18-9245-fc64440f742e/resource/3b946968-319e-4125-8971-2a33d5bf000c/download/vic_locality_polygon_shp.zip",
                "LGAs.zip")
  unzip("LGAs.zip")
  file.remove("LGAs.zip")
}

vic_suburb_polygon <- readOGR( 
  dsn="./" , 
  layer="VIC_LOCALITY_POLYGON_SHP"
)

aux_polygon<-raster::intersect(melb_boundary,vic_suburb_polygon)
melb_suburb_polygon <- vic_suburb_polygon %>% filter(NAME %in% aux_polygon@data$NAME)

rm("aux_polygon","vic_suburb_polygon")

#postcodes

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
