### Labeled province level map

# Figure S20

# Read in data and useful functions
devtools::install_github("yutannihilation/ggsflabel")


# Source geo_code matched DES 
source("script/data_and_functions.R")


## Shape file and names
shp <- readRDS("data/china_shp.rds") %>% 
  mutate(PYNAME = as.character(PYNAME)) %>% 
  mutate(CNTY_CODE = as.character(CNTY_CODE)) %>% 
  mutate(PYNAME = if_else(CNTY_CODE == "431000", "Chenzhou", PYNAME)) %>% # Incorrect name 
  filter(lvl=="prv") %>% 
  mutate(PYNAME = if_else(PYNAME == "Neimenggu", "Inner-Mongolia", PYNAME)) %>% 
  mutate(PYNAME = if_else(PYNAME == "Xizang", "Tibet", PYNAME)) %>%
  st_as_sf()

# Just prv
all_china <- shp %>% 
  filter(lvl=="prv")

# Prv lables
prv_name <-  shp %>%
  filter(lvl=="prv") %>% 
  filter(PYNAME!="Ningxia"&PYNAME!="Beijing"&PYNAME!="Tianjin"&PYNAME!="Hebei"&PYNAME!="Hainan"&PYNAME!="Guangdong"&PYNAME!="Taiwan"&PYNAME!="Shanghai"&PYNAME!="Jiangsu"&PYNAME!="Fujian")

# Prv lables offset
prv_name_small <- shp %>%
  filter(lvl=="prv") %>% 
  filter(PYNAME=="Ningxia"|PYNAME=="Beijing"|PYNAME=="Tianjin"|PYNAME=="Hebei"|PYNAME=="Hainan"|PYNAME=="Guangdong"|PYNAME=="Taiwan"|PYNAME=="Shanghai"|PYNAME=="Jiangsu"|PYNAME=="Fujian")


# Nine dash line shp
nine <- st_read("data/nine/nine.shp") 
nine <- st_transform(nine,st_crs(all_china))

# World shp
world <- st_read("data/world_shp/Countries_WGS84.shp") 
world <- st_transform(world,st_crs(all_china))

# Plot map
ggplot()+
  geom_sf(data=world,fill="grey90",colour="white")+
  geom_sf(data= world %>% filter(CNTRY_NAME=="China"),colour="white",size=2)+
  geom_sf(data=all_china, colour="white",fill="lightblue")+
  geom_sf(data = nine, colour = "lightblue", fill = "lightblue", size = 1)+
  
  ggsflabel::geom_sf_label(data=prv_name, aes(label=stringr::word(PYNAME)), size = 2.5) +
  ggsflabel::geom_sf_label_repel(data=prv_name_small, aes(label=stringr::word(PYNAME)), 
                                 nudge_x = c(3,-2,3,-2.5,4,
                                             3.5,1,2,4,0), 
                                 nudge_y = c(2,2,-0.5,0.5,0,
                                             0.5,-0.01,-2,-0.5,2),
                                 seed = 10, size = 2.5) +
  
  theme_void() +
  coord_sf(xlim=c(73.44649,135.08315),ylim=c(3.408477,53.55765))


# Save map
ggsave(
  filename = "output/Sup_fig_S20.png",
  width = 230,
  height = 210,
  dpi = 320,
  units = "mm"
)







