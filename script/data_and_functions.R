

# Read in data and useful functions

# Load in packages
pacman::p_load(readr, readxl, tidyverse, lubridate, tsibble, ggpubr, geofacet, sf, colorspace, cowplot, ggtext, ggnewscale, grid, RColorBrewer)

### Data extraction sheet (DES) of all data included in analysis 
DES_geo <- read_excel("DES/DES.xlsx") %>% 
  mutate(prv = if_else(prv == "Neimenggu", "Inner Mongolia", prv)) %>% 
  mutate(prv = if_else(prv == "Xizang", "Tibet", prv))


#### Function to monthly aggreate all data
# Input outcome of interest to identify, read and combine csv files

monthly_long_function <- function(MEASURE, STRAIN, POP_DENOM){
  
  # DES 
  DES <- DES_geo %>% 
    as_tibble() %>% 
    mutate(csv= paste("cleandata/",  csv,".csv", sep = "")) 
  
  # Pull list of csv files of given outcome
  all_outcome_vector <- DES %>% 
    filter(measure == MEASURE) %>% 
    filter(strain == STRAIN) %>% 
    filter(pop_denom == POP_DENOM) %>%
    pull(csv)
  
  # Filter DES to outcome of interest and csv files in cleandata folder
  file_vector <- DES %>% 
    filter(csv %in% all_outcome_vector) %>% 
    filter(csv %in% paste0("cleandata/",list.files("cleandata/"))) %>% 
    pull(csv)
  
  # Function to aggregate weekly data to monthly data 
  w_to_m <- function(i){ 
    
    file_vector[i] %>%
      setNames(nm = .) %>%
      map_df(~read_csv(., col_types = cols(), col_names = FALSE, skip = 1), .id = "file") %>% 
      group_by(X2) %>% 
      expand(Date = seq(floor_date(X2, unit = "month"),
                        ceiling_date(X2, unit= "month")-days(1), by="day"), X3) %>% 
      ungroup() %>% 
      mutate(time = yearmonth(Date)) %>%
      group_by(time) %>% 
      summarise(x = mean(X3), t = max(Date)) %>% 
      mutate(file = paste(as.character(file_vector[i]))) %>% 
      select(file, time, t, x) %>%
      mutate(geo_code = DES_geo %>% 
               as_tibble() %>% 
               mutate(csv= paste("cleandata/",  csv,".csv", sep = "")) %>% 
               filter(csv == file_vector[i]) %>% 
               pull(geo_code))
  }
  
  
  # Function to keep monthly data
  m_to_m <- function(i){
    
    file_vector[i] %>%
      setNames(nm = .) %>%
      map_df(~read_csv(., col_types = cols(), col_names = FALSE, skip = 1), .id = "file") %>% 
      select(file, time = X1, t = X2, x = X3 ) %>% 
      mutate(time = yearmonth(t)) %>%
      mutate(geo_code = DES_geo %>% 
               as_tibble() %>% 
               mutate(csv= paste("cleandata/",  csv,".csv", sep = "")) %>% 
               filter(csv == file_vector[i]) %>% 
               pull(geo_code)) 
  }
  
  
  # Combinded function based of DES info 
  month_correct <- function(x){
    
    if(DES %>% 
       filter(csv == file_vector[x]) %>% 
       pull(time_unit) == "month"){m_to_m(x)}else if(DES %>% 
                                                     filter(csv == file_vector[x]) %>% 
                                                     pull(time_unit) == "week"){w_to_m(x)}
  }
  
  # Map month correct funtion to all in file vector
  monthly_data <- map(1:length(file_vector), month_correct) %>% 
    bind_rows() %>% 
    mutate(time = yearmonth(time))
  
  return(monthly_data)
  
}


###---### Useful georefrenceing ###---###

# Shape file and names
shp <- readRDS("data/china_shp.rds") %>% # Unable to be publicly shared
  mutate(PYNAME = as.character(PYNAME)) %>% 
  mutate(CNTY_CODE = as.character(CNTY_CODE)) %>% 
  mutate(PYNAME = if_else(CNTY_CODE == "431000", "Chenzhou", PYNAME)) %>% # Incorrect name 
  mutate(PYNAME = if_else(PYNAME == "Neimenggu", "Inner Mongolia", PYNAME)) %>% 
  mutate(PYNAME = if_else(PYNAME == "Xizang", "Tibet", PYNAME)) %>% 
  st_as_sf()

# Latitude order of prv, prf, cty based on centroid
shp_code <- shp %>% 
  as_tibble() %>% 
  select(prv,prf,cty, PYNAME) %>%
  mutate(code = paste0(prv,prf,cty)) %>% 
  select(PYNAME, code)

geo_lat <- shp %>% 
  mutate(cent = st_centroid(geometry)) %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  st_as_sf() %>% 
  st_cast("MULTIPOINT") %>% 
  st_coordinates(cent) %>% 
  as_tibble() %>% 
  select(X,Y) %>% 
  bind_cols(shp) %>% 
  as_tibble() %>% 
  select(prv, prf, cty, X, Y) %>% 
  arrange(Y) %>% 
  mutate(geo_lat = paste0(prv,prf,cty)) %>% 
  left_join(., shp_code, c("geo_lat"="code")) %>% 
  pull(PYNAME) %>% 
  unique()

### Names and codes of all admin regions 
geo_name_function <- function(){
  
  name_code <- shp %>% 
    as_tibble() %>% 
    select(prv,prf,cty, PYNAME) %>%
    mutate(code = paste0(prv,prf,cty)) %>% 
    select(PYNAME, code) %>% 
    mutate(prv = str_sub(code, 1, 2)) %>% 
    mutate(prf = str_sub(code, 3, 4)) %>% 
    mutate(cty = str_sub(code, 5, 6)) 
  
  prv_name <- shp %>% 
    as_tibble() %>% 
    select(prv,prf,cty, PYNAME) %>% 
    filter(prf == "00") %>% 
    mutate(prv_name = PYNAME) %>% 
    select(-PYNAME, -prf, -cty)
  
  prf_name <- shp %>% 
    as_tibble() %>% 
    select(prv,prf,cty, PYNAME) %>% 
    filter(prf != "00") %>% 
    filter(cty == "00") %>% 
    mutate(prf_name = PYNAME) %>% 
    select(-PYNAME, -cty)
  
  cty_name <- shp %>% 
    as_tibble() %>% 
    select(prv,prf,cty, PYNAME) %>% 
    filter(prf != "00") %>% 
    filter(cty != "00") %>% 
    mutate(cty_name = PYNAME) %>% 
    select(-PYNAME)
  
  geo_name <- left_join(name_code, prv_name, by = c("prv"="prv")) %>% 
    left_join(., prf_name, by = c("prv"="prv", "prf" = "prf")) %>% 
    left_join(., cty_name, by = c("prv"="prv", "prf" = "prf", "cty" = "cty")) %>% 
    select(-PYNAME)
  
  return(geo_name)
  
}

geo_name <- geo_name_function()


# Nine dash line shp
nine <- st_read("data/nine/nine.shp") %>% 
  st_transform(.,st_crs(shp))


# Palette
my_pal <- colorRampPalette(brewer.pal(n = 9, name = "Oranges"))
