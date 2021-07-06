### Search results 

# Produces results for:

# Figure S3

# Read in data and useful functions
source("script/data_and_functions.R")

Sys.setlocale("LC_ALL","English")

# Figure S3 - Maps of outcome coverage

# Map shp file
map <- shp %>% 
  st_as_sf() %>% 
  select(PYNAME, lvl, prv, prf, cty) 

# Data for each outcome
ILI_func <- function(){
  
  match <- DES_geo %>% 
    filter(measure == "ILI %") %>% 
    filter(pop_denom == "outpatient") %>% 
    filter(strain == "N/A")
  
  #prv
  prv <-  DES_geo %>% 
    filter(measure == "ILI %") %>% 
    filter(pop_denom == "outpatient") %>% 
    filter(strain == "N/A") %>%
    filter(prf_c == "00") %>% 
    filter(cty_c == "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  #prf
  prf <-  DES_geo %>% 
    filter(measure == "ILI %") %>% 
    filter(pop_denom == "outpatient") %>% 
    filter(strain == "N/A") %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c == "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  #cty
  cty <-  DES_geo %>% 
    filter(measure == "ILI %") %>% 
    filter(pop_denom == "outpatient") %>% 
    filter(strain == "N/A") %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c != "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  map_data <- bind_rows(prv, prf, cty) %>% 
    right_join(map, c("prv_c"="prv", "prf_c" = "prf", "cty_c" = "cty")) 
  
  map_prv <- map_data %>% 
    filter(prf_c == "00") %>% 
    filter(cty_c == "00") %>% 
    st_as_sf() %>% 
    mutate(name = "ILI consulation rate")
  
  map_prf <- map_data %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c == "00") %>% 
    filter(!is.na(no_study)) %>% 
    st_as_sf() %>% 
    mutate(name = "ILI consulation rate")
  
  map_cty <- map_data %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c != "00") %>% 
    filter(!is.na(no_study)) %>% 
    st_as_sf() %>% 
    mutate(name = "ILI consulation rate")
  
  ILI <- ggplot() +
    geom_sf(data = map_prv, aes(fill = no_study)) +
    scale_fill_gradient(name = "Province", low = "white", high = "#1b9e77", na.value = "grey90", breaks = c(3, 6),limits = c(0, 9), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=1, title.hjust= 0.5, frame.colour = "black"))+
    
    new_scale("fill") +
    
    geom_sf(data = map_prf, aes(fill = no_study)) +
    geom_sf(data = nine, colour = "grey50", fill = "grey50", size = 0.5)+
    scale_fill_gradient(name = "Prefecture", low = "white", high = "#d95f02", na.value = "grey90", breaks = c(2, 4),limits = c(0, 6), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=2, title.hjust= 0.5, frame.colour = "black"))+
    
    new_scale("fill") +
    
    geom_sf(data = map_cty, aes(fill = no_study)) +
    scale_fill_gradient(name = "County", low = "white", high = "#7570b3", na.value = "grey90", breaks = c(1, 2),limits = c(0, 3), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=3, title.hjust= 0.5, frame.colour = "black"))+
    facet_grid(cols =  vars(name)) +
    
    theme(panel.grid.major = element_line(color = grey(0.75),linetype = "dashed", size = 0.5), 
          panel.background = element_rect(fill = "white"),
          strip.background = element_blank(),
          legend.box = "horizontal",
          legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_text(size = 6),
          title = element_text(size = 8))
  
  return(ILI)
  
}

TPO_func <- function(){
  
  match <- DES_geo %>% 
    filter(measure == "Positive rate") %>% 
    filter(pop_denom == "ILI outpatient") %>% 
    filter(strain == "All")
  
  #prv
  prv <- DES_geo %>% 
    filter(measure == "Positive rate") %>% 
    filter(pop_denom == "ILI outpatient") %>% 
    filter(strain == "All") %>% 
    filter(prf_c == "00") %>% 
    filter(cty_c == "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  #prf
  prf <- DES_geo %>% 
    filter(measure == "Positive rate") %>% 
    filter(pop_denom == "ILI outpatient") %>% 
    filter(strain == "All") %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c == "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  #cty
  cty <- DES_geo %>% 
    filter(measure == "Positive rate") %>% 
    filter(pop_denom == "ILI outpatient") %>% 
    filter(strain == "All") %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c != "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  map_data <- bind_rows(prv, prf, cty) %>% 
    right_join(map, c("prv_c"="prv", "prf_c" = "prf", "cty_c" = "cty")) 
  
  map_prv <- map_data %>% 
    filter(prf_c == "00") %>% 
    filter(cty_c == "00") %>% 
    st_as_sf()
  
  map_prf <- map_data %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c == "00") %>% 
    filter(!is.na(no_study)) %>% 
    st_as_sf()
  
  map_cty <- map_data %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c != "00") %>% 
    filter(!is.na(no_study)) %>% 
    st_as_sf() %>% 
    mutate(name = "Influenza test positivity (ILI outpatient)")
  
  TPO <- ggplot() +
    geom_sf(data = map_prv, aes(fill = no_study)) +
    scale_fill_gradient(name = "Province", low = "white", high = "#1b9e77", na.value = "grey90", breaks = c(3, 6),limits = c(0, 9), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=1, title.hjust= 0.5, frame.colour = "black"))+
    
    new_scale("fill") +
    
    geom_sf(data = map_prf, aes(fill = no_study)) +
    geom_sf(data = nine, colour = "grey50", fill = "grey50", size = 0.5)+
    scale_fill_gradient(name = "Prefecture", low = "white", high = "#d95f02", na.value = "grey90", breaks = c(2, 4),limits = c(0, 6), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=2, title.hjust= 0.5, frame.colour = "black"))+
    
    new_scale("fill") +
    
    geom_sf(data = map_cty, aes(fill = no_study)) +
    scale_fill_gradient(name = "County", low = "white", high = "#7570b3", na.value = "grey90", breaks = c(1, 2),limits = c(0, 3), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=3, title.hjust= 0.5, frame.colour = "black"))+
    facet_grid(cols =  vars(name)) +
    
    theme(panel.grid.major = element_line(color = grey(0.75),linetype = "dashed", size = 0.5), 
          panel.background = element_rect(fill = "white"),
          strip.background = element_blank(),
          legend.box = "horizontal",
          legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_text(size = 6),
          title = element_text(size = 8))
  
  return(TPO)
  
}

TPI_func <- function(){
  
  match <- DES_geo %>% 
    filter(measure == "Positive rate") %>% 
    filter(pop_denom == "SARI") %>% 
    filter(strain == "All")
  
  #prv
  prv <- DES_geo %>% 
    filter(measure == "Positive rate") %>% 
    filter(pop_denom == "SARI") %>% 
    filter(strain == "All") %>% 
    filter(prf_c == "00") %>% 
    filter(cty_c == "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  #prf
  prf <- DES_geo %>% 
    filter(measure == "Positive rate") %>% 
    filter(pop_denom == "SARI") %>% 
    filter(strain == "All") %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c == "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  #cty
  cty <- DES_geo %>% 
    filter(measure == "Positive rate") %>% 
    filter(pop_denom == "SARI") %>% 
    filter(strain == "All") %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c != "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  map_data <- bind_rows(prv, prf, cty) %>% 
    right_join(map, c("prv_c"="prv", "prf_c" = "prf", "cty_c" = "cty")) 
  
  map_prv <- map_data %>% 
    filter(prf_c == "00") %>% 
    filter(cty_c == "00") %>% 
    st_as_sf() %>% 
    mutate(name = "Influenza test positivity (SARI inpatient)")
  
  map_prf <- map_data %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c == "00") %>% 
    filter(!is.na(no_study)) %>% 
    st_as_sf() %>% 
    mutate(name = "Influenza test positivity (SARI inpatient)")
  
  map_cty <- map_data %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c != "00") %>% 
    filter(!is.na(no_study)) %>% 
    st_as_sf() %>% 
    mutate(name = "Influenza test positivity (SARI inpatient)")
  
  TPI <- ggplot() +
    geom_sf(data = map_prv, aes(fill = no_study)) +
    scale_fill_gradient(name = "Province", low = "white", high = "#1b9e77", na.value = "grey90", breaks = c(3, 6),limits = c(0, 9), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=1, title.hjust= 0.5, frame.colour = "black"))+
    
    new_scale("fill") +
    
    geom_sf(data = map_prf, aes(fill = no_study)) +
    geom_sf(data = nine, colour = "grey50", fill = "grey50", size = 0.5)+
    scale_fill_gradient(name = "Prefecture", low = "white", high = "#d95f02", na.value = "grey90", breaks = c(2, 4),limits = c(0, 6), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=2, title.hjust= 0.5, frame.colour = "black"))+
    
    new_scale("fill") +
    
    geom_sf(data = map_cty, aes(fill = no_study)) +
    scale_fill_gradient(name = "County", low = "white", high = "#7570b3", na.value = "grey90", breaks = c(1, 2),limits = c(0, 3), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=3, title.hjust= 0.5, frame.colour = "black"))+
    facet_grid(cols =  vars(name)) +
    
    theme(panel.grid.major = element_line(color = grey(0.75),linetype = "dashed", size = 0.5), 
          panel.background = element_rect(fill = "white"),
          strip.background = element_blank(),
          legend.box = "horizontal",
          legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_text(size = 6),
          title = element_text(size = 8))
  
  return(TPI)
  
}

MOR_func <- function(){
  
  match <- DES_geo %>% 
    filter(measure == "Mort_flu") %>% 
    filter(pop_denom == "mortality_resp" | pop_denom == "mortality_all") %>% 
    filter(strain == "N/A")
  
  #prv
  prv <-  DES_geo %>% 
    filter(measure == "Mort_flu") %>% 
    filter(pop_denom == "mortality_resp" | pop_denom == "mortality_all") %>% 
    filter(strain == "N/A") %>% 
    filter(prf_c == "00") %>% 
    filter(cty_c == "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  #prf
  prf <-  DES_geo %>% 
    filter(measure == "Mort_flu") %>% 
    filter(pop_denom == "mortality_resp" | pop_denom == "mortality_all") %>% 
    filter(strain == "N/A") %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c == "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  #cty
  cty <-  DES_geo %>% 
    filter(measure == "Mort_flu") %>% 
    filter(pop_denom == "mortality_resp" | pop_denom == "mortality_all") %>% 
    filter(strain == "N/A") %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c != "00") %>% 
    group_by(prv_c, prf_c, cty_c) %>% 
    summarise(no_study = n_distinct(id_study)) %>% 
    ungroup()
  
  map_data <- bind_rows(prv, prf, cty) %>% 
    right_join(map, c("prv_c"="prv", "prf_c" = "prf", "cty_c" = "cty")) 
  
  map_prv <- map_data %>% 
    filter(prf_c == "00") %>% 
    filter(cty_c == "00") %>% 
    st_as_sf() %>% 
    mutate(name = "Influenza associated mortality")
  
  map_prf <- map_data %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c == "00") %>% 
    filter(!is.na(no_study)) %>% 
    st_as_sf() %>% 
    mutate(name = "Influenza associated mortality")
  
  map_cty <- map_data %>% 
    filter(prf_c != "00") %>% 
    filter(cty_c != "00") %>% 
    filter(!is.na(no_study)) %>% 
    st_as_sf() %>% 
    mutate(name = "Influenza associated mortality")
  
  MOR <- ggplot() +
    geom_sf(data = map_prv, aes(fill = no_study)) +
    scale_fill_gradient(name = "Province", low = "white", high = "#1b9e77", na.value = "grey90", breaks = c(3, 6),limits = c(0, 9), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=1, title.hjust= 0.5, frame.colour = "black"))+
    
    new_scale("fill") +
    
    geom_sf(data = map_prf, aes(fill = no_study)) +
    scale_fill_gradient(name = "Prefecture", low = "white", high = "#d95f02", na.value = "grey90", breaks = c(2, 4),limits = c(0, 6), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=2, title.hjust= 0.5, frame.colour = "black"))+
    
    new_scale("fill") +
    
    geom_sf(data = map_cty, aes(fill = no_study)) +
    geom_sf(data = nine, colour = "grey50", fill = "grey50", size = 0.5)+
    scale_fill_gradient(name = "County", low = "white", high = "#7570b3", na.value = "grey90", breaks = c(1, 2),limits = c(0, 3), 
                        guide = guide_colorsteps(even.steps = TRUE, show.limits = TRUE, title.position="top", barwidth = 12, order=3, title.hjust= 0.5, frame.colour = "black"))+
    facet_grid(cols =  vars(name)) +
    
    theme(panel.grid.major = element_line(color = grey(0.75),linetype = "dashed", size = 0.5), 
          panel.background = element_rect(fill = "white"),
          strip.background = element_blank(),
          legend.box = "horizontal",
          legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_text(size = 6),
          title = element_text(size = 8))
  
  return(MOR)
  
}


ILI <- ILI_func()
TPO <- TPO_func()
TPI<- TPI_func()
MOR <- MOR_func()


# Plot maps in grid
plot_no_legend <- plot_grid(ILI + theme(legend.position = "none"),
                            TPO + theme(legend.position = "none"),
                            TPI + theme(legend.position = "none"),
                            MOR + theme(legend.position = "none"),
                            labels = c("A", "B", "C", "D"),
                            ncol = 2)

legend <- get_legend(ILI + theme(legend.box.margin = margin(-20,0,0,5)))


# Plot
plot_grid(plot_no_legend, legend, ncol = 1, rel_heights = c(10, 1))


#Save
ggsave("output/Sup_fig_S3.png",
       width = 210,
       height = 230,
       dpi = 320,
       units = "mm")
