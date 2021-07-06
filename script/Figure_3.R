### Epidemic onset and duration

# Produces results for:

# Figure 3, Figure S15 - S17, & onset month and epidemic length results

# Read in data and useful functions
source("script/data_and_functions.R")

# Figure 3 - Epidemic maps of influenza test positivity by strain

# Raw TS for all strains 
month_data_all <- monthly_long_function("Positive rate", "All", "ILI outpatient") %>% 
  mutate(strain = "All influenza strains")

# Raw TS for A/H3N2
month_data_H3 <- monthly_long_function("Positive rate", "A/H3N2", "ILI outpatient") %>% 
  mutate(strain = "A/H3N2")

# Raw TS for A/H1N1pdm09
month_data_H1 <- monthly_long_function("Positive rate", "A/H1N1pdm09", "ILI outpatient") %>% 
  mutate(strain = "A/H1N1pdm09")

# Raw TS for B 
month_data_B <- monthly_long_function("Positive rate", "B", "ILI outpatient") %>% 
  mutate(strain = "B")


### ALL STRAIN MAP FUNCTION

plot_epidemic_map <- function(STRAIN){
  
  # Raw TS for indiviual strains bind
  strain_comb <- bind_rows(month_data_all, month_data_H3, month_data_H1, month_data_B)
  
  #### Mean monthly rates (MMR) ####
  
  # MMR for all strains 
  MMR_all <- strain_comb %>% 
    mutate(month = month(time)) %>% 
    group_by(strain, geo_code, time) %>% 
    summarise(mean = mean(x), date = max(t)) %>% 
    ungroup() %>% 
    mutate(month = month(time)) %>% 
    group_by(strain, geo_code, month) %>% 
    summarise(month_mean = mean(mean, na.rm = TRUE),
              sd = sd(mean, na.rm = TRUE),
              n = n()) %>%
    mutate(se = sd / sqrt(n),
           lower_ci = month_mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper_ci = month_mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
    left_join(., shp_code, c("geo_code"="code")) %>% 
    filter(n >= 2)
  
  
  ### Map data with prf and cty 
  epi_data <- MMR_all %>% 
    filter(strain == STRAIN) %>% 
    select(1:4) %>% 
    group_by(geo_code) %>% 
    mutate(sum = sum(month_mean)) %>% 
    mutate(proportion = (month_mean/sum)) %>% 
    arrange(desc(proportion)) %>% 
    mutate(cumsum = cumsum(proportion)) %>% 
    mutate(epidemic = if_else(cumsum <= 0.75, "Epidemic", "Non-epidemic")) %>%
    mutate(prv = substr(geo_code, 1,2)) %>% 
    mutate(prf = substr(geo_code, 3,4)) %>% 
    mutate(cty = substr(geo_code, 5,6)) %>% 
    mutate(month_name = month(month, label = TRUE, abbr = FALSE)) %>% 
    mutate(oct_month = if_else(month >= 10, month-9, month+3)) %>% 
    group_by(geo_code, epidemic) %>% 
    mutate(onset = min(oct_month)) %>% 
    group_by(geo_code) %>% 
    mutate(epidemic = if_else(epidemic == "Epidemic" & oct_month == onset, "Epidemic onset", epidemic)) %>% 
    right_join(., shp, by = c("prv"="prv", "prf" = "prf", "cty"="cty")) %>% 
    st_as_sf() %>% 
    mutate(month_name = month(month, label = TRUE, abbr = FALSE)) %>% 
    mutate(epidemic_ord = factor(epidemic, levels = rev(c("Non-epidemic", "Epidemic onset", "Epidemic", "No data")))) %>% 
    mutate(oct_name = factor(month_name, levels = c("October", "November", "December",
                                                    "January", "February", "March",
                                                    "April", "May", "June",
                                                    "July", "August", "September"))) %>% 
    filter(!is.na(month_name))
  
  # Blank layer
  blank_shp <- shp %>% 
    as_tibble() %>% 
    st_as_sf() %>% 
    filter(prf == "00") %>% 
    mutate(epidemic_ord = "No data") %>% 
    mutate(epidemic_ord = factor(epidemic_ord, levels = rev(c("Non-epidemic", "Epidemic onset", "Epidemic", "No data")))) 
  
  # Prv layer
  epi_map_prv <- epi_data %>% 
    filter(prf == "00") %>% 
    filter(cty == "00")
  
  # Prf layer
  epi_map_prf <- epi_data %>% 
    filter(prf != "00") %>% 
    filter(cty == "00")
  
  # Cty layer
  epi_map_cty <- epi_data %>% 
    filter(prf != "00") %>% 
    filter(cty != "00")
  
  # Plot map
  ggplot()+
    geom_sf(data = blank_shp, aes(fill=epidemic_ord))+
    geom_sf(data = epi_map_prv, aes(fill=epidemic_ord))+
    geom_sf(data = epi_map_prf,aes(fill=epidemic_ord))+
    geom_sf(data = epi_map_cty,aes(fill=epidemic_ord))+
    geom_sf(data = nine, colour = "grey50", fill = "grey50", size = 0.5)+
    facet_wrap(~oct_name, ncol = 3) +
    scale_fill_manual(values =  c("Non-epidemic" = "#91cf60", "Epidemic onset" = "#ffffbf", "Epidemic" = "#fc8d59", "No data" = "grey90"), 
                      breaks = c("Non-epidemic", "Epidemic onset", "Epidemic", "No data")) +
    theme(panel.grid.major = element_line(color = grey(0.75),linetype = "dashed", size = 0.5), 
          panel.background = element_rect(fill = "white"),
          strip.background = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 6),
          title = element_text(size = 8))
  
}


# All strain plot
plot_epidemic_map("All influenza strains")

ggsave("output/Fig_3.png",
       width = 170,
       height = 240,
       dpi = 320,
       units = "mm")


# A/H3N2 plot
plot_epidemic_map("A/H3N2")

ggsave("output/Sup_fig_S15.png",
       width = 170,
       height = 240,
       dpi = 320,
       units = "mm")


# A/H1N1pdm09 plot
plot_epidemic_map("A/H1N1pdm09")

ggsave("output/Sup_fig_S16.png",
       width = 170,
       height = 240,
       dpi = 320,
       units = "mm")


# B plot
plot_epidemic_map("B")

ggsave("output/Sup_fig_S17.png",
       width = 170,
       height = 240,
       dpi = 320,
       units = "mm")


### Epidemic length and onset month results - Province level

epi_dur <- function(STRAIN){
  
  #Shape code
  shp_code <- shp %>% 
    as_tibble() %>% 
    select(prv,prf,cty, PYNAME) %>%
    mutate(code = paste0(prv,prf,cty)) %>% 
    select(PYNAME, code)
  
  #### Mean monthly rates (MMR) ####
  
  # MMR for all strains 
  MMR_all <- bind_rows(month_data_all, month_data_H3, month_data_H1, month_data_B) %>% 
    mutate(month = month(time)) %>% 
    group_by(strain, geo_code, time) %>% 
    summarise(mean = mean(x), date = max(t)) %>% 
    ungroup() %>% 
    mutate(month = month(time)) %>% 
    group_by(strain, geo_code, month) %>% 
    summarise(month_mean = mean(mean, na.rm = TRUE),
              sd = sd(mean, na.rm = TRUE),
              n = n()) %>%
    mutate(se = sd / sqrt(n),
           lower_ci = month_mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper_ci = month_mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
    left_join(., shp_code, c("geo_code"="code"))
  
  
  ### Map data with prf and cty 
  epi_length <- MMR_all %>% 
    filter(strain == STRAIN) %>% 
    select(1:4) %>% 
    group_by(geo_code) %>% 
    mutate(sum = sum(month_mean)) %>% 
    mutate(proportion = (month_mean/sum)) %>% 
    arrange(desc(proportion)) %>% 
    mutate(cumsum = cumsum(proportion)) %>% 
    mutate(epidemic = if_else(cumsum <= 0.75, "Epidemic", "Non-epidemic")) %>%
    mutate(prv = substr(geo_code, 1,2)) %>% 
    mutate(prf = substr(geo_code, 3,4)) %>% 
    mutate(cty = substr(geo_code, 5,6)) %>% 
    filter(prf == "00") %>%
    mutate(epi_no = if_else(epidemic == "Epidemic", 1, 0)) %>% 
    summarise(epi_month = sum(epi_no)) %>% 
    left_join(., shp_code, c("geo_code"="code")) %>%
    mutate(PYNAME = factor(PYNAME, levels = geo_lat)) %>% 
    arrange(desc(PYNAME))
  
  onset_month <- MMR_all %>% 
    filter(strain == STRAIN) %>% 
    select(1:4) %>% 
    group_by(geo_code) %>% 
    mutate(sum = sum(month_mean)) %>% 
    mutate(proportion = (month_mean/sum)) %>% 
    arrange(desc(proportion)) %>% 
    mutate(cumsum = cumsum(proportion)) %>% 
    mutate(epidemic = if_else(cumsum <= 0.75, "Epidemic", "Non-epidemic")) %>%
    mutate(prv = substr(geo_code, 1,2)) %>% 
    mutate(prf = substr(geo_code, 3,4)) %>% 
    mutate(cty = substr(geo_code, 5,6)) %>% 
    filter(prf == "00") %>% 
    mutate(month_name = month(month, label = TRUE, abbr = FALSE)) %>% 
    mutate(oct_month = if_else(month >= 10, month-9, month+3)) %>% 
    group_by(geo_code, epidemic) %>% 
    mutate(onset = min(oct_month)) %>% 
    group_by(geo_code) %>% 
    mutate(epidemic = if_else(epidemic == "Epidemic" & oct_month == onset, "Epidemic onset", epidemic)) %>% 
    right_join(., shp, by = c("prv"="prv", "prf" = "prf", "cty"="cty")) %>% 
    st_as_sf() %>% 
    mutate(month_name = month(month, label = TRUE, abbr = FALSE)) %>% 
    mutate(epidemic_ord = factor(epidemic, levels = rev(c("Non-epidemic", "Epidemic onset", "Epidemic", "No data")))) %>% 
    mutate(oct_name = factor(month_name, levels = c("October", "November", "December",
                                                    "January", "February", "March",
                                                    "April", "May", "June",
                                                    "July", "August", "September"))) %>% 
    filter(!is.na(month_name)) %>% 
    filter(epidemic_ord == "Epidemic onset") %>% 
    as_tibble() %>% 
    select(geo_code, month_name, epidemic)
  
  epidemic_table <- left_join(epi_length, onset_month, by = c("geo_code" = "geo_code")) %>% 
    mutate(strain = STRAIN) %>% 
    mutate(month_mean = mean(epi_month),
           sd = sd(epi_month, na.rm = TRUE),
           n = n()) %>%
    mutate(se = sd / sqrt(n),
           lower_ci = month_mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper_ci = month_mean + qt(1 - (0.05 / 2), n - 1) * se)
  
  
  return(epidemic_table)
  
}

# Epidemic length and onset in each province and overall mean (all strain)
epi_dur("All influenza strains") 

# Epidemic length and onset in each province and overall mean (A/H3N2)
epi_dur("A/H3N2") 

# Epidemic length and onset in each province and overall mean (A/H1N1pdm09)
epi_dur("A/H1N1pdm09") 

# Epidemic length and onset in each province and overall mean (B)
epi_dur("B") 

