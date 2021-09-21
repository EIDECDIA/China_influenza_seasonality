### Epidemic intensity results

# Produces results for:

# Figure 4 and epidemic intensity results

# Read in data and useful functions
source("script/data_and_functions.R")

# Figure 4 - Epidemic intensity of influenza test positivity by strain

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

# Strains combined 
strain_comb <- bind_rows(month_data_H3, month_data_H1, month_data_B)


### Calculate intensity (all influenza)
all_inten <- month_data_all %>% 
  group_by(strain, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% 
  ungroup() %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  filter(prf == "00") %>% 
  mutate(season = if_else(date >= as.Date("2009-10-01") & date <= as.Date("2010-09-30"), "09/10", "non-pand")) %>% # Pandemic or non-pandemic 
  filter(season != "09/10") %>% #Exclude pandemic year
  mutate(month = month(date)) %>% 
  mutate(oct_month = if_else(month >= 10, month-9, month+3)) %>% 
  mutate(year = year(date)) %>%
  mutate(season = if_else(oct_month >= 1 & oct_month < 4, year, year-1)) %>% 
  mutate(season_n = paste(str_sub(season, 3), str_sub(as.character(season + 1),3), sep = "/")) %>% #Make flu seasons (Oct - Oct)
  group_by(geo_code, season_n) %>% 
  mutate(comp = if_else(max(oct_month)- min(oct_month)!= 11, "No", "Yes")) %>% # Is there a complete season of data?
  filter(comp == "Yes") %>% # Filter to complete seasons 
  mutate(flu = if_else(mean != 0, 1, 0)) %>% 
  mutate(flu_sum = sum(flu)) %>% 
  filter(flu_sum > 3) %>%       # Only include seasons where flu is recorded in more than 3 months of the year
  mutate(mean_sum = sum(mean)) %>% 
  mutate(frac = mean/mean_sum) %>%
  mutate(v = -frac * log(frac)^(-1)) %>% 
  mutate(intensity = sum(v)) %>% #Entropy
  ungroup() %>% 
  mutate(max = max(intensity, na.rm= TRUE)) %>% 
  mutate(min = min(intensity, na.rm = TRUE)) %>% 
  mutate(adj_in = ((intensity - min)/(max-min))) %>% # Normalise between 0-1 for all provinces in each season
  group_by(geo_code, season_n) %>% 
  filter(row_number()==1) %>%
  group_by(geo_code) %>% 
  summarise(mean_int = mean(adj_in, na.rm = TRUE),
            sd = sd(adj_in, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_95 = mean_int - qt(1 - (0.05 / 2), n - 1) * se,
         upper_95 = mean_int + qt(1 - (0.05 / 2), n - 1) * se,
         lower_75 = mean_int - qt(1 - (0.25 / 2), n - 1) * se,
         upper_75 = mean_int + qt(1 - (0.25 / 2), n - 1) * se) %>% 
  left_join(., shp_code, c("geo_code"="code")) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>% 
  mutate(strain = "All Influenza Strains") %>% 
  mutate(chn_mean = mean(mean_int))

# Plot
all_inten_plot <- all_inten %>% 
  ggplot()+
  geom_linerange(aes(x=mean_int, y= lat_order, xmin=lower_95, xmax= upper_95), size= 2, alpha= 0.3, color = "#ff7f00")+
  geom_linerange(aes(x=mean_int, y= lat_order, xmin=lower_75, xmax= upper_75), size= 2, alpha= 0.5, color = "#ff7f00")+
  geom_point(aes(x=mean_int, y= lat_order), size= 1.5, shape = 15, alpha = 1, color = "#ff7f00")+
  geom_vline(aes(xintercept = chn_mean), linetype = "dashed", color = "grey50")+
  facet_wrap(~strain)+
  scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1)) +
  labs(x = "Normalised mean epidemic intensity, v")+
  coord_cartesian(xlim = c(0, 1.1), ylim = c(0.5,30.5),expand=F)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_text(hjust = 0, size = 7),
        axis.text.x = element_text(vjust = 0.3, hjust =0.5),
        axis.title.y = element_blank())

# Calculate intensity (by strain)
strain_inten <- strain_comb %>% 
  group_by(strain, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% 
  ungroup() %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  filter(prf == "00") %>% 
  mutate(season = if_else(date >= as.Date("2009-10-01") & date <= as.Date("2010-09-30"), "09/10", "non-pand")) %>% # Pandemic or non-pandemic 
  filter(season != "09/10") %>% #Exclude pandemic year
  mutate(month = month(date)) %>% 
  mutate(oct_month = if_else(month >= 10, month-9, month+3)) %>% 
  mutate(year = year(date)) %>%
  mutate(season = if_else(oct_month >= 1 & oct_month < 4, year, year-1)) %>% 
  mutate(season_n = paste(str_sub(season, 3), str_sub(as.character(season + 1),3), sep = "/")) %>% #Make flu seasons (Oct - Oct)
  group_by(strain, geo_code, season_n) %>% 
  mutate(comp = if_else(max(oct_month)- min(oct_month)!= 11, "No", "Yes")) %>%  # Is there a complete season of data?
  filter(comp == "Yes") %>% # Filter to complete seasons 
  mutate(flu = if_else(mean != 0, 1, 0)) %>% 
  mutate(flu_sum = sum(flu)) %>% 
  filter(flu_sum > 3 ) %>%  # Only include seasons where flu is recorded in more than 3 months of the year
  mutate(strain_season = paste0(strain,season_n)) %>% 
  filter(strain_season != "A/H1N1pdm0911/12") %>% 
  filter(strain_season != "A/H1N1pdm0914/15") %>% 
  filter(strain_season != "B12/13") %>% 
  mutate(mean_sum = sum(mean)) %>% 
  mutate(frac = mean/mean_sum) %>%
  mutate(v = -frac * log(frac)^(-1)) %>% 
  mutate(intensity = sum(v)) %>% #Entropy
  ungroup() %>% 
  mutate(max = max(intensity, na.rm= TRUE)) %>% 
  mutate(min = min(intensity, na.rm = TRUE)) %>% 
  mutate(adj_in = ((intensity - min)/(max-min))) %>% # Normalise between 0-1 for all provinces in each season
  group_by(strain, geo_code, season_n) %>% 
  filter(row_number()==1) %>%
  group_by(strain, geo_code) %>% 
  summarise(mean_int = mean(adj_in, na.rm = TRUE),
            sd = sd(adj_in, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_95 = mean_int - qt(1 - (0.05 / 2), n - 1) * se,
         upper_95 = mean_int + qt(1 - (0.05 / 2), n - 1) * se,
         lower_75 = mean_int - qt(1 - (0.25 / 2), n - 1) * se,
         upper_75 = mean_int + qt(1 - (0.25 / 2), n - 1) * se) %>% 
  ungroup() %>% 
  left_join(., shp_code, c("geo_code"="code")) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>%
  mutate(strain = factor(strain, levels = c("A/H3N2", "A/H1N1pdm09", "B"))) %>% 
  group_by(strain) %>% 
  mutate(chn_mean = mean(mean_int))

# Plot
strain_inten_plot <- strain_inten %>% 
  ggplot()+
  geom_linerange(aes(x=mean_int, y= lat_order, xmin=lower_95, xmax= upper_95, color = strain), size= 2, alpha= 0.3) +
  geom_linerange(aes(x=mean_int, y= lat_order, xmin=lower_75, xmax= upper_75, color = strain), size= 2, alpha= 0.5) +
  geom_point(aes(x=mean_int, y= lat_order, color = strain), size= 1.5, shape = 15, alpha = 1)+
  geom_vline(aes(xintercept = chn_mean), linetype = "dashed", color = "grey50")+
  scale_color_brewer(palette = "Set2")+
  scale_x_continuous(breaks = c(0,0.25,0.5,0.75,1)) +
  labs(x = "Normalised mean epidemic intensity, v")+
  facet_wrap(~strain, ncol = 3)+
  coord_cartesian(xlim = c(0, 1.1), 
                  ylim = c(0.5,24.5),
                  expand=F)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_blank(),
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(5, "mm"),
        axis.text.y = element_text(hjust = 0, size = 7),
        axis.text.x = element_text(vjust = 0.3, hjust =0.5),
        axis.title.y = element_blank())


##### Combined plot
plot_grid(
  all_inten_plot + theme(legend.position = "none"),
  strain_inten_plot + theme(legend.position = "none"), 
  labels = c("A", "B"),
  ncol = 1, rel_heights = c(0.55,0.45))

ggsave("output/Fig_4.png",
       width = 210,
       height = 180,
       dpi = 320,
       units = "mm")


# Mean intesity across all provinces 
strain_inten %>% 
  group_by(strain) %>% 
  mutate(prv_mean = mean_int) %>% 
  summarise(mean_int = mean(prv_mean, na.rm = TRUE),
            sd = sd(prv_mean, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_95 = mean_int - qt(1 - (0.05 / 2), n - 1) * se,
         upper_95 = mean_int + qt(1 - (0.05 / 2), n - 1) * se,
         lower_75 = mean_int - qt(1 - (0.25 / 2), n - 1) * se,
         upper_75 = mean_int + qt(1 - (0.25 / 2), n - 1) * se) 
