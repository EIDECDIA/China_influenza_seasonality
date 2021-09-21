### MMR results

# Produces results for:

# Figure 2 and MMR results

# Read in data and useful functions
source("script/data_and_functions.R")

# Figure 2 - Heatmap of influenza test positivity by strain

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

#### Mean monthly rates (MMR) ####

# MMR for all strains 
# Pre vs. post pandemic MMR (only provinces)
MMR_all <- month_data_all %>% 
  mutate(month = month(time)) %>% 
  group_by(strain, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% # mean of all studies 
  ungroup() %>% 
  mutate(season = if_else(date >= as.Date("2009-10-01") & date <= as.Date("2010-09-30"), "09/10", "non-pand")) %>% # Pandemic or non-pandemic 
  filter(season != "09/10") %>% # Exclude pandemic year
  mutate(pp = if_else(date <= as.Date("2009-09-30"), "Pre-pandemic", "Post-pandemic")) %>% # Pre or post pandemic
  mutate(pp = factor(pp, levels = c("Pre-pandemic", "Post-pandemic"))) %>% 
  mutate(month = month(time)) %>% 
  group_by(strain, geo_code, pp, month) %>% 
  summarise(month_mean = mean(mean, na.rm = TRUE)*100, # MMR by admin region for both pre and post pandemic 
            sd = sd(mean, na.rm = TRUE),
            n = n()) %>%
  mutate(geo_code = as.character(geo_code)) %>% 
  left_join(., shp_code, c("geo_code"="code")) %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>% 
  filter(n >= 2) %>%  # Filter to only regions with >=2 years data
  filter(prf == "00") %>% # Filter to only provinces
  mutate(m_name = month(month, label = TRUE, abbr = TRUE)) %>% 
  mutate(m_name_or = factor(m_name, levels = c( "Oct","Nov", "Dec",
                                                "Jan", "Feb","Mar", 
                                                "Apr", "May", "Jun",
                                                "Jul", "Aug", "Sep"))) %>% 
  mutate(strain_or = factor(strain, levels = c("All influenza strains", "A/H3N2", "A/H1N1pdm09", "B")))

# MMR for individual strains 
# Pre vs. post pandemic MMR (only provinces)
MMR_strain <- strain_comb %>% 
  mutate(month = month(time)) %>% 
  group_by(strain, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% # mean of all studies 
  ungroup() %>% 
  mutate(season = if_else(date >= as.Date("2009-10-01") & date <= as.Date("2010-09-30"), "09/10", "non-pand")) %>% # Pandemic or non-pandemic 
  filter(season != "09/10") %>% # Exclude pandemic year
  mutate(pp = if_else(date <= as.Date("2009-09-30"), "Pre-pandemic", "Post-pandemic")) %>% # Pre or post pandemic
  mutate(pp = factor(pp, levels = c("Pre-pandemic", "Post-pandemic"))) %>% 
  mutate(month = month(time)) %>% 
  group_by(strain, geo_code, pp, month) %>% 
  summarise(month_mean = mean(mean, na.rm = TRUE)*100, # MMR by admin region for both pre and post pandemic 
            sd = sd(mean, na.rm = TRUE),
            n = n()) %>%
  mutate(geo_code = as.character(geo_code)) %>% 
  left_join(., shp_code, c("geo_code"="code")) %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>% 
  filter(n >= 2) %>%  # Filter to only regions with >=2 years data
  filter(prf == "00") %>% # Filter to only provinces
  mutate(m_name = month(month, label = TRUE, abbr = TRUE)) %>% 
  mutate(m_name_or = factor(m_name, levels = c( "Oct","Nov", "Dec",
                                                "Jan", "Feb","Mar", 
                                                "Apr", "May", "Jun",
                                                "Jul", "Aug", "Sep"))) %>% 
  mutate(strain_or = factor(strain, levels = c("All influenza strains", "A/H3N2", "A/H1N1pdm09", "B")))


#### Plots ####

# All strain 
MMR_all_plot <- MMR_all %>% 
  filter(pp == "Post-pandemic") %>% 
  ggplot(., aes(x = m_name_or, y = lat_order)) +
  geom_tile(aes(fill = month_mean))+
  facet_grid(cols =  vars(strain_or)) +
  scale_fill_gradientn(name="MMR influenza test positivity (%)",
                       colours = my_pal(100), 
                       limits = c(0, 45), 
                       breaks= c(10, 20, 30, 40),
                       guide= guide_colorsteps(show.limits=FALSE, barwidth=12, title.position="top", frame.colour = "black")) +
  labs(y = "",x = "") +
  coord_cartesian(xlim = c(0.5, 12.5),expand=F)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.background = element_rect(fill ="grey90"),
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust =0))

# Individual strains 
MMR_strain_plot <- MMR_strain  %>% 
  filter(pp == "Post-pandemic") %>% 
  ggplot(., aes(x = m_name_or, y = lat_order)) +
  geom_tile(aes(fill = month_mean))+
  facet_grid(cols =  vars(strain_or)) +
  
  scale_fill_gradientn(name="MMR influenza test positivity (%)",
                       colours = my_pal(100), 
                       limits = c(0, 45), 
                       breaks= c(10, 20, 30, 40),
                       guide= guide_colorsteps(show.limits=FALSE, barwidth=12, title.position="top", frame.colour = "black")) +
  labs(y = "",x = "") +
  coord_cartesian(xlim = c(0.5, 12.5),expand=F)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.background = element_rect(fill ="grey90"),
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust =0))


#### Plot combined #### 
## FIG 2 ##

plot_no_legend <- plot_grid(
  MMR_all_plot + theme(legend.position = "none"),
  MMR_strain_plot + theme(legend.position = "none"), 
  labels = c("A", "B"),
  ncol = 1, rel_heights = c(0.55,0.45))

legend <- get_legend(MMR_all_plot + theme(legend.box.margin = margin(-10,10,10,60)))

plot_grid(plot_no_legend, legend, ncol = 1, rel_heights = c(10, 1), align = "h")

ggsave("output/Fig_2.png",
       width = 210,
       height = 200,
       dpi = 320,
       units = "mm")


#### Text results ####

# All strain  peak and low months across all provinces
# Post pandemic
MMR_all %>% 
  filter(pp == "Post-pandemic") %>% 
  group_by(month) %>% 
  summarise(mean = mean(month_mean, na.rm = TRUE),
            sd = sd(month_mean, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  filter(mean == max(mean) | mean == min(mean))

# Strain specific peak and low months across all provinces
# Post pandemic
MMR_strain %>% 
  filter(pp == "Post-pandemic") %>% 
  group_by(strain, month) %>% 
  summarise(mean = mean(month_mean, na.rm = TRUE),
            sd = sd(month_mean, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  filter(mean == max(mean) | mean == min(mean)) %>% 
  arrange(strain, mean)

# Min and Max prv MMR in highest and lowest mean months
# Post pandemic
MMR_all %>% 
  filter(pp == "Post-pandemic") %>% 
  filter(month == 1 | month == 10) %>% 
  group_by(strain, month) %>% 
  filter(month_mean == max(month_mean) | month_mean == min(month_mean)) %>% 
  arrange(month, month_mean)

# Max month in each province 
MMR_all %>% 
  group_by(lat_order, month) %>%
  summarise(mean = mean(month_mean, na.rm = TRUE)) %>%
  filter(mean == max(mean)) %>% 
  mutate(value = "max") %>% view()

# Min month in each province 
MMR_all %>% 
  group_by(lat_order, month) %>%
  summarise(mean = mean(month_mean, na.rm = TRUE)) %>%
  filter(mean == min(mean)) %>% 
  mutate(value = "min") %>% view()

