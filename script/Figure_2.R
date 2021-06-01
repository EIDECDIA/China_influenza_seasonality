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
MMR_all <- month_data_all %>% 
  mutate(month = month(time)) %>% 
  group_by(strain, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% # mean of all studies 
  ungroup() %>% 
  mutate(month = month(time)) %>% 
  group_by(strain, geo_code, month) %>% 
  summarise(month_mean = mean(mean, na.rm = TRUE)*100, # MMR 
            sd = sd(mean, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = month_mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = month_mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  mutate(geo_code = as.character(geo_code)) %>% 
  left_join(., shp_code, c("geo_code"="code")) %>% 
  filter(n >= 2) # only prv with >2 years data

# MMR for individual strains 
MMR_strain <- strain_comb %>% 
  mutate(month = month(time)) %>% 
  group_by(strain, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% 
  ungroup() %>% 
  mutate(month = month(time)) %>% 
  group_by(strain, geo_code, month) %>% 
  summarise(month_mean = mean(mean, na.rm = TRUE)*100,
            sd = sd(mean, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = month_mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = month_mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  mutate(geo_code = as.character(geo_code)) %>% 
  left_join(., shp_code, c("geo_code"="code")) %>% 
  filter(n >= 2) # only prv with >2 years data


#### Plots ####

# All strain 
MMR_all_plot <- MMR_all %>%  
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>% 
  filter(prf == "00") %>% 
  mutate(date = ymd(200101)+ months(month-1)) %>% 
  mutate(m_name = as.character(month(date, label = TRUE, abbr = TRUE))) %>% 
  mutate(m_name_or = factor(m_name, levels = c( "Oct","Nov", "Dec",
                                                "Jan", "Feb","Mar", 
                                                "Apr", "May", "Jun",
                                                "Jul", "Aug", "Sep"))) %>% 
  mutate(strain_or = factor(strain, levels = c("All influenza strains", "A/H3N2", "A/H1N1pdm09", "B"))) %>% 
  ggplot(., aes(x = m_name_or, y = lat_order)) +
  geom_tile(aes(fill = month_mean))+
  facet_grid(cols =  vars(strain_or)) +
  scale_fill_continuous_sequential(name="MMR influenza test positivity (%)", 
                                   palette = "Oranges", 
                                   breaks= c(5, 10, 15, 20, 25), 
                                   guide=guide_colorsteps(show.limits=FALSE, barwidth=12, title.position="top", frame.colour = "black"))+
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
MMR_strain_plot <- MMR_strain %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>% 
  filter(prf == "00") %>% 
  mutate(date = ymd(200101)+ months(month-1)) %>% 
  mutate(m_name = as.character(month(date, label = TRUE, abbr = TRUE))) %>% 
  mutate(m_name_or = factor(m_name, levels = c( "Oct","Nov", "Dec",
                                                "Jan", "Feb","Mar", 
                                                "Apr", "May", "Jun",
                                                "Jul", "Aug", "Sep"))) %>% 
  mutate(strain_or = factor(strain, levels = c("All influenza strains", "A/H3N2", "A/H1N1pdm09", "B"))) %>% 
  ggplot(., aes(x = m_name_or, y = lat_order)) +
  geom_tile(aes(fill = month_mean))+
  facet_grid(cols =  vars(strain_or)) +
  
  scale_fill_continuous_sequential(name="MMR influenza test positivity (%)", 
                                   palette = "Oranges", 
                                   breaks= c( 5, 1, 15, 2, 25), 
                                   guide=guide_colorsteps(show.limits=FALSE, barwidth=8, title.position="top", frame.colour = "black"))+
  labs(y = "",x = "") +
  coord_cartesian(xlim = c(0.5, 12.5),expand=F)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.background = element_rect(fill ="grey90"),
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(5, "mm"),
        axis.text.y = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(size = 6, angle = 90, vjust = 0.3, hjust =0))


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

# Average rate in each month (all strain, all prv)
MMR_all %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>% 
  filter(prf == "00") %>% 
  group_by(month) %>% 
  summarise(mean = mean(month_mean, na.rm = TRUE),
            sd = sd(month_mean, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se) 

# All provinces in Jan
MMR_all %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>% 
  filter(prf == "00") %>% 
  filter(month == 1) %>% 
  arrange(month_mean)

# All provinces in Jun
MMR_all %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>% 
  filter(prf == "00") %>% 
  filter(month == 6) %>% 
  arrange(month_mean)

# Strain specific peak months across all provinces
MMR_strain %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>% 
  filter(prf == "00") %>% 
  group_by(strain, month) %>% 
  summarise(mean = mean(month_mean, na.rm = TRUE),
            sd = sd(month_mean, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  group_by(strain) %>% 
  filter(mean == max(mean))

# Max and min months in each province 
MMR_all %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>% 
  filter(prf == "00") %>% 
  group_by(lat_order, month) %>%
  summarise(mean = mean(month_mean, na.rm = TRUE)) %>%
  filter(mean == max(mean)) %>% 
  mutate(value = "max") %>% view()

MMR_all %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  mutate(lat_order = factor(PYNAME, levels = geo_lat)) %>% 
  filter(prf == "00") %>% 
  group_by(lat_order, month) %>%
  summarise(mean = mean(month_mean, na.rm = TRUE)) %>%
  filter(mean == min(mean)) %>% 
  mutate(value = "min") %>% view()

