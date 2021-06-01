### Search results x GDP PC and population

# Produces results for:

# Figure S4 GDP PC and population results

# Read in data and useful functions
source("script/data_and_functions.R")

# Figure S4 - Box plots

# Province GDP data
gdpall_prv <- read_rds("covar/GDP_prv.rds") %>% 
  mutate(gdp = `2010`) %>% 
  select(gdp, prv,prf,cty) %>% 
  mutate(code = paste0(prv,prf,cty))

# Prefecture GDP data 
gdpall_prf <- read_rds("covar/GDP_prf.rds") %>% 
  mutate(gdp = `2010`) %>% 
  select(gdp, prv,prf,cty) %>% 
  mutate(code = paste0(prv,prf,cty))

# County GDP data 
gdpall_cty <- read_rds("covar/GDP_cty_all.rds") %>% 
  mutate(gdp = `2010`) %>% 
  select(gdp, prv,prf,cty) %>% 
  mutate(code = paste0(prv,prf,cty))

# Bind all admin regions GDP data
gdp_all <- bind_rows(gdpall_prv, gdpall_prf, gdpall_cty)

# Total population data (2010 census)
all_pop <- read_rds("covar/raw_census_pop_2010.rds")[[1]] %>% 
  select(tot, prv, prf, cty) %>% 
  mutate(tot = as.numeric(tot)) %>% 
  mutate(code = paste0(prv,prf,cty))

# Calulate GDP per capita (USD)
gdp_pc <- full_join(gdp_all, all_pop, by = c("code" = "code")) %>% 
  select(prv = prv.x, prf = prf.x, cty = cty.x, code, gdp, tot) %>% 
  mutate(gdp_pc = gdp/tot) %>% 
  mutate(gdp_pc_dol = gdp_pc/3.3106) %>% 
  mutate(lvl = "prv") %>% 
  mutate(lvl = if_else(prf != "00" & cty == "00", "prf", lvl)) %>% 
  mutate(lvl = if_else(cty != "00", "cty", lvl)) %>% 
  mutate(study = "China")


# GDP and pop data for admin regions reporting ILI
ILI <- DES_geo %>% 
  filter(measure == "ILI %") %>% 
  filter(pop_denom == "outpatient") %>% 
  filter(strain == "N/A") %>% 
  select(geo_code) %>% 
  distinct() %>% 
  left_join(., gdp_pc, by = c("geo_code" = "code")) %>% 
  mutate(study = "ILI") %>% 
  bind_rows(gdp_pc) %>% 
  filter(!is.na(lvl)) %>% 
  mutate(outcome = "ILI")

# GDP and pop data for admin regions reporting TPO
TPO <- DES_geo %>% 
  filter(measure == "Positive rate") %>% 
  filter(pop_denom == "ILI outpatient") %>% 
  filter(strain == "All") %>% 
  select(geo_code) %>% 
  distinct() %>% 
  left_join(., gdp_pc, by = c("geo_code" = "code")) %>% 
  mutate(study = "TPO") %>% 
  bind_rows(gdp_pc) %>% 
  filter(!is.na(lvl)) %>% 
  mutate(outcome = "TPO")

# GDP and pop data for admin regions reporting TPI
TPI <- DES_geo %>% 
  filter(measure == "Positive rate") %>% 
  filter(pop_denom == "SARI") %>% 
  filter(strain == "All") %>% 
  select(geo_code) %>% 
  distinct() %>% 
  left_join(., gdp_pc, by = c("geo_code" = "code")) %>% 
  mutate(study = "TPI") %>% 
  bind_rows(gdp_pc) %>% 
  filter(!is.na(lvl)) %>% 
  mutate(outcome = "TPI")

# GDP and pop data for admin regions reporting MOR
MOR <- DES_geo %>% 
  filter(measure == "Mort_flu") %>%
  filter(pop_denom == "mortality_resp" | pop_denom == "mortality_all" ) %>% 
  filter(strain == "N/A") %>% 
  select(geo_code) %>% 
  distinct() %>% 
  left_join(., gdp_pc, by = c("geo_code" = "code")) %>% 
  mutate(study = "MOR") %>% 
  bind_rows(gdp_pc) %>% 
  filter(!is.na(lvl)) %>% 
  mutate(outcome = "MOR")


# Plot A - GDP PC
a<- bind_rows(ILI, TPO, TPI, MOR) %>% 
  mutate(lvl2 = if_else(lvl == "prv", "Province", lvl)) %>% 
  mutate(lvl2 = if_else(lvl == "prf", "Prefecture", lvl2)) %>% 
  mutate(lvl2 = if_else(lvl == "cty", "County", lvl2)) %>% 
  mutate(lvl2 = factor(lvl2, levels = c("Province", "Prefecture", "County"))) %>% 
  mutate(study = factor(study, levels = c("China", "ILI", "TPO", "TPI", "MOR"))) %>% 
  mutate(china = if_else(study == "China", "Mainland China", "Represented in study")) %>% 
  filter(gdp_pc_dol <= 40000) %>% 
  ggplot()+
  geom_boxplot(aes(x = study , y = gdp_pc_dol, fill = china))+
  scale_fill_manual(values = c("#fc8d62", "#66c2a5"))+
  facet_wrap(~lvl2, scales = "free_y")+
  labs(y = "GDP per capita (USD)")+
  theme_cowplot()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank())

# Plot B - Total population
b <- bind_rows(ILI, TPO, TPI, MOR) %>% 
  mutate(lvl2 = if_else(lvl == "prv", "Province", lvl)) %>% 
  mutate(lvl2 = if_else(lvl == "prf", "Prefecture", lvl2)) %>% 
  mutate(lvl2 = if_else(lvl == "cty", "County", lvl2)) %>% 
  mutate(lvl2 = factor(lvl2, levels = c("Province", "Prefecture", "County"))) %>% 
  mutate(study = factor(study, levels = c("China", "ILI", "TPO", "TPI", "MOR"))) %>% 
  mutate(china = if_else(study == "China", "Mainland China", "Represented in study")) %>% 
  mutate(tot_mil = tot/1000000) %>% 
  ggplot()+
  geom_boxplot(aes(x = study , y = tot_mil, fill = china))+
  scale_fill_manual(values = c("#fc8d62", "#66c2a5"))+
  facet_wrap(~lvl2, scales = "free_y")+
  labs(y = "Total population (millions)")+
  theme_cowplot()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank())


#### Plot combined #### 

plot_no_legend <- plot_grid(
  a + theme(legend.position = "none"),
  b + theme(legend.position = "none"), 
  labels = c("A", "B"),
  ncol = 1, rel_heights = c(0.5,0.5),
  align = "v")

legend <- get_legend(a + theme(legend.box.margin = margin(10,0,10,210)))

plot_grid(plot_no_legend, legend, ncol = 1, rel_heights = c(10, 1))

ggsave("output/Sup_fig_S4.png",
       width = 210,
       height = 180,
       dpi = 320,
       units = "mm")


## Results for manuscript text 

# ILI prf all China 
bind_rows(ILI, TPO, TPI, MOR) %>% 
  mutate(lvl2 = if_else(lvl == "prv", "Province", lvl)) %>% 
  mutate(lvl2 = if_else(lvl == "prf", "Prefecture", lvl2)) %>% 
  mutate(lvl2 = if_else(lvl == "cty", "County", lvl2)) %>% 
  mutate(lvl2 = factor(lvl2, levels = c("Province", "Prefecture", "County"))) %>% 
  mutate(study = factor(study, levels = c("All", "ILI", "TPO", "TPI", "MOR"))) %>% 
  mutate(china = if_else(study == "All", "Mainland China", "Represented in study")) %>% 
  mutate(tot_mil = tot/1000000) %>% 
  filter(lvl == "prf") %>% 
  group_by(study) %>% 
  summarise(mean_pop = mean(tot_mil, na.rm = TRUE),
            sd = sd(tot_mil, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = mean_pop - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = mean_pop + qt(1 - (0.05 / 2), n - 1) * se)


