### MMR results

# Produces results for:

# Figure S14 and Prv x prf/cty same peak month

# Read in data and useful functions
source("script/data_and_functions.R")

# Figure S14 - Geo_facet plot and same peaking moht results

# Read in all cleaned positive rate data (all strain)
month_data_all <- monthly_long_function("Positive rate", "All", "ILI outpatient")

# Province level MMR
MMR_prv <- month_data_all %>% 
  mutate(month = month(time)) %>% 
  group_by(geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% # mean of all studies 
  ungroup() %>% 
  mutate(season = if_else(date >= as.Date("2009-10-01") & date <= as.Date("2010-09-30"), "09/10", "non-pand")) %>% # Pandemic or non-pandemic 
  filter(season != "09/10") %>% # Exclude pandemic year
  mutate(pp = if_else(date <= as.Date("2009-09-30"), "Pre-pandemic", "Post-pandemic")) %>% # Pre or post pandemic
  mutate(pp = factor(pp, levels = c("Pre-pandemic", "Post-pandemic"))) %>% 
  filter(pp == "Post-pandemic") %>% 
  mutate(month = month(time)) %>% 
  group_by(geo_code, month) %>% 
  summarise(month_mean = mean(mean, na.rm = TRUE)*100, # MMR 
            sd = sd(mean, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = month_mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = month_mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  left_join(., geo_name, c("geo_code"="code")) %>% 
  filter(n >= 2) %>% # only prv with >2 years data
  mutate(full_year = sum(month)) %>% 
  filter(full_year == 78) %>% # only include regions with full year
  mutate(date = ymd(200101)+ months(month-1)) %>% 
  mutate(m_name = as.character(month(date, label = TRUE, abbr = TRUE))) %>% 
  mutate(m_name_or = factor(m_name, levels = c( "Oct","Nov", "Dec",
                                                "Jan", "Feb","Mar", 
                                                "Apr", "May", "Jun",
                                                "Jul", "Aug", "Sep"))) %>% 
  mutate(prv_true = if_else(prf == "00", "Province", "Sub-province")) %>% 
  mutate(prv_true = if_else(prf != "00" & cty == "00", "Prefecture", prv_true)) %>% 
  mutate(prv_true = if_else(prf != "00" & cty != "00", "County", prv_true)) %>% 
  filter(prv_true == "Province") %>% 
  mutate(name = as.character(prv_name)) %>% 
  mutate(prv_true = factor(prv_true, levels = c("County", "Prefecture", "Province")))

# Sub province level
MMR_sub <-  month_data_all %>% 
  mutate(month = month(time)) %>% 
  group_by(geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% # mean of all studies 
  ungroup() %>% 
  mutate(season = if_else(date >= as.Date("2009-10-01") & date <= as.Date("2010-09-30"), "09/10", "non-pand")) %>% # Pandemic or non-pandemic 
  filter(season != "09/10") %>% # Exclude pandemic year
  mutate(pp = if_else(date <= as.Date("2009-09-30"), "Pre-pandemic", "Post-pandemic")) %>% # Pre or post pandemic
  mutate(pp = factor(pp, levels = c("Pre-pandemic", "Post-pandemic"))) %>% 
  filter(pp == "Post-pandemic") %>% 
  mutate(month = month(time)) %>% 
  group_by(geo_code, month) %>% 
  summarise(month_mean = mean(mean, na.rm = TRUE)*100, # MMR 
            sd = sd(mean, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = month_mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = month_mean + qt(1 - (0.05 / 2), n - 1) * se) %>% 
  left_join(., geo_name, c("geo_code"="code")) %>% 
  filter(n >= 2) %>% # only prv with >2 years data
  mutate(full_year = sum(month)) %>% 
  filter(full_year == 78) %>% # only include regions with full year
  mutate(date = ymd(200101)+ months(month-1)) %>% 
  mutate(m_name = as.character(month(date, label = TRUE, abbr = TRUE))) %>% 
  mutate(m_name_or = factor(m_name, levels = c( "Oct","Nov", "Dec",
                                                "Jan", "Feb","Mar", 
                                                "Apr", "May", "Jun",
                                                "Jul", "Aug", "Sep"))) %>% 
  mutate(prv_true = if_else(prf == "00", "Province", "Sub-province")) %>% 
  mutate(prv_true = if_else(prf != "00" & cty == "00", "Prefecture", prv_true)) %>% 
  mutate(prv_true = if_else(prf != "00" & cty != "00", "County", prv_true)) %>% 
  filter(prv_true == "Prefecture" | prv_true == "County") %>% 
  mutate(name = as.character(prv_name)) %>% 
  mutate(prv_true = factor(prv_true, levels = c("County", "Prefecture", "Province")))


# Geo facet grid 
CHN_GF <- data.frame(
  code = c("HLJ", "NM", "JL", "BJ", "LN", "XJ", "GS", "SHX", "HEB", "TJ", "XZ", "QH", "NX", "SAX", "HEN", "SD", "SC", "CQ", "HUB", "AH", "JS", "SH", "YN", "GZ", "HUN", "JX", "ZJ", "GX", "GD", "FJ", "HAN"),
  row = c(1, 2, 2, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 9),
  col = c(7, 6, 7, 6, 7, 1, 2, 4, 5, 6, 1, 2, 3, 4, 5, 6, 2, 3, 4, 5, 6, 7, 2, 3, 4, 5, 6, 4, 5, 6, 5),
  name = c("Heilongjiang", "Inner Mongolia", "Jilin", "Beijing", "Liaoning", "Xinjiang", "Gansu", "Shanxi", "Hebei", "Tianjin", "Tibet", "Qinghai", "Ningxia", "Shaanxi", "Henan", "Shandong", "Sichuan", "Chongqing", "Hubei", "Anhui", "Jiangsu", "Shanghai", "Yunnan", "Guizhou", "Hunan", "Jiangxi", "Zhejiang", "Guangxi", "Guangdong", "Fujian", "Hainan"),
  stringsAsFactors = FALSE)


# Plot geo_facet line plots
MMR_prv  %>% 
  ggplot()+
  geom_line(data = MMR_sub, aes(x = m_name_or, y = month_mean, group = geo_code, color = prv_true), size = 0.5, alpha = 0.75, linetype = "solid")+
  geom_line(aes(x = m_name_or, y = month_mean, group = geo_code , color = prv_true), size = 1) +
  scale_x_discrete(breaks = c("Oct", "Jan", "Apr", "Jul"))+
  scale_color_manual(values =  c("Province" = "#fc8d62", "Prefecture" = "#8da0cb", "County" = "#a6d854"), breaks = c("Province", "Prefecture", "County"), na.value = "grey90") +
  facet_geo(~name, grid=CHN_GF) +
  labs(y = "MMR influenza test positivity (%)", x = "") +
  coord_cartesian(ylim = c(0.0, 35), expand=F)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_blank(),
        axis.text.y = element_text(size = 6, hjust = 0),
        axis.text.x = element_text(size = 6, angle = 90, hjust =0),
        panel.border = element_rect(fill = NA, color = "grey50"),
        panel.background = element_rect(fill = NA),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 6),
        strip.background = element_rect(color = "grey50", fill = "grey95") )


ggsave("output/Sup_fig_S14.png",
       width = 210,
       height = 210,
       dpi = 320,
       units = "mm")


###

# Calculation for same peaks 

# List of prv codes
prv_no <- bind_rows(MMR_prv,MMR_sub) %>% 
  pull(prv) %>% 
  unique()

# Function
same_peak <- function(x){
  
  prv_month <- bind_rows(MMR_prv,MMR_sub) %>% 
    filter(prv == prv_no[x]) %>%
    group_by(geo_code) %>% 
    mutate(peak = max(month_mean)) %>% 
    mutate(peak_month = if_else(month_mean == peak, "max", "no")) %>% 
    filter(peak_month == "max") %>% 
    filter(prf == "00") %>% 
    pull(month)
  
  prv_month_plus <- tibble(mon =c(prv_month, prv_month+1, prv_month-1)) %>% 
    mutate(mon = if_else(mon == 0, 12, mon)) %>% 
    mutate(mon = if_else(mon == 13, 1, mon)) %>% 
    pull()
  
  sub_month <- bind_rows(MMR_prv,MMR_sub) %>% 
    filter(prv == prv_no[x]) %>%
    group_by(geo_code) %>% 
    mutate(peak = max(month_mean)) %>% 
    mutate(peak_month = if_else(month_mean == peak, "max", "no")) %>% 
    filter(peak_month == "max") %>% 
    filter(prf != "00") %>% 
    pull(month)
  
  tibble(prv = prv_no[x], 
         prf_no = length(sub_month),
         prf_same = sum(sub_month == prv_month),
         prf_plus = sum(is.element(sub_month, prv_month_plus)),
         same_per = prf_same/prf_no,
         plus_per = prf_plus/prf_no)
}

# Bind all provinces
same_peak_tib <- map(1:length(prv_no), same_peak) %>% 
  bind_rows()


# Province based percentages - same month 
same_peak_tib %>% 
  summarise(same_mean = mean(same_per, na.rm = TRUE),
            sd = sd(same_per, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = same_mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = same_mean + qt(1 - (0.05 / 2), n - 1) * se)

# Province based percentages - +- 1 month 
same_peak_tib %>% 
  summarise(plus_mean = mean(plus_per, na.rm = TRUE),
            sd = sd(plus_per, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower_ci = plus_mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper_ci = plus_mean + qt(1 - (0.05 / 2), n - 1) * se)



