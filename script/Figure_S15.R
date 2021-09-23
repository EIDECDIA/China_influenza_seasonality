### Cross outcome MMR comparison

# Produces results for:

# Figure S15 

# Read in data and useful functions
source("script/data_and_functions.R")

# Figure S14 - Cross outcome bivariate plots

# Raw data for each outcome
ILI <- monthly_long_function("ILI %", "N/A", "outpatient")

TPO<- monthly_long_function("Positive rate", "All", "ILI outpatient")

TPI<- monthly_long_function("Positive rate", "All", "SARI")

MR<- monthly_long_function("Mort_flu", "N/A", "mortality_resp")

MAC<- monthly_long_function("Mort_flu", "N/A", "mortality_all")


# Study ILI data to MMR ILI
ILI_MMR <- ILI %>% 
  mutate(measure = "ILI") %>% 
  group_by(measure, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% 
  ungroup() %>% 
  mutate(month = month(time)) %>% 
  group_by(measure, geo_code, month) %>% 
  summarise(ILI = mean(mean, na.rm = TRUE), 
            n = n()) %>%
  ungroup() %>% 
  select(geo_code, month, ILI, n) %>% 
  filter(n >= 2) # only regions with >2 years data


# Study TPO data to MMR TPO
TPO_MMR <- TPO %>% 
  mutate(measure = "TPO") %>% 
  group_by(measure, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% 
  ungroup() %>% 
  mutate(month = month(time)) %>% 
  group_by(measure, geo_code, month) %>% 
  summarise(TPO = mean(mean, na.rm = TRUE), 
            n = n()) %>%
  ungroup() %>% 
  select(geo_code, month, TPO, n) %>% 
  filter(n >= 2) # only regions with >2 years data

# Study TPI data to MMR TPI
TPI_MMR <- TPI %>% 
  mutate(measure = "TPI") %>% 
  group_by(measure, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% 
  ungroup() %>% 
  mutate(month = month(time)) %>% 
  group_by(measure, geo_code, month) %>% 
  summarise(TPI = mean(mean, na.rm = TRUE), 
            n = n()) %>%
  ungroup() %>% 
  select(geo_code, month, TPI, n) %>% 
  filter(n >= 2) # only regions with >2 years data

# Study MR data to MMR MR
MR_MMR <- MR %>% 
  mutate(measure = "MR") %>% 
  group_by(measure, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% 
  ungroup() %>% 
  mutate(month = month(time)) %>% 
  group_by(measure, geo_code, month) %>% 
  summarise(MR = mean(mean, na.rm = TRUE), 
            n = n()) %>%
  ungroup() %>% 
  select(geo_code, month, MR, n) %>% 
  filter(n >= 2) # only regions with >2 years data

# Study MAC data to MMR MAC
MAC_MMR <- MAC %>% 
  mutate(measure = "MAC") %>% 
  group_by(measure, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% 
  ungroup() %>% 
  mutate(month = month(time)) %>% 
  group_by(measure, geo_code, month) %>% 
  summarise(MAC = mean(mean, na.rm = TRUE), 
            n = n()) %>%
  ungroup() %>% 
  select(geo_code, month, MAC, n) %>% 
  filter(n >= 2) # only regions with >2 years data


compare <- full_join(ILI_MMR, TPO_MMR, by = c("geo_code" = "geo_code", "month" = "month")) %>% 
  full_join(., TPI_MMR, by = c("geo_code" = "geo_code", "month" = "month")) %>% 
  full_join(., MR_MMR, by = c("geo_code" = "geo_code", "month" = "month")) %>% 
  full_join(., MAC_MMR, by = c("geo_code" = "geo_code", "month" = "month"))

# Grid plot

# ILI ~ TPO
summary(lm(data = compare, formula =  TPO ~ ILI))   

a<- compare %>% 
  filter(!is.na(ILI) & !is.na(TPO)) %>% 
  ggplot(aes(y = TPO, x = ILI)) +
  geom_point()+
  geom_smooth(method = "lm") +
  annotation_custom(grobTree(text_grob("y = 0.13 + 0.018x\nR^2 = 0.002", x = 0.1, y= 0.9, hjust = 0, size = 6)))+ 
  theme_cowplot()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(size = 6))

# TPI ~ ILI
summary(lm(data = compare, formula =  TPI ~ ILI))

b<- compare %>% 
  filter(!is.na(TPI) & !is.na(ILI)) %>% 
  ggplot(aes(y = TPI, x = ILI)) +
  geom_point()+
  geom_smooth(method = "lm")+
  annotate("text", x = 1, y = 1, label = "No data")+
  theme_cowplot()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_blank())

# TPI ~ TPO
summary(lm(data = compare, formula =  TPI ~ TPO))

c<- compare %>% 
  filter(!is.na(TPI) & !is.na(TPO)) %>% 
  ggplot(aes(y = TPI, x = TPO)) +
  geom_point()+
  geom_smooth(method = "lm")+
  annotation_custom(grobTree(text_grob("y = -0.006 + 0.62x\nR^2 = 0.72", x = 0.1, y= 0.9, hjust = 0, size = 6)))+ 
  theme_cowplot()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 6))

# MR ~ ILI
summary(lm(data = compare, formula =  MR ~ ILI))

d<- compare %>% 
  filter(!is.na(MR) & !is.na(ILI)) %>% 
  ggplot(aes(y = MR, x = ILI)) +
  geom_point()+
  geom_smooth(method = "lm")+
  annotation_custom(grobTree(text_grob("y = -1.43 + 35.9x\nR^2 = 0.32", x = 0.1, y= 0.9, hjust = 0, size = 6)))+ 
  theme_cowplot()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text = element_text(size = 6))


# MR ~ TPO
summary(lm(data = compare, formula =  MR ~ TPO))

e<- compare %>% 
  filter(!is.na(MR) & !is.na(TPO)) %>% 
  ggplot(aes(y = MR, x = TPO)) +
  geom_point()+
  geom_smooth(method = "lm")+
  annotation_custom(grobTree(text_grob("y = 0.005 + 1.33x\nR^2 = 0.79", x = 0.1, y= 0.9, hjust = 0, size = 6)))+ 
  theme_cowplot()+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 6))



# MR ~ TPI
summary(lm(data = compare, formula =  MR ~ TPI))

f<- compare %>% 
  filter(!is.na(MR) & !is.na(TPI)) %>% 
  ggplot(aes(y = MR, x = TPI)) +
  geom_point()+
  geom_smooth(method = "lm")+
  annotate("text", x = 1, y = 1, label = "No data")+
  theme_cowplot()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_blank())

# MAC ~ ILI
summary(lm(data = compare, formula =  MAC ~ ILI))

g<- compare %>% 
  filter(!is.na(MAC) & !is.na(ILI)) %>% 
  ggplot(aes(y = MAC, x = ILI)) +
  geom_point()+
  geom_smooth(method = "lm") +
  annotation_custom(grobTree(text_grob("y = -1.66 + 55.5x\nR^2 = 0.71", x = 0.1, y= 0.9, hjust = 0, size = 6)))+ 
  theme_cowplot()+
  theme(legend.position = "none",
        axis.text = element_text(size = 6))

# MAC ~ TPO
summary(lm(data = compare, formula =  MAC ~ TPO))

h<- compare %>% 
  filter(!is.na(MAC) & !is.na(TPO)) %>% 
  ggplot(aes(y = MAC, x = TPO)) +
  geom_point()+
  geom_smooth(method = "lm")+
  annotation_custom(grobTree(text_grob("y = -0.81 + 13.5x\nR^2 = 0.66", x = 0.1, y= 0.9, hjust = 0, size = 6)))+ 
  theme_cowplot()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text = element_text(size = 6))



# MAC ~ TPI
summary(lm(data = compare, formula =  MAC ~ TPI))

i<- compare %>% 
  filter(!is.na(MAC) & !is.na(TPI)) %>% 
  ggplot(aes(y = MAC, x = TPI)) +
  geom_point()+
  geom_smooth(method = "lm")+
  annotate("text", x = 1, y = 1, label = "No data")+
  theme_cowplot()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text = element_blank())

# MAC ~ MR
summary(lm(data = compare, formula =  MAC ~ MR))

j<- compare %>% 
  filter(!is.na(MAC) & !is.na(MR)) %>% 
  ggplot(aes(y = MAC, x = MR)) +
  geom_point()+
  geom_smooth(method = "lm")+
  annotate("text", x = 1, y = 1, label = "No data")+
  theme_cowplot()+
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text = element_blank())


grid_outcome <- plot_grid(
  a, NULL, NULL, NULL,
  b, c, NULL, NULL,
  d, e, f, NULL,
  g, h, i, j,
  ncol = 4, align = "hv")

grid_outcome

ggsave("output/Sup_fig_S15.png",
       width = 200,
       height = 200,
       dpi = 320,
       units = "mm")
