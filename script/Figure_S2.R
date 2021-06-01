### Search results 

# Produces results for:

# Figure S2

# Read in data and useful functions
source("script/data_and_functions.R")

# Figure S2 - Histogram of studies included
DES_geo %>% 
  group_by(id_study) %>% 
  filter(row_number()==1) %>% 
  select(id_study, year) %>% 
  mutate(lan = substring(id_study, 1,1)) %>% 
  mutate(lang = if_else(lan == "E", "English", "Chinese")) %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot(aes(x = year, fill =lang))+
  geom_histogram(binwidth = 1, color = "black") +
  labs(y = "No. of stuides included",x = "Year of publication") +
  scale_fill_manual(values =  c("Chinese" = "#fc8d62", "English" = "#66c2a5"), 
                    breaks = c("Chinese", "English")) +
  guides(fill = guide_legend(title = "Publication language", title.position = "top", size = 6)) +
  coord_cartesian(xlim = c(2000, 2020),ylim= c(0, 40), expand=F)+
  theme_minimal()+
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(size = 10, hjust = 0),
        legend.text = element_text(size = 8, hjust = 0),
        panel.background = element_rect(fill ="white"),
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        axis.text.y = element_text(size = 8, hjust = 0),
        axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3, hjust =0))

ggsave("output/Sup_fig_S2.png",
       width = 200,
       height = 120,
       dpi = 320,
       units = "mm")

