### Seasonal decomposition results

# Produces results for:

# Figure S19 

# Read in data and useful functions
source("script/data_and_functions.R")

# Figure S18 - Heatmap of province level seasonal decomposition influenza test positivity

TPO<- monthly_long_function("Positive rate", "All", "ILI outpatient")

TPO_uni <- TPO %>% 
  mutate(measure = "TPO") %>% 
  group_by(measure, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% 
  ungroup() %>% 
  group_by(geo_code) %>% 
  mutate(n_month = length(time)) %>% 
  filter(n_month >= 24) %>% 
  ungroup() %>% 
  left_join(., geo_name, by = c("geo_code" = "code"))

regions <- TPO_uni %>% 
  distinct(geo_code) %>% 
  pull()

sea_decom <- function(i){
  
  min_year <- TPO_uni %>% 
    filter(geo_code == regions[[i]]) %>% 
    arrange(time) %>% 
    mutate(year = year(date)) %>% 
    pull(year) %>% 
    first()
  
  min_month <- TPO_uni %>% 
    filter(geo_code == regions[[i]]) %>% 
    arrange(time) %>% 
    mutate(month = month(date)) %>% 
    pull(month) %>% 
    first()
  
  date <- TPO_uni %>% 
    filter(geo_code == regions[[i]]) %>% 
    arrange(time) %>% 
    pull(date) 
  
  decomp <- TPO_uni %>% 
    filter(geo_code == regions[[i]]) %>% 
    select(mean) %>% 
    ts(., frequency = 12, start = c(min_year, min_month)) %>% 
    decompose()
  
  tibble(date = date,
         geo_code = regions[[i]],
         obs = as.double(decomp$x),
         trend = as.double(decomp$trend), 
         seasonal = as.double(decomp$seasonal), 
         random = as.double(decomp$random)) %>% 
    left_join(., geo_name, by = c("geo_code" = "code"))
  
}

sea_decom(1)

decomp_all <- map(1:length(regions), sea_decom) %>% 
  bind_rows() %>% 
  mutate(time = yearmonth(date)) %>% 
  left_join(., shp_code, by = c("geo_code" = "code")) %>% 
  mutate(PYNAME = factor(PYNAME, levels = geo_lat))


##################

### Observation
obs <- decomp_all %>% 
  filter(prf == "00") %>% 
  ggplot(., aes(x = date, y = PYNAME)) +
  geom_tile(aes(fill = obs*100), width =35)+
  geom_vline(xintercept = as.Date("2005/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2006/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2007/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2008/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2009/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2010/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2011/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2012/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2013/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2014/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2015/10/01"), linetype = "dashed")+  
  geom_vline(xintercept = as.Date("2016/10/01"), linetype = "dashed")+ 
  geom_vline(xintercept = as.Date("2017/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2018/10/01"), linetype = "dashed")+
  scale_fill_continuous_sequential(name="Observed\nrate", 
                                   palette = "Oranges", 
                                   rev = T,
                                   breaks= c(20, 40, 60),
                                   na.value = "grey90",
                                   guide=guide_colorsteps(show.limits=FALSE, barwidth=2, title.position="top", frame.colour = "black"))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(y = "",x = "") +
  coord_cartesian(xlim = c(as.Date("2005-08-01"),as.Date("2015-12-01")),expand=F) +
  theme_minimal()+
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(size = 8, hjust = 0),
        panel.background = element_rect(fill ="grey90") ,
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_markdown(size = 6, hjust = 0),
        axis.text.x = element_text(size = 8, vjust = 0, hjust =0.5),
        axis.title = element_blank(),
        plot.margin = margin(2,0,2,8, "mm"))


### Trend
trend <- decomp_all %>% 
  filter(prf == "00") %>% 
  ggplot(., aes(x = date, y = PYNAME)) +
  geom_tile(aes(fill = trend*100), width =35)+
  geom_vline(xintercept = as.Date("2005/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2006/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2007/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2008/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2009/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2010/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2011/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2012/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2013/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2014/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2015/10/01"), linetype = "dashed")+  
  geom_vline(xintercept = as.Date("2016/10/01"), linetype = "dashed")+ 
  geom_vline(xintercept = as.Date("2017/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2018/10/01"), linetype = "dashed")+
  scale_fill_continuous_sequential(name="Long-term\ntrend", 
                                   palette = "Blues 3", 
                                   rev = T,
                                   breaks= c( 10, 15, 20),
                                   na.value = "grey90",
                                   guide=guide_colorsteps(show.limits=FALSE, barwidth=2, title.position="top", frame.colour = "black"))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(y = "",x = "") +
  coord_cartesian(xlim = c(as.Date("2005-08-01"),as.Date("2015-12-01")),expand=F) +
  theme_minimal()+
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(size = 8, hjust = 0),
        panel.background = element_rect(fill ="grey90") ,
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_markdown(size = 6, hjust = 0),
        axis.text.x = element_text(size = 8, vjust = 0, hjust =0.5),
        axis.title = element_blank(),
        plot.margin = margin(2,0,2,8, "mm"))


## Seasonal effect
seasonal <- decomp_all %>% 
  filter(prf == "00") %>% 
  ggplot(., aes(x = date, y = PYNAME)) +
  geom_tile(aes(fill = seasonal*100), width =35)+
  geom_vline(xintercept = as.Date("2005/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2006/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2007/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2008/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2009/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2010/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2011/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2012/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2013/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2014/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2015/10/01"), linetype = "dashed")+  
  geom_vline(xintercept = as.Date("2016/10/01"), linetype = "dashed")+ 
  geom_vline(xintercept = as.Date("2017/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2018/10/01"), linetype = "dashed")+
  scale_fill_continuous_diverging(name="Seasonal\neffect", 
                                  palette = "blue-red3", 
                                  mid= 0, 
                                  breaks= c(-10, -5, 0, 5, 10), 
                                  guide=guide_colorsteps(show.limits=FALSE, barwidth=2, title.position="top", frame.colour = "black"))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(y = "",x = "") +
  coord_cartesian(xlim = c(as.Date("2005-08-01"),as.Date("2015-12-01")),expand=F) +
  theme_minimal()+
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(size = 8, hjust = 0),
        panel.background = element_rect(fill ="grey90") ,
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_markdown(size = 6, hjust = 0),
        axis.text.x = element_text(size = 8, vjust = 0, hjust =0.5),
        axis.title = element_blank(),
        plot.margin = margin(2,0,2,8, "mm"))


### Random 
random <- decomp_all %>% 
  filter(prf == "00") %>% 
  ggplot(., aes(x = date, y = PYNAME)) +
  geom_tile(aes(fill = random*100), width =35)+
  geom_vline(xintercept = as.Date("2005/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2006/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2007/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2008/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2009/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2010/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2011/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2012/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2013/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2014/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2015/10/01"), linetype = "dashed")+  
  geom_vline(xintercept = as.Date("2016/10/01"), linetype = "dashed")+ 
  geom_vline(xintercept = as.Date("2017/10/01"), linetype = "dashed")+
  geom_vline(xintercept = as.Date("2018/10/01"), linetype = "dashed")+
  scale_fill_continuous_diverging(name="Random\nerror", 
                                  palette = "Purple-green", 
                                  mid= 0, 
                                  breaks= c(-20, -10, 0, 10, 20),
                                  na.value = "grey90",
                                  guide=guide_colorsteps(show.limits=FALSE, barwidth=2, title.position="top", frame.colour = "black"))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  labs(y = "",x = "") +
  coord_cartesian(xlim = c(as.Date("2005-08-01"),as.Date("2015-12-01")),expand=F) +
  theme_minimal()+
  theme(legend.position = "right",
        legend.box = "vertical",
        legend.title = element_text(size = 8, hjust = 0),
        panel.background = element_rect(fill ="grey90") ,
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_markdown(size = 6, hjust = 0),
        axis.text.x = element_text(size = 8, vjust = 0, hjust =0.5),
        axis.title = element_blank(),
        plot.margin = margin(2,0,2,8, "mm"))


# Plot combined
plot_grid(obs, trend, seasonal, random, ncol = 1,  align = "v", labels = c("A", "B", "C", "D"))


ggsave("output/Sup_fig_S19.png",
       width = 200,
       height = 250,
       dpi = 320,
       units = "mm")

