### All reconstructed time-series for all admin regions

# Produces results for:

# Figure S5 - S8 and MMR results

# Read in data and useful functions
source("script/data_and_functions.R")


# Nested lat order
# Nested prv latitude sort - extract codes 
group_lat <- geo_name %>% 
  mutate(prv_name = factor(prv_name, levels = geo_lat)) %>% 
  group_by(prv_name) %>% 
  mutate(prf_name = factor(prf_name, levels = geo_lat)) %>% 
  arrange(prv_name) %>% 
  pull(code) %>% 
  unique()

prv_1 <- geo_name %>% 
  mutate(prv_name = factor(prv_name, levels = geo_lat)) %>% 
  filter(prf == "00") %>% 
  arrange(prv_name) %>% 
  pull(prv) %>% 
  as.character()

group_lat <- map(1:length(prv_1), function(x){geo_name %>% 
    filter(prv == prv_1[x]) %>% 
    arrange(desc(prf, cty))}) %>% 
  bind_rows() %>% 
  pull(code)

# Nested prv latitude sort - extract names 
long_name <- geo_name %>% 
  mutate(prv_short = if_else(str_count(prv_name, '\\w+') == 1,  word(prv_name, 1), word(prv_name, 1,2))) %>% 
  mutate(prf_short = word(prf_name,1)) %>% 
  mutate(cty_short = word(cty_name,1)) %>% 
  mutate(long_name = paste(prv_short, if_else(is.na(prf_short), "", prf_short))) %>%
  mutate(long_name = paste(long_name, if_else(is.na(cty_short), "", cty_short))) %>% 
  select(code, long_name)

group_lat_name <- geo_name %>% 
  mutate(prv_short = if_else(str_count(prv_name, '\\w+') == 1,  word(prv_name, 1), word(prv_name, 1,2))) %>% 
  mutate(prf_short = word(prf_name,1)) %>% 
  mutate(cty_short = word(cty_name,1)) %>% 
  mutate(long_name = paste(prv_short, if_else(is.na(prf_short), "", prf_short))) %>%
  mutate(long_name = paste(long_name, if_else(is.na(cty_short), "", cty_short)))  %>% 
  mutate(code_lat = factor(code, levels = group_lat)) %>%
  arrange(code_lat) %>% 
  pull(long_name) %>% 
  unique() %>% 
  as.vector()


###
ILI <- monthly_long_function("ILI %", "N/A", "outpatient") %>% 
  mutate(measure = "ILI")

TPO<- monthly_long_function("Positive rate", "All", "ILI outpatient") %>% 
  mutate(measure = "TPO")

TPO_H3<- monthly_long_function("Positive rate", "A/H3N2", "ILI outpatient") %>% 
  mutate(measure = "TPO_H3")

TPO_H1<- monthly_long_function("Positive rate", "A/H1N1pdm09", "ILI outpatient") %>% 
  mutate(measure = "TPO_H1")

TPO_B<- monthly_long_function("Positive rate", "B", "ILI outpatient") %>% 
  mutate(measure = "TPO_B")

TPI<- monthly_long_function("Positive rate", "All", "SARI") %>% 
  mutate(measure = "TPI")

MR<- monthly_long_function("Mort_flu", "N/A", "mortality_resp") %>% 
  mutate(measure = "MR")

MAC<- monthly_long_function("Mort_flu", "N/A", "mortality_all") %>% 
  mutate(measure = "MAC")

all_data <- bind_rows(ILI, TPO, TPO_H3, TPO_H1, TPO_B, TPI, MR, MAC)


# Data manipulation 
data <- all_data %>%
  mutate(year_month = yearmonth(t)) %>% 
  group_by(measure, geo_code, year_month) %>% 
  summarise(value = mean(x), date = max(t)) %>% 
  ungroup() %>% 
  mutate(prv = substr(geo_code, 1,2)) %>% 
  mutate(prf = substr(geo_code, 3,4)) %>% 
  mutate(cty = substr(geo_code, 5,6)) %>% 
  left_join(., long_name, c("geo_code"="code")) %>% 
  mutate(lat_order = factor(long_name, levels = group_lat_name))


# Factor order with text colour 
lev <- data %>% 
  arrange(lat_order) %>% 
  mutate(lat_order = as.character(lat_order)) %>% 
  mutate(x_label = paste("<span style = 'color: ", 
                         ifelse(prf == "00", "black", "grey50"),
                         ";'>", lat_order, "</span>", sep = "")) %>% 
  mutate(geo_code = factor(geo_code, levels = group_lat) ) %>% 
  arrange(geo_code) %>% 
  pull(x_label) %>% 
  unique()


# Plot figure 
### ILI

data %>% 
  filter(measure == "ILI") %>% 
  mutate(measure = "ILI consultation rate") %>% 
  arrange(lat_order) %>% 
  mutate(lat_order = as.character(lat_order)) %>% 
  mutate(x_label = paste("<span style = 'color: ", 
                         ifelse(prf == "00", "black", "grey50"),
                         ";'>", lat_order, "</span>", sep = "")) %>% 
  mutate(x_label= factor(x_label, levels =lev)) %>% 
  ggplot() +
  geom_tile(aes(x = year_month, y = x_label, fill = value*100), width = 35, height = 1) +
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
  facet_wrap(~measure, ncol = 1) +
  scale_fill_continuous_sequential(name="ILI consultation rate (%)", 
                                   palette = "Oranges", 
                                   breaks= c( 5, 10, 15, 20, 25),
                                   guide=guide_colorsteps(show.limits=FALSE, barheight= 0.75, barwidth=12, title.position="top", frame.colour = "black"))+
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")+
  
  coord_cartesian(expand=F, xlim = c(as.Date("2005-07-01"), as.Date("2019-02-01")))+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.background = element_rect(fill ="grey90") ,
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_markdown(size = 6, hjust = 0),
        axis.text.x = element_text(size = 8, vjust = 0, hjust =0.5),
        axis.title = element_blank())

ggsave("output/Sup_fig_S5.png",
       width = 180,
       height = 220,
       dpi = 320,
       units = "mm")

# TPO
data %>% 
  filter(measure == "TPO") %>% 
  mutate(measure = "All influenza strains (ILI outpatient)") %>% 
  arrange(lat_order) %>% 
  mutate(lat_order = as.character(lat_order)) %>% 
  mutate(x_label = paste("<span style = 'color: ", 
                         ifelse(prf == "00", "black", "grey50"),
                         ";'>", lat_order, "</span>", sep = "")) %>% 
  mutate(x_label= factor(x_label, levels =lev)) %>% 
  ggplot() +
  geom_tile(aes(x = year_month, y = x_label, fill = value*100), width = 35, height = 1) +
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
  facet_wrap(~measure, ncol = 1) +
  scale_fill_continuous_sequential(name="Influenza test positivity (%)", 
                                   palette = "Oranges", 
                                   breaks= c(20, 40, 60, 80), 
                                   guide=guide_colorsteps(show.limits=FALSE, barheight= 0.75, barwidth=12, title.position="top", frame.colour = "black"))+
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")+
  
  coord_cartesian(expand=F, xlim = c(as.Date("2005-07-01"), as.Date("2019-02-01")))+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.background = element_rect(fill ="grey90") ,
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_markdown(size = 5.5, hjust = 0),
        axis.text.x = element_text(size = 8, vjust = 0, hjust =0.5),
        axis.title = element_blank())

ggsave("output/Sup_fig_S6.png",
       width = 180,
       height = 230,
       dpi = 320,
       units = "mm")

#####TPO_strain
data %>% 
  filter(measure == "TPO_H3" |measure == "TPO_H1" | measure == "TPO_B") %>% 
  mutate(measure = if_else(measure == "TPO_H3", "A/H3N2", measure)) %>%
  mutate(measure = if_else(measure == "TPO_H1", "A/H1N1pdm09", measure)) %>%
  mutate(measure = if_else(measure == "TPO_B", "B", measure)) %>%
  mutate(measure = factor(measure, levels = c("A/H3N2", "A/H1N1pdm09", "B"))) %>% 
  arrange(lat_order) %>% 
  mutate(lat_order = as.character(lat_order)) %>% 
  mutate(x_label = paste("<span style = 'color: ", 
                         ifelse(prf == "00", "black", "grey50"),
                         ";'>", lat_order, "</span>", sep = "")) %>% 
  mutate(x_label= factor(x_label, levels =lev)) %>% 
  ggplot() +
  geom_tile(aes(x = year_month, y = x_label, fill = value*100), width = 35, height = 1) +
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
  facet_wrap(~measure, ncol = 1, scales = "free") +
  scale_fill_continuous_sequential(name= "Influenza test positivity (%)", 
                                   palette = "Oranges", 
                                   breaks= c( 20, 40, 60, 80), 
                                   guide= guide_colorsteps(show.limits=FALSE, barheight =0.75, barwidth=12, title.position="top", frame.colour = "black"))+
  scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")+
  
  coord_cartesian(expand=F, xlim = c(as.Date("2006-07-01"), as.Date("2018-07-01")))+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        panel.background = element_rect(fill ="grey90") ,
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(0, "mm"),
        axis.text.y = element_markdown(size = 4, hjust = 0),
        axis.text.x = element_text(size = 6, vjust = 0, hjust =0.5),
        axis.title = element_blank())

ggsave("output/Sup_fig_S7.png",
       width = 180,
       height = 240,
       dpi = 320,
       units = "mm")



# Other outcome plot

plot_rest_MMR <- function(){
  
  TPI_plot <- data %>% 
    filter(measure == "TPI") %>%
    mutate(measure = "All influenza strains (SARI inpatient)") %>% 
    arrange(lat_order) %>% 
    mutate(lat_order = as.character(lat_order)) %>% 
    mutate(x_label = paste("<span style = 'color: ", 
                           ifelse(prf == "00", "black", "grey50"),
                           ";'>", lat_order, "</span>", sep = "")) %>% 
    mutate(x_label= factor(x_label, levels =lev)) %>% 
    
    ggplot() +
    geom_tile(aes(x = year_month, y = x_label, fill = value*100), width = 35, height = 1) +
    geom_vline(xintercept = as.Date("2010/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2011/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2012/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2013/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2014/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2015/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2016/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2017/10/01"), linetype = "dashed")+
    facet_wrap(~measure, ncol = 1) +
    scale_fill_continuous_sequential(name="Influenza test positivity (%)", 
                                     palette = "Oranges", 
                                     breaks= c(10, 20, 30, 40, 50), 
                                     guide=guide_colorsteps(show.limits=FALSE, barwidth=2, title.position="top", frame.colour = "black"))+
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")+
    labs(y = "",x = "") +
    coord_fixed(ratio = 300, expand = FALSE)+
    theme_minimal()+
    theme(legend.position = "right",
          legend.box = "horizontal",
          legend.title = element_text(size = 8),
          panel.background = element_rect(fill ="grey90") ,
          panel.border = element_rect(fill = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black"),
          panel.spacing = unit(8, "mm"),
          axis.text.y = element_markdown(size = 6, hjust = 0),
          axis.text.x = element_text(size = 8, vjust = 0, hjust =0.5),
          axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  
  MR_plot <- data %>% 
    filter(measure == "MR") %>% 
    mutate(measure = "Influenza associated respiratory mortality") %>% 
    arrange(lat_order) %>% 
    mutate(lat_order = as.character(lat_order)) %>% 
    mutate(x_label = paste("<span style = 'color: ", 
                           ifelse(prf == "00", "black", "grey50"),
                           ";'>", lat_order, "</span>", sep = "")) %>% 
    mutate(x_label= factor(x_label, levels =lev)) %>% 
    
    ggplot() +
    geom_tile(aes(x = year_month, y = x_label, fill = value), width = 35, height = 1) +
    geom_vline(xintercept = as.Date("2010/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2011/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2012/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2013/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2014/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2015/10/01"), linetype = "dashed")+
    facet_wrap(~measure, ncol = 1) +
    scale_fill_continuous_sequential(name="Mortality rate \n(per 100 000 person-years)", 
                                     palette = "Oranges", 
                                     breaks= c(0.5, 1, 1.5, 2, 2.5), 
                                     guide=guide_colorsteps(show.limits=FALSE, barwidth=2, title.position="top", frame.colour = "black"))+
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")+
    labs(y = "",x = "") +
    coord_fixed(ratio = 300, expand = FALSE)+
    theme_minimal()+
    theme(legend.position = "right",
          legend.box = "horizontal",
          legend.title = element_text(size = 8),
          panel.background = element_rect(fill ="grey90") ,
          panel.border = element_rect(fill = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black"),
          panel.spacing = unit(8, "mm"),
          axis.text.y = element_markdown(size = 6, hjust = 0),
          axis.text.x = element_text(size = 8, vjust = 0, hjust =0.5),
          axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  MAC_plot <- data %>% 
    filter(measure == "MAC") %>%
    mutate(measure = "Influenza associated all-cause mortality") %>% 
    arrange(lat_order) %>% 
    mutate(lat_order = as.character(lat_order)) %>% 
    mutate(x_label = paste("<span style = 'color: ", 
                           ifelse(prf == "00", "black", "grey50"),
                           ";'>", lat_order, "</span>", sep = "")) %>% 
    mutate(x_label= factor(x_label, levels =lev)) %>% 
    
    ggplot() +
    geom_tile(aes(x = year_month, y = x_label, fill = value), width = 35, height = 1) +
    geom_vline(xintercept = as.Date("2004/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2005/10/01"), linetype = "dashed")+
    geom_vline(xintercept = as.Date("2006/10/01"), linetype = "dashed")+
    facet_wrap(~measure, ncol = 1) +
    scale_fill_continuous_sequential(name="Mortality rate \n(per 100 000 person-years)", 
                                     palette = "Oranges", 
                                     breaks= c(1,2,3,4,5), 
                                     guide=guide_colorsteps(show.limits=FALSE, barwidth=2, title.position="top", frame.colour = "black"))+
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y")+
    labs(y = "",x = "") +
    coord_fixed(ratio = 250, expand = FALSE)+
    theme_minimal()+
    theme(legend.position = "right",
          legend.box = "horizontal",
          legend.title = element_text(size = 8),
          panel.background = element_rect(fill ="grey90") ,
          panel.border = element_rect(fill = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black"),
          panel.spacing = unit(8, "mm"),
          axis.text.y = element_markdown(size = 6, hjust = 0),
          axis.text.x = element_text(size = 8, vjust = 0, hjust =0.5),
          axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
  
  
  plot_grid(TPI_plot, MR_plot, MAC_plot, ncol = 1, align = "v", rel_heights = c(10,10,7.5), labels = c("A", "B", "C"))
  
}

plot_rest_MMR()

ggsave("output/Sup_fig_S8.png",
       width = 200,
       height = 180,
       dpi = 320,
       units = "mm")

