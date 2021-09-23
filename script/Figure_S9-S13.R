### MMR all outcome, all admin regions

# Produces results for:

# Figure S9 - S13 and MMR results

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
MMR_data <- all_data %>%
  group_by(measure, geo_code, time) %>% 
  summarise(mean = mean(x), date = max(t)) %>% # mean of all studies 
  ungroup() %>% 
  mutate(season = if_else(date >= as.Date("2009-10-01") & date <= as.Date("2010-09-30"), "09/10", "non-pand")) %>% # Pandemic or non-pandemic 
  filter(season != "09/10") %>% # Exclude pandemic year
  mutate(pp = if_else(date <= as.Date("2009-09-30"), "Pre-pandemic", "Post-pandemic")) %>% # Pre or post pandemic
  mutate(pp = factor(pp, levels = c("Pre-pandemic", "Post-pandemic"))) %>% 
  mutate(month = month(time)) %>% 
  group_by(measure, geo_code, pp, month) %>% 
  summarise(month_mean = mean(mean, na.rm = TRUE), # MMR  
            n = n()) %>%  
  left_join(., geo_name, c("geo_code"="code")) %>% 
  filter(n >= 2) %>% # only prv with >2 years data
  left_join(., long_name, c("geo_code"="code")) %>% 
  mutate(lat_order = factor(long_name, levels = group_lat_name)) %>% 
  mutate(date = ymd(200101)+ months(month-1)) %>% 
  mutate(m_name = as.character(month(date, label = TRUE, abbr = TRUE))) %>% 
  mutate(m_name_or = factor(m_name, levels = c( "Oct","Nov", "Dec",
                                                "Jan", "Feb","Mar", 
                                                "Apr", "May", "Jun",
                                                "Jul", "Aug", "Sep"))) 



# Factor order with text colour 
lev <- MMR_data %>% 
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

###TPO compare pre vs. post pandemic 
MMR_data %>% 
  filter(measure == "TPO") %>% 
  mutate(measure = "All influenza strains (ILI outpatient)") %>% 
  filter(prf == "00") %>% 
  arrange(lat_order) %>% 
  mutate(lat_order = as.character(lat_order)) %>% 
  mutate(x_label = paste("<span style = 'color: ", 
                         ifelse(prf == "00", "black", "grey50"),
                         ";'>", lat_order, "</span>", sep = "")) %>% 
  mutate(x_label= factor(x_label, levels =lev)) %>% 
  
  ggplot(., aes(x = m_name_or, y = x_label)) +
  geom_tile(aes(fill = month_mean*100)) +
  facet_grid(cols =  vars(pp)) +
  #facet_wrap(~measure, ncol = 1) +
  scale_fill_continuous_sequential(name="Infleunza test positivity (%)", 
                                   palette = "Oranges", 
                                   breaks= c(10, 20, 30, 40, 50), 
                                   guide=guide_colorsteps(show.limits=FALSE, barheight= 0.75, barwidth=12, title.position="top", frame.colour = "black"))+
  labs(y = "",x = "") +
  coord_cartesian(expand=F)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        panel.background = element_rect(fill ="grey90"),
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_markdown(size = 6, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust =0),
        axis.title = element_blank())

ggsave("output/Sup_fig_S9.png",
       width = 180,
       height = 220,
       dpi = 320,
       units = "mm")


### ILI (post-pandemic only)

MMR_data %>% 
  filter(measure == "ILI") %>% 
  mutate(measure = "ILI consultation rate") %>% 
  filter(pp == "Post-pandemic") %>% # filter to post-pandemic 
  arrange(lat_order) %>% 
  mutate(lat_order = as.character(lat_order)) %>% 
  mutate(x_label = paste("<span style = 'color: ", 
                         ifelse(prf == "00", "black", "grey50"),
                         ";'>", lat_order, "</span>", sep = "")) %>% 
  mutate(x_label= factor(x_label, levels =lev)) %>% 
  
  ggplot(., aes(x = m_name_or, y = x_label)) +
  geom_tile(aes(fill = month_mean *100)) +
  facet_wrap(~measure, ncol = 1) +
  scale_fill_continuous_sequential(name="ILI consultation rate (%)", 
                                   palette = "Oranges", 
                                   breaks= c(2, 4, 6, 8, 10), 
                                   guide=guide_colorsteps(show.limits=FALSE, barheight= 0.75, barwidth=12, title.position="top", frame.colour = "black"))+
  labs(y = "",x = "") +
  coord_cartesian(expand=F)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_markdown(size = 6, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust =0),
        axis.title = element_blank())


ggsave("output/Sup_fig_S10.png",
       width = 140,
       height = 220,
       dpi = 320,
       units = "mm")


###TPO
MMR_data %>% 
  filter(measure == "TPO") %>% 
  mutate(measure = "All influenza strains (ILI outpatient)") %>% 
  filter(pp == "Post-pandemic") %>% # filter to post-pandemic 
  arrange(lat_order) %>% 
  mutate(lat_order = as.character(lat_order)) %>% 
  mutate(x_label = paste("<span style = 'color: ", 
                         ifelse(prf == "00", "black", "grey50"),
                         ";'>", lat_order, "</span>", sep = "")) %>% 
  mutate(x_label= factor(x_label, levels =lev)) %>% 
  
  ggplot(., aes(x = m_name_or, y = x_label)) +
  geom_tile(aes(fill = month_mean*100)) +
  facet_wrap(~measure, ncol = 1) +
  scale_fill_continuous_sequential(name="Infleunza test positivity (%)", 
                                   palette = "Oranges", 
                                   breaks= c(10, 20, 30, 40, 50), 
                                   guide=guide_colorsteps(show.limits=FALSE, barheight= 0.75, barwidth=12, title.position="top", frame.colour = "black"))+
  labs(y = "",x = "") +
  coord_cartesian(expand=F)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        panel.background = element_rect(fill ="grey90"),
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(8, "mm"),
        axis.text.y = element_markdown(size = 6, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust =0),
        axis.title = element_blank())

ggsave("output/Sup_fig_S11.png",
       width = 140,
       height = 230,
       dpi = 320,
       units = "mm")



###TPO_strain
MMR_data %>% 
  filter(measure == "TPO_H3" |measure == "TPO_H1" | measure == "TPO_B") %>% 
  mutate(measure = if_else(measure == "TPO_H3", "A/H3N2", measure)) %>%
  mutate(measure = if_else(measure == "TPO_H1", "A/H1N1pdm09", measure)) %>%
  mutate(measure = if_else(measure == "TPO_B", "B", measure)) %>%
  mutate(measure = factor(measure, levels = c("A/H3N2", "A/H1N1pdm09", "B"))) %>% 
  filter(pp == "Post-pandemic") %>% # filter to post-pandemic 
  arrange(lat_order) %>% 
  mutate(lat_order = as.character(lat_order)) %>% 
  mutate(x_label = paste("<span style = 'color: ", 
                         ifelse(prf == "00", "black", "grey50"),
                         ";'>", lat_order, "</span>", sep = "")) %>% 
  mutate(x_label= factor(x_label, levels =lev)) %>% 
  
  ggplot(., aes(x = m_name_or, y = x_label), width = 5, height = 20) +
  geom_tile(aes(fill = month_mean*100)) +
  facet_wrap(~measure, ncol = 1, scales = "free") +
  scale_fill_continuous_sequential(name="Infleunza test positivity (%)", 
                                   palette = "Oranges", 
                                   breaks= c(10, 20, 30, 40, 50), 
                                   guide=guide_colorsteps(show.limits=FALSE, barheight= 0.75, barwidth=12, title.position="top", frame.colour = "black"))+
  labs(y = "",x = "") +
  coord_cartesian(expand=F)+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        panel.border = element_rect(fill = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        panel.spacing = unit(0, "mm"),
        axis.text.y = element_markdown(size = 4, hjust = 0),
        axis.text.x = element_text(angle = 90, vjust = 0.4, hjust =0, size = 6),
        axis.title = element_blank())

ggsave("output/Sup_fig_S12.png",
       width = 140,
       height = 230,
       dpi = 320,
       units = "mm")

# Other outcome plot

plot_rest_MMR <- function(){
  
  TPI_plot <- MMR_data %>% 
    filter(measure == "TPI") %>%
    mutate(measure = "All influenza strains (SARI inpatient)") %>% 
    arrange(lat_order) %>% 
    mutate(lat_order = as.character(lat_order)) %>% 
    mutate(x_label = paste("<span style = 'color: ", 
                           ifelse(prf == "00", "black", "grey50"),
                           ";'>", lat_order, "</span>", sep = "")) %>% 
    mutate(x_label= factor(x_label, levels =lev)) %>% 
    
    ggplot(., aes(x = m_name_or, y = x_label)) +
    geom_tile(aes(fill = month_mean*100)) +
    facet_wrap(~measure, ncol = 1) +
    scale_fill_continuous_sequential(name="Infleunza test positivity (%)", 
                                     palette = "Oranges", 
                                     breaks= c(10, 20, 30, 40, 50), 
                                     guide=guide_colorsteps(show.limits=FALSE, barwidth=2, title.position="top", frame.colour = "black"))+
    labs(y = "",x = "") +
    coord_cartesian(expand=F)+
    #coord_fixed(ratio = 2.5, expand = FALSE)+
    theme_minimal()+
    theme(legend.position = "right",
          legend.box = "vertical",
          legend.title = element_text(size = 8, hjust = 0),
          panel.background = element_rect(fill ="grey90"),
          panel.border = element_rect(fill = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black"),
          panel.spacing = unit(8, "mm"),
          axis.text.y = element_markdown(size = 8, hjust = 0),
          axis.text.x = element_text(angle = 90, vjust = 0.4, hjust =0),
          axis.title = element_blank(),
          plot.margin = unit(c(0.25, 0, 0.25, 0), "cm"))
  
  
  MR_plot <- MMR_data %>% 
    filter(measure == "MR") %>% 
    mutate(measure = "Influenza associated respiratory mortality") %>% 
    arrange(lat_order) %>% 
    mutate(lat_order = as.character(lat_order)) %>% 
    mutate(x_label = paste("<span style = 'color: ", 
                           ifelse(prf == "00", "black", "grey50"),
                           ";'>", lat_order, "</span>", sep = "")) %>% 
    mutate(x_label= factor(x_label, levels =lev)) %>% 
    
    ggplot(., aes(x = m_name_or, y = x_label)) +
    geom_tile(aes(fill = month_mean)) +
    facet_wrap(~measure, ncol = 1) +
    scale_fill_continuous_sequential(name="Mortality rate \n(per 100 000 person-years)", 
                                     palette = "Oranges", 
                                     breaks= c(0.2, 0.4, 0.6, 0.8, 1), 
                                     guide=guide_colorsteps(show.limits=FALSE, barwidth=2, title.position="top", frame.colour = "black"))+
    labs(y = "",x = "") +
    coord_cartesian(expand=F)+
    #coord_fixed(ratio = 2.5, expand = FALSE)+
    theme_minimal()+
    theme(legend.position = "right",
          legend.box = "vertical",
          legend.title = element_text(size = 8, hjust = 0),
          panel.background = element_rect(fill ="grey90"),
          panel.border = element_rect(fill = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black"),
          panel.spacing = unit(8, "mm"),
          axis.text.y = element_markdown(size = 8, hjust = 0),
          axis.text.x = element_text(angle = 90, vjust = 0.4, hjust =0),
          axis.title = element_blank(),
          plot.margin = unit(c(0.25, 0, 0.25, 0), "cm"))
  
  MAC_plot <- MMR_data %>% 
    filter(measure == "MAC") %>%
    mutate(measure = "Influenza associated all-cause mortality") %>% 
    arrange(lat_order) %>% 
    mutate(lat_order = as.character(lat_order)) %>% 
    mutate(x_label = paste("<span style = 'color: ", 
                           ifelse(prf == "00", "black", "grey50"),
                           ";'>", lat_order, "</span>", sep = "")) %>% 
    mutate(x_label= factor(x_label, levels =lev)) %>% 
    
    ggplot(., aes(x = m_name_or, y = x_label)) +
    geom_tile(aes(fill = month_mean)) +
    facet_wrap(~measure, ncol = 1) +
    scale_fill_continuous_sequential(name="Mortality rate \n(per 100 000 person-years)", 
                                     palette = "Oranges", 
                                     breaks= c(0.5, 1, 1.5, 2, 2.5), 
                                     guide=guide_colorsteps(show.limits=FALSE, barwidth=2, title.position="top", frame.colour = "black"))+
    labs(y = "",x = "") +
    coord_cartesian(expand=F)+
    #coord_fixed(ratio = 2.5, expand = FALSE)+
    theme_minimal()+
    theme(legend.position = "right",
          legend.box = "vertical",
          legend.title = element_text(size = 8, hjust = 0),
          panel.background = element_rect(fill ="grey90"),
          panel.border = element_rect(fill = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black"),
          panel.spacing = unit(8, "mm"),
          axis.text.y = element_markdown(size = 8, hjust = 0),
          axis.text.x = element_text(angle = 90, vjust = 0.4, hjust =0),
          axis.title = element_blank(),
          plot.margin = unit(c(0.25, 0, 0.25, 0), "cm"))
  
  
  plot_grid(TPI_plot, MR_plot, MAC_plot, ncol = 1, align = "v", rel_heights = c(12.5,10,7.5), labels = c("A", "B", "C"))
  
}

plot_rest_MMR()

ggsave("output/Sup_fig_S13.png",
       width = 140,
       height = 160,
       dpi = 320,
       units = "mm")


