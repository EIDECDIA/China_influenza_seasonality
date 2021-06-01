### Combining data from seperate studies example

# Produces results for:

# Figure S1

# Read in data and useful functions
source("script/data_and_functions.R")

# Figure S1 - Beijing all strain influenza test positivity

# Set system to read Chinese characters
Sys.setlocale("LC_ALL","Chinese")

all_month_data <- monthly_long_function("Positive rate", "All", "ILI outpatient") %>% 
  filter(geo_code == "110000")

#
name_stud <- DES_geo %>% 
  group_by(id_study) %>% 
  filter(row_number()==1) %>% 
  select(id_study, title, author, journal, year) %>% 
  mutate(space = word(author, 1, sep = " ")) %>% 
  mutate(semi = word(author, 1, sep = ";")) %>% 
  mutate(comm = word(author, 1, sep = ",")) %>% 
  mutate(space_n = nchar(space)) %>% 
  mutate(semi_n = nchar(semi)) %>% 
  mutate(comm_n = nchar(comm)) %>%
  group_by(id_study) %>% 
  mutate(type = pmin(space_n, semi_n, comm_n)) %>%
  mutate(space_type = if_else(space_n == type, "space", "no")) %>% 
  mutate(semi_type = if_else(semi_n == type, "semi", "no")) %>% 
  mutate(comm_type = if_else(comm_n == type, "comm", "no"))

space <-  name_stud %>% 
  filter(space_type != "no") %>% 
  mutate(col_type = space_type) %>% 
  select(-9, -10, -11, -12, -13, -14, -15)

semi <-  name_stud %>% 
  filter(semi_type != "no") %>% 
  mutate(col_type = semi_type) %>% 
  select(-9, -10, -11, -12, -13, -14, -15)

comm <-  name_stud %>% 
  filter(comm_type != "no") %>% 
  mutate(col_type = comm_type) %>% 
  select(-9, -10, -11, -12, -13, -14, -15)

final <- bind_rows(space, semi, comm) %>%
  mutate(first = if(col_type == "space"){paste0(space, " et al.,")}
         else if(col_type == "semi") {paste0(semi, " et al.,")}
         else if(col_type == "comm"){paste0(comm, " et al.,")}) %>% 
  ungroup() %>% 
  arrange(year) %>%
  mutate("No." = row_number()) %>% 
  select(id_study, title, year, first, No.)


DES_file <- DES_geo %>% 
  mutate(file = paste0("cleandata/", csv, ".csv")) %>% 
  select(id_study, file, author, year) %>% 
  left_join(., final, by = c("id_study" = "id_study")) %>% 
  mutate(comb = paste0(first, " (", No. , ")")) %>% 
  arrange(No.) %>% 
  mutate(comb = as_factor(comb))


study <- all_month_data %>% 
  left_join(., DES_file, by = c("file" = "file")) %>% 
  ggplot()+
  geom_line(aes(x = t, y = x*100, color = comb), alpha = 0.8)+
  scale_x_date(breaks = "1 year", date_labels = "%Y")+
  #scale_color_brewer(palette = "Set2")+
  labs(y = "Influenza test positive rate (%)")+
  coord_cartesian(ylim = c(0, 80),expand=F)+
  theme_cowplot()+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank())


mean <- all_month_data %>% 
  filter(geo_code == "110000") %>% 
  mutate(year = year(t)) %>% 
  mutate(month = month(t)) %>% 
  group_by(year, month) %>% 
  summarise(mean = mean(x), t = max(t)) %>% 
  mutate(legend = "Mean") %>% 
  ggplot()+
  geom_line(aes(x = t, y = mean*100, color = legend), size = 1)+
  scale_x_date(breaks = "1 year", date_labels = "%Y")+
  scale_color_manual(values = "black")+
  labs(y = "Influenza test positive rate (%)")+
  coord_cartesian(ylim = c(0, 80),expand=F)+
  theme_cowplot()+
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 8),
        strip.background = element_blank())


#### Plot combined #### 

plot_grid(
  study, mean, 
  labels = c("A", "B"),
  ncol = 1, rel_heights = c(0.5,0.5),
  align = "v")

ggsave("output/Sup_fig_S1.png",
       width = 210,
       height = 150,
       dpi = 320,
       units = "mm")

Sys.setlocale("LC_ALL","English")
