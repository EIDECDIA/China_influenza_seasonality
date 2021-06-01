### Search results 

# Produces results for:

# Table 1

# Read in data and useful functions
source("script/data_and_functions.R")

# Table 1 - Number of studies repoting each outcome and the areas repesented
table_1 <- function(){
  
  table_part <- function(MEASURE, POP, STRAIN){
    
    # no studies and prv, prf, cty
    prv <- DES_geo %>% 
      filter(measure == MEASURE) %>% 
      filter(pop_denom == POP) %>% # ILI check
      filter(strain == STRAIN) %>% 
      filter(prf_c == "00") %>% 
      filter(cty_c == "00") %>% 
      pull(geo_code) %>% # "id_study" or "prf"
      unique() %>% 
      length() %>% 
      as_tibble() %>% 
      select(no_region = value) %>% 
      mutate(no_study = DES_geo %>% 
               filter(measure == MEASURE) %>% 
               filter(pop_denom == POP) %>% # ILI check
               filter(strain == STRAIN) %>% 
               filter(prf_c == "00") %>% 
               filter(cty_c == "00") %>% 
               pull(id_study) %>% # "id_study" or "prf"
               unique() %>% 
               length()) %>% 
      mutate(region = "prv", outcome = MEASURE, total = 31, strain = STRAIN, pop = POP) %>% 
      mutate(percent= no_region/total * 100) %>% 
      select(region, outcome, pop, strain, no_study, no_region, total, percent)
    
    prf <- DES_geo %>% 
      filter(measure == MEASURE) %>% 
      filter(pop_denom == POP) %>% # ILI check
      filter(strain == STRAIN) %>% 
      filter(prf_c != "00") %>% 
      filter(cty_c == "00") %>% 
      pull(geo_code) %>% # "id_study" or "prf"
      unique() %>% 
      length() %>% 
      as_tibble() %>% 
      select(no_region = value) %>% 
      mutate(no_study = DES_geo %>% 
               filter(measure == MEASURE) %>% 
               filter(pop_denom == POP) %>% # ILI check
               filter(strain == STRAIN) %>% 
               filter(prf_c != "00") %>% 
               filter(cty_c == "00") %>% 
               pull(id_study) %>% # "id_study" or "prf"
               unique() %>% 
               length()) %>% 
      mutate(region = "prf", outcome = MEASURE, total = 341, strain = STRAIN, pop = POP) %>% 
      mutate(percent= no_region/total * 100) %>% 
      select(region, outcome, pop, strain, no_study, no_region, total, percent)
    
    cty <- DES_geo %>% 
      filter(measure == MEASURE) %>% 
      filter(pop_denom == POP) %>% # ILI check
      filter(strain == STRAIN) %>% 
      filter(prf_c != "00") %>% 
      filter(cty_c != "00") %>% 
      pull(geo_code) %>% # "id_study" or "prf"
      unique() %>% 
      length() %>% 
      as_tibble() %>% 
      select(no_region = value) %>% 
      mutate(no_study = DES_geo %>% 
               filter(measure == MEASURE) %>% 
               filter(pop_denom == POP) %>% # ILI check
               filter(strain == STRAIN) %>% 
               filter(prf_c != "00") %>% 
               filter(cty_c != "00") %>% 
               pull(id_study) %>% # "id_study" or "prf"
               unique() %>% 
               length()) %>% 
      mutate(region = "cty", outcome = MEASURE, total = 2937, strain = STRAIN, pop = POP) %>% 
      mutate(percent= no_region/total * 100) %>% 
      select(region, outcome, pop, strain, no_study, no_region, total, percent)
    
    bind_rows(prv, prf, cty)
    
  }
  
  bind_rows(table_part("ILI %", "outpatient", "N/A"),
            table_part("Positive rate", "ILI outpatient", "All"),
            table_part("Positive rate", "SARI", "All"),
            table_part("Mort_flu", "mortality_resp", "N/A"),
            table_part("Mort_flu", "mortality_all", "N/A")) #### Mortality calculate by hand combined
}

# Table 1 output
#### Caluclate mortality combined by hand 
table_1()
