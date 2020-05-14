# Public Communication Control ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


# Select Variables
PC_C <- V_dem %>% 
  select(country_name, 
         year, 
         v2mecrit, # Print/broadcast media critical
         v2meslfcen, # Media self-censorship

         v2mecrit_nr, # Nr Coders
         v2meslfcen_nr # Nr Coders
         ) 


# Truncate Coders at 5
PC_C = PC_C %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .))) 

# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement
PC_C = PC_C %>%
  mutate(communication_control_core = cdf(scale_fun(v2mecrit)*0.6 + scale_fun(v2meslfcen)*0.4),
         communication_control_core = if_else(communication_control_core < 0.001, 0.001, communication_control_core)
  ) 

# Aggregate Number of Coders (Minimum)
PC_C = PC_C %>%
  rowwise() %>%
  mutate(
    communication_control_core_nr = min_fun(c(v2mecrit_nr, v2meslfcen_nr)),
  ) %>% 
  ungroup()

