# Regulation of the Intermediate Sphere Equality ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


# Select Variables
RI_E = V_dem %>% 
  select(country_name, 
         year, 
         v2pepwrses, # Power distributed by socioeconomic position
         v2pepwrsoc, # Power distributed by social group
         v2pepwrgen, # Power distributed by gender

         v2pepwrses_nr, # Nr Coders
         v2pepwrsoc_nr, # Nr Coders
         v2pepwrgen_nr # Nr Coders
         ) 


# Truncate Coders at 5
RI_E = RI_E %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .)))

# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

RI_E = RI_E %>% 
  mutate(intermediate_equality_core = cdf(scale_fun(v2pepwrses)*(1/3) + scale_fun(v2pepwrsoc)*(1/3) + scale_fun(v2pepwrgen)*(1/3))
  ) 

# Aggregate Number of Coders (Minimum)
RI_E = RI_E  %>%
  rowwise() %>%
  mutate(intermediate_equality_core_nr = min_fun(c(v2pepwrses_nr, v2pepwrsoc_nr, v2pepwrgen_nr))
  ) %>% 
  ungroup()
