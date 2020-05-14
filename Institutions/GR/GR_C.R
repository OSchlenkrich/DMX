# Guarantee of Rights Control ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


# Select Variables

GR_C <- V_dem %>% 
  select(country_name, 
         year, 
         v2juhccomp, # Compliance with high court
         v2jucomp, # Compliance with judiciary
         v2exrescon, # Executive respects constitution

         v2juhccomp_nr, # Nr Coders
         v2jucomp_nr, # Nr Coders 
         v2exrescon_nr # Nr Coders
         )  

# Truncate Coders at 5
GR_C = GR_C %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .)))


# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

GR_C = GR_C %>%
  mutate(
    rights_control_core = cdf(scale_fun(v2exrescon) * 0.25 + scale_fun(v2juhccomp) * 0.5 + scale_fun(v2jucomp) * 0.25),
    rights_control_core = if_else(rights_control_core < 0.001, 0.001, rights_control_core)
  )

# Aggregate Number of Coders (Minimum)
GR_C = GR_C %>%
  rowwise() %>%
  mutate(
    rights_control_core_nr = min_fun(c(v2exrescon_nr, v2juhccomp_nr, v2jucomp_nr))
  )


