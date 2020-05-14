# Regulation of the Intermediate Sphere Control ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com

# Select Variables
RI_C <- V_dem %>% 
  select(country_name, 
         year, 
         v2lgoppart, # Legislature opposition parties
         v2dlengage, # Engaged society
         v2psbars, # Barriers to parties
         v2psbars_ord, # Barriers to parties ordinal
         v2lgbicam, # Legislature bicameral

         v2lgoppart_nr, # Nr Coders
         v2dlengage_nr # Nr Coders
  ) 

# Truncate Coders at 5
RI_C = RI_C %>% 
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .)))


# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

RI_C = RI_C %>%
  mutate(intermediate_partycontrol_facto = if_else(v2lgbicam==0,0.001, cdf(scale_fun(v2lgoppart))),
         intermediate_csocontrol_facto = cdf(scale_fun(v2dlengage)),
         intermediate_control_core = intermediate_partycontrol_facto * (3/4) +  intermediate_csocontrol_facto * (1/4),
         intermediate_control_core = if_else(intermediate_control_core < 0.001, 0.001, intermediate_control_core)
  ) 

# Aggregate Number of Coders (Minimum)
RI_C = RI_C%>%
  rowwise() %>%
  mutate(
    intermediate_partycontrol_facto_nr = min_fun(v2lgoppart_nr),
    intermediate_csocontrol_facto_nr = min_fun(v2dlengage_nr),
    intermediate_control_core_nr = min_fun(c(intermediate_partycontrol_facto_nr, intermediate_csocontrol_facto_nr))
  ) %>% 
  ungroup()