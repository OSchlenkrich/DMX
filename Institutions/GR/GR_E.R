# Guarantee of Rights Equality ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


# Select Variables

GR_E <- V_dem %>% 
  select(country_name, 
         year, 
         v2clacjust, # Social class equality in respect for civil liberties
         v2clsocgrp, # Social group equality in respect for civil liberties
         v2clacjstm, # Access to justice for men
         v2clacjstw, # Access to justice for women
         v2xcl_acjst, # Access to justice

         v2clacjust_nr, # Nr Coders
         v2clsocgrp_nr, # Nr Coders
         v2clacjstm_nr, # Nr Coders
         v2clacjstw_nr # Nr Coders
         )  

# Truncate Coders at 5
GR_E = GR_E %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .))) 


# Average v2clacjstm (male) and v2clacjstw (women) to freedom of discussion indicator
GR_E = GR_E %>%
  mutate(access_justice_average = as.numeric(scale_fun(v2clacjstw) * 0.5 + scale_fun(v2clacjstm) * 0.5)) 


# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

GR_E = GR_E %>%
  mutate(rights_access_facto = cdf(access_justice_average),
         rights_equalitylaw_facto = cdf(scale_fun(v2clacjust) * 0.5 + scale_fun(v2clsocgrp)  * 0.5),
         rights_equality_core = (rights_access_facto * rights_equalitylaw_facto)^(1/2)
  )  

# Aggregate Number of Coders (Minimum)
GR_E = GR_E %>%
  rowwise() %>%
  mutate(
    rights_access_facto_nr = min_fun(c(v2clacjstw_nr, v2clacjstm_nr)),
    rights_equalitylaw_facto_nr = min_fun(c(v2clacjust_nr, v2clsocgrp_nr)),
    rights_equality_core_nr = min_fun(c(rights_access_facto_nr, rights_equalitylaw_facto_nr))
  ) %>% 
  ungroup()

