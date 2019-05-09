# Guarantee of Rights Freedom ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


# Select Variables
GR_F <- V_dem %>% 
  select(country_name, 
         year, 
         v2juhcind, # High court independence
         v2juncind, # Lower court independence
         v2cltrnslw, # Transparent laws with predictable enforcement
         v2juaccnt, # Judicial accountability

         v2juhcind_nr, # Nr Coders
         v2juncind_nr, # Nr Coders
         v2cltrnslw_nr, # Nr Coders
         v2juaccnt_nr) # Nr Coders


# Truncate Coders at 5
GR_F = GR_F %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .)))



# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

GR_F = GR_F %>%
  mutate(rights_independence_facto = cdf(scale_fun(v2juhcind)*(2/3) + scale_fun(v2juncind)*(1/3)),
         rights_certainty_facto = cdf(scale_fun(v2juaccnt) * 0.3 + scale_fun(v2cltrnslw) * 0.7),
         rights_freedom_core = (rights_independence_facto * rights_certainty_facto)^(1/2),
  )  


# Aggregate Number of Coders (Minimum)
GR_F = GR_F%>%
  rowwise() %>%
  mutate(
    rights_independence_facto_nr = min_fun(c(v2juhcind_nr, v2juncind_nr)),
    rights_certainty_facto_nr = min_fun(c(v2juaccnt_nr, v2cltrnslw_nr)),
    rights_freedom_core_nr = min_fun(c(rights_independence_facto_nr, rights_certainty_facto_nr))
  ) %>% 
  ungroup()

