# Public Communication Equality ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


# Select Variables

PC_E <- V_dem %>% 
  select(country_name, 
         year, 
         v2mebias, # Media bias
         v2merange, # Print/broadcast media perspectives

         v2mebias_nr,  # Nr Coders
         v2merange_nr)  # Nr Coders


# Truncate Coders at 5
PC_E = PC_E %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .))) 

# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

PC_E = PC_E %>%
  mutate(communication_representation_facto = cdf(scale_fun(v2mebias) * 0.5 + scale_fun(v2merange) * 0.5),
         communication_equality_core = communication_representation_facto,
         communication_equality_core = if_else(communication_equality_core < 0.001, 0.001, communication_equality_core)
  )


# Aggregate Number of Coders (Minimum)
PC_E = PC_E %>%
  rowwise() %>%
  mutate(
    communication_representation_facto_nr = min_fun(c(v2mebias_nr, v2merange_nr)),
    communication_equality_core_nr = min_fun(communication_representation_facto_nr)
  ) %>% 
  ungroup()
