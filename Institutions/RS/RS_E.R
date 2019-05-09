# Rules Settlement Equality ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


# Select Variables
RS_E <- V_dem %>% 
  select(country_name, 
         year,
         v2lgdsadlo, # Representation of disadvantaged social groups
         v2lgdsadlobin_ord, # Representation of disadvantaged social groups binary
         v2dlconslt, # Range of consultation
         v2clrspct, # Rigorous and impartial public administration
         v2lgbicam, # Legislature bicameral

         v2lgdsadlo_nr, # Nr Coders
         v2lgdsadlobin_nr, # Nr Coders
         v2dlconslt_nr, # Nr Coders
         v2clrspct_nr # Nr Coders
         ) 

# Truncate Coders at 5
RS_E = RS_E %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .))) 


# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement
RS_E = RS_E %>%
  mutate(rules_settlement_parltreatment_facto = cdf(scale_fun(v2dlconslt)),
         rules_settlement_exetreatment_facto = cdf(scale_fun(v2clrspct)),
         rule_settlement_equality_core = (rules_settlement_parltreatment_facto *  rules_settlement_exetreatment_facto)^(1/2)
  ) 


# Aggregate Number of Coders (Minimum)
RS_E = RS_E %>%
  rowwise() %>%
  mutate(
    rules_settlement_parltreatment_facto_nr = min_fun(v2dlconslt_nr),
    rules_settlement_exetreatment_facto_nr = min_fun(v2clrspct_nr),
    rule_settlement_equality_core_nr = min_fun(c(rules_settlement_parltreatment_facto_nr, rules_settlement_exetreatment_facto_nr))
  ) %>% 
  ungroup()

