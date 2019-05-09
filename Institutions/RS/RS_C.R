# Rules Settlement Control ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


# Select Variables
RS_C <- V_dem %>% 
  select(country_name, 
         year,
         v2lgqstexp, # Legislature questions officials in practice
         v2lginvstp, # Legislature investigates in practice
         v2lgfunds, # Legislature controls resources
         v2lgfunds_ord, # Legislature controls resources ordinal
         v2lgotovst, # Executive oversight
         v2lgbicam, # Legislature bicameral

         v2lgqstexp_nr, # Nr Coders
         v2lginvstp_nr, # Nr Coders
         v2lgfunds_nr, # Nr Coders
         v2lgotovst_nr # Nr Coders
         ) 

# Truncate Coders at 5
RS_C = RS_C %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .))) 


# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

RS_C = RS_C %>%
  mutate(rules_settlement_admcontrol_facto = if_else(v2lgbicam==0, 0, cdf(scale_fun(v2lgotovst))),
         rules_settlement_parlcontrol_facto = if_else(v2lgbicam==0, 0, cdf(scale_fun(v2lgfunds) * (0.3) +  scale_fun(v2lginvstp) * (0.4) + scale_fun(v2lgqstexp) * (0.3))),
         rule_settlement_control_core = rules_settlement_admcontrol_facto * (1/3) + rules_settlement_parlcontrol_facto * (2/3)
  )  


# Aggregate Number of Coders (Minimum)

RS_C = RS_C %>%
  rowwise() %>%
  mutate(
    rules_settlement_admcontrol_facto_nr = min_fun(v2lgotovst_nr),
    rules_settlement_parlcontrol_facto_nr = min_fun(c(v2lgfunds_nr, v2lginvstp_nr, v2lgqstexp_nr)),
    rule_settlement_control_core_nr = min_fun(c(rules_settlement_admcontrol_facto_nr, rules_settlement_parlcontrol_facto_nr))
  ) %>% 
  ungroup()
