# Procedures of Decision Freedom ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com

# Select Variables
PD_F = V_dem %>% 
  select(country_name, 
         year,
         HOS_appointment = v2expathhs, # HOS appointment in practice
         HOG_appointment = v2expathhg, # HOG appointment in practice
         HOG_appointment_legislature = v2ex_legconhog, #HOG appointed by legislature
         HOS_appointment_legislature = v2ex_legconhos, # HOS appointed by legislature
         HOS_power = v2exdfpphs_ord, # HOS proposes legislation in practice
         NoHOG = v2exhoshog, # HOS = HOG
         v2x_elecreg, # Electoral regime index
         Elected_Leg = v2xlg_elecreg, # Legislative electoral regime index
         
         v2elasmoff_ord, # Legislative election assume office ordinal
         v2elirreg,  # Election other voting irregularities
         v2elintim,  # Election government intimidation
         v2elfrfair, # Election free and fair
         v2elmulpar_ord, # Elections multiparty ordinal
         
         v2elasmoff_nr, # Nr Coders
         v2elirreg_nr, # Nr Coders
         v2elintim_nr, # Nr Coders
         v2elfrfair_nr,# Nr Coders 
         v2elmulpar_nr # Nr Coders
  ) %>%
  arrange(country_name, year)

# Fill in Values between elections
PD_F = PD_F %>%
  group_by(country_name) %>%
  mutate_at(vars(starts_with("v2el")), funs(fill_elections(., v2x_elecreg))) %>%
  ungroup()  

# Truncate Number of Coders at 5
PD_F = PD_F %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .)))


# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

# Transform v2elasmoff_ord and v2elmulpar_ord between 0.5 and 1
PD_F = PD_F %>%
  ungroup() %>%
  mutate(v2elasmoff_ord_tran = minmax(as.numeric(v2elasmoff_ord), 0.5) + 0.5) %>%
  mutate(v2elmulpar_ord_tran = minmax(as.numeric(v2elmulpar_ord), 0.5) + 0.5)

# Create all Components
PD_F = PD_F %>%
  mutate(ElectedExecutive = ifelse(NoHOG == 1 & (HOS_appointment == 7 | (HOS_appointment_legislature == 1 & Elected_Leg == 1)), 1, 
                                   ifelse(HOS_power<2 & (HOS_appointment == 7 | (HOS_appointment_legislature == 1 & Elected_Leg == 1)), 1,
                                          ifelse(HOS_power==2 & (HOG_appointment == 8 | (HOG_appointment_legislature == 1 & Elected_Leg == 1) | (HOG_appointment == 6 & Elected_Leg == 1)),1,0.5)))) %>%
  rowwise() %>%
  mutate(decision_choice_facto = min_fun(c(if_else(v2x_elecreg==0,0,v2elmulpar_ord_tran), ElectedExecutive))) %>%
  ungroup() %>%
  mutate(
    decision_allocation_facto = if_else(v2x_elecreg==0,0,v2elasmoff_ord_tran),
    decision_procedure_facto = if_else(v2x_elecreg==0,0,cdf(scale_fun(v2elirreg)*.25 + scale_fun(v2elintim)*.25 + scale_fun(v2elfrfair)*.5)),
    decision_freedom_core = decision_procedure_facto*decision_allocation_facto*decision_choice_facto,
    
  ) 


# Aggregate Number of Coders (Minimum)
PD_F = PD_F %>%
  rowwise() %>%
  mutate(decision_allocation_facto_nr = min_fun(v2elasmoff_nr),
         decision_choice_facto_nr = min_fun(v2elmulpar_nr),
         decision_procedure_facto_nr = min_fun(c(v2elirreg_nr, v2elintim_nr, v2elfrfair_nr)),
         decision_freedom_core_nr = min_fun(c(decision_procedure_facto_nr, decision_allocation_facto_nr, decision_choice_facto_nr))
  ) %>% 
  ungroup()

