# Procedures of Decision Control ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


PD_C <- V_dem %>% 
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
         
         v2elmulpar_ord, # Elections multiparty ordinal
         v2elfrfair, # Election free and fair
         v2elembaut, # EMB autonomy
         v2elembcap, # EMB capacity
         v2eldommon, # Election domestic election monitors
         v2elintmon, # Election international monitors
         
         v2elembaut_nr, # Nr Coders
         v2elembcap_nr, # Nr Coders
         v2elmulpar_nr # Nr Coders
  ) %>%
  arrange(country_name, year) 

# Fill in Values between elections for v2eldommon and v2elintmon
PD_C = PD_C %>%
  group_by(country_name) %>%
  mutate_at(vars(v2eldommon, v2elintmon), funs(fill_elections_w_ref(., v2x_elecreg, v2elfrfair))) %>%
  mutate(v2elmulpar_ord = fill_elections(v2elmulpar_ord, v2x_elecreg)) %>% 
  ungroup() 

# Austrias values for v2eldommon and v2elintmon changed unreasonably (V-Dem V8 --> V-Dem V9) (from 1 to 0)
# reverting
PD_C$v2eldommon[(PD_C$country_name == "Austria") & (PD_C$year >= 2006)] = 1


# Scaling of Variables and Truncate Coders at 5
PD_C = PD_C %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .))) %>% 
  mutate(v2elmulpar_ord_tran = minmax(as.numeric(v2elmulpar_ord), 0.5) + 0.5)


# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

PD_C = PD_C %>%
  mutate(ElectedExecutive = ifelse(NoHOG == 1 & (HOS_appointment == 7 | (HOS_appointment_legislature == 1 & Elected_Leg == 1)), 1, 
                                   ifelse(HOS_power<2 & (HOS_appointment == 7 | (HOS_appointment_legislature == 1 & Elected_Leg == 1)), 1,
                                          ifelse(HOS_power==2 & (HOG_appointment == 8 | (HOG_appointment_legislature == 1 & Elected_Leg == 1) | (HOG_appointment == 6 & Elected_Leg == 1)),1,0.5)))) %>%
  rowwise() %>%
  mutate(decision_choice_facto = min_fun(c(if_else(v2x_elecreg==0,0.001,v2elmulpar_ord_tran), ElectedExecutive))) %>%
  ungroup() %>%
  mutate(decision_autonomyemb_facto = if_else(v2x_elecreg==0,0.001, cdf(scale_fun(v2elembaut))),
         decision_capacityemb_facto = if_else(v2x_elecreg==0,0.001, cdf(scale_fun(v2elembcap))),
         decision_emb_facto = ((decision_autonomyemb_facto * decision_capacityemb_facto)^(1/2))*decision_choice_facto
  )

PD_C$decision_civilmonitors_facto =  NA
PD_C$decision_civilmonitors_facto[PD_C$v2x_elecreg==0] = 0.001
PD_C$decision_civilmonitors_facto[PD_C$v2eldommon==0 & PD_C$v2elintmon==0] = 0.001
PD_C$decision_civilmonitors_facto[PD_C$v2eldommon==1 & PD_C$v2elintmon==1] = 1
PD_C$decision_civilmonitors_facto[PD_C$v2eldommon==1 & PD_C$v2elintmon==0] = 1
PD_C$decision_civilmonitors_facto[PD_C$v2eldommon==0 & PD_C$v2elintmon==1] = 1
PD_C$decision_civilmonitors_facto[is.na(PD_C$v2eldommon)==T & PD_C$v2elintmon==1] = 1
PD_C$decision_civilmonitors_facto[is.na(PD_C$v2elintmon)==T & PD_C$v2eldommon==1] = 1

# Increases Missings from 850 to 2250
#PD_C$decision_civilmonitors_facto[is.na(PD_C$v2elintmon)==T & PD_C$v2eldommon==0] = 0.001
#PD_C$decision_civilmonitors_facto[is.na(PD_C$v2eldommon)==T & PD_C$v2elintmon==0] = 0.001

# Transform decision_civilmonitors_facto
PD_C$decision_civilmonitors_facto = minmax(PD_C$decision_civilmonitors_facto, 0.5) + 0.5

# Final Calculation
PD_C = PD_C %>%
  mutate(decision_control_core = decision_emb_facto * decision_civilmonitors_facto,
         decision_control_core = if_else(decision_control_core < 0.001, 0.001, decision_control_core)
  )

# Aggregate Number of Coders (Minimum)
PD_C = PD_C %>%
  rowwise() %>%
  mutate(decision_choice_facto_nr = min_fun(v2elmulpar_nr),
         decision_autonomyemb_facto_nr = min_fun(v2elembaut_nr),
         decision_capacityemb_facto_nr = min_fun(v2elembcap_nr),
         decision_emb_facto_nr = min_fun(c(decision_autonomyemb_facto_nr, decision_capacityemb_facto_nr)),
         decision_control_core_nr = min_fun(c(decision_emb_facto_nr, decision_choice_facto_nr))
  ) %>% 
  ungroup()
