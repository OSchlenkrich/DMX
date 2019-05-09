# Rules Settlement Freedom ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com

# Select Variables
RS_F <- V_dem %>% 
  select(country_name, 
         year,
         HOS_appointment = v2expathhs, # HOS appointment in practice
         HOG_appointment = v2expathhg, # HOG appointment in practice
         HOG_appointment_legislature = v2ex_legconhog,  #HOG appointed by legislature
         HOS_appointment_legislature = v2ex_legconhos,  #HOS appointed by legislature
         HOS_power = v2exdfpphs_ord, # HOS proposes legislation in practice
         NoHOG = v2exhoshog, # HOS = HOG
         Elected_Leg = v2xlg_elecreg, # Legislative electoral regime index
         
         hs_royal_remove = "v2exrmhsol_3_mean", # HOS removal in practice
         hs_military_remove = "v2exrmhsol_4_mean", # HOS removal in practice
         hs_religious_remove = "v2exrmhsol_5_mean", # HOS removal in practice
         hs_tribe_remove = "v2exrmhsol_6_mean", # HOS removal in practice
         hg_royal_remove = "v2exrmhgnp_3_mean", # HOG removal in practice
         hg_military_remove = "v2exrmhgnp_4_mean", # HOG removal in practice
         hg_hs_remove = "v2exrmhgnp_5_mean", # HOG removal in practice
         hg_religious_remove = "v2exrmhgnp_6_mean", # HOG removal in practice
         hg_tribe_remove = "v2exrmhgnp_7_mean", # HOG removal in practice
         
         v2svdomaut, # Domestic autonomy
         v2svinlaut, # International autonomy
         v2cltort, # Freedom from torture
         v2clkill, # Freedom from political killings
         
         v2svdomaut_nr, # Nr Coders
         v2svinlaut_nr, # Nr Coders
         v2cltort_nr, # Nr Coders
         v2clkill_nr, # Nr Coders
         hs_remove_nr = v2exrmhsol_3_nr, # Nr Coders
         hg_remove_nr = v2exrmhgnp_3_nr # Nr Coders
  )


# Truncate Coders at 5
RS_F = RS_F %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .))) 


# Non-Democratic Vetoplayers
RS_F = RS_F %>%
  mutate(
    HOS_elected = ifelse(NoHOG == 1 & (HOS_appointment == 7 | (HOS_appointment_legislature == 1 & Elected_Leg == 1)), 1, 
                          ifelse(HOS_power<2 & (HOS_appointment == 7 | (HOS_appointment_legislature == 1 & Elected_Leg == 1)), 1, 0.5)),
    HOG_elected = ifelse(NoHOG == 0 & HOS_power==2 & (HOG_appointment == 8 | (HOG_appointment_legislature == 1 & Elected_Leg == 1) | (HOG_appointment == 6 & Elected_Leg == 1)),1,0.5)
  ) %>%
  mutate(
    HOS_veto = if_else(HOS_elected == 0.5 & HOG_elected == 0.5, 0.5, if_else(HOS_elected == 1 & (hs_royal_remove >= 0.4 | hs_military_remove >= 0.4 | hs_religious_remove >= 0.4 | hs_tribe_remove >= 0.4), 0.5, 
                                                                                if_else(HOS_elected == 1 & (hs_royal_remove >= 0.25 | hs_military_remove >= 0.25 | hs_religious_remove >= 0.25 | hs_tribe_remove >= 0.25), 0.75, 1))),
    HOG_veto = if_else(HOS_elected == 0.5 & HOG_elected == 0.5, 0.5, if_else(HOG_elected == 1 & (hg_royal_remove >= 0.4 | hg_military_remove >= 0.4 | hg_religious_remove >= 0.4 | hg_tribe_remove >= 0.4), 0.5, 
                                                                                  if_else(HOG_elected == 1 & (hg_royal_remove >= 0.25 | hg_military_remove >= 0.25 | hg_religious_remove >= 0.25 | hg_tribe_remove >= 0.25), 0.75, 1)))
  ) %>%
  rowwise()  %>%
  mutate(no_vetoplayer = min_fun(c(HOG_veto, HOS_veto))) %>%
  ungroup()


# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

RS_F = RS_F %>%
  mutate(
    rules_settlement_noveto_facto = no_vetoplayer,
    rules_settlement_personal_facto = cdf(scale_fun(v2cltort) * 0.5 + scale_fun(v2clkill) * 0.5),
    rules_settlement_independence_facto = minmax(cdf(scale_fun(v2svdomaut) * 0.5 + scale_fun(v2svinlaut)  * 0.5), 0.5)+0.5,
  ) 


RS_F = RS_F %>%
  rowwise() %>%
  mutate(
    rules_settlement_government_facto = min_fun(c(rules_settlement_noveto_facto, rules_settlement_independence_facto)), 
    rule_settlement_freedom_core = rules_settlement_government_facto * rules_settlement_personal_facto
    ) %>% 
  ungroup()


# Aggregate Number of Coders (Minimum)

RS_F = RS_F %>%
  rowwise() %>%
  mutate(
    rules_settlement_noveto_facto_nr = min_fun(c(hg_remove_nr, hs_remove_nr)),
    rules_settlement_personal_facto_nr = min_fun(c(v2cltort_nr, v2clkill_nr)),
    rules_settlement_independence_facto_nr = min_fun(c(v2svdomaut_nr, v2svinlaut_nr)),
    rules_settlement_government_facto_nr = min_fun(c(rules_settlement_noveto_facto_nr, rules_settlement_independence_facto_nr)),
    rule_settlement_freedom_core_nr = min_fun(c(rules_settlement_government_facto_nr, rules_settlement_personal_facto_nr))
  ) %>% 
  ungroup()


