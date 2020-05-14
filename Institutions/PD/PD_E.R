# Procedures of Decision Equality ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


# Select Variables
PD_E = V_dem %>% 
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
         v2elrstrct, # Candidate restriction by ethnicity, race, religion, or language
         v2elrsthos, # HOS restriction by ethnicity, race, religion, or language
         v2elrsthog, # HOG restriction by ethnicity, race, religion, or language 
         v2elrgstry, # Election voter registry
         v2elsuffrage, # Percent of population with suffrage
         
         v2elrgstry_nr, # Nr Coders
         v2elmulpar_nr # Nr Coders
  ) %>%
  arrange(country_name, year)


# Fill in Values between elections
PD_E = PD_E %>% 
  mutate(v2elrgstry = fill_elections(v2elrgstry, v2x_elecreg),
         v2elrgstry_nr = fill_elections(v2elrgstry_nr, v2x_elecreg),
         v2elmulpar_ord = fill_elections(v2elmulpar_ord, v2x_elecreg),
         v2elmulpar_nr = fill_elections(v2elmulpar_nr, v2x_elecreg)
  )


# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement


# Scaling of Variables and Truncate Coders at 5

PD_E =   PD_E %>% 
  mutate(v2elsuffrage_trans = v2elsuffrage/100,
         v2elsuffrage_trans = ifelse(v2elsuffrage_trans == 0, 0.001, v2elsuffrage_trans))  %>%
  mutate(v2elmulpar_ord_tran = minmax(as.numeric(v2elmulpar_ord), 0.5) + 0.5) %>% 
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .)))

# Create all Components

PD_E <- PD_E %>%
  mutate(ElectedExecutive = ifelse(NoHOG == 1 & (HOS_appointment == 7 | (HOS_appointment_legislature == 1 & Elected_Leg == 1)), 1, 
                                   ifelse(HOS_power<2 & (HOS_appointment == 7 | (HOS_appointment_legislature == 1 & Elected_Leg == 1)), 1,
                                          ifelse(HOS_power==2 & (HOG_appointment == 8 | (HOG_appointment_legislature == 1 & Elected_Leg == 1) | (HOG_appointment == 6 & Elected_Leg == 1)),1,0.5)))) %>%
  rowwise() %>%
  mutate(decision_choice_facto = min_fun(c(if_else(v2x_elecreg==0,0.001,v2elmulpar_ord_tran), ElectedExecutive))) %>%
  ungroup() %>%
  mutate(decision_registration_facto = if_else(v2x_elecreg==0,0.5, (minmax(cdf(scale_fun(v2elrgstry)), 0.5) + 0.5)),
         decision_activeextension_facto = if_else(v2x_elecreg==0, 0.001, v2elsuffrage_trans),
         decision_activesuffrage_facto = decision_activeextension_facto * decision_registration_facto,
         decision_equality_core = decision_activesuffrage_facto * decision_choice_facto,
         decision_equality_core = if_else(decision_equality_core < 0.001, 0.001, decision_equality_core)
  ) 


# Aggregate Number of Coders (Minimum)

PD_E = PD_E %>%
  rowwise() %>%
  mutate(decision_choice_facto_nr = min_fun(v2elmulpar_nr),
         decision_registration_facto_nr = min_fun(v2elrgstry_nr),
         decision_activesuffrage_facto_nr = min_fun(decision_registration_facto_nr),
         decision_equality_core_nr = min_fun(c(decision_activesuffrage_facto_nr, decision_choice_facto_nr))
  ) %>% 
  ungroup()
