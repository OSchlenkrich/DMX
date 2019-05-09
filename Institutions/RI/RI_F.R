# Regulation of the Intermediate Sphere Freedom ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


# Select Variables
RI_F <- V_dem %>% 
  select(country_name, 
         year, 
         v2psbars, # Barriers to parties
         v2psbars_ord, # Barriers to parties ordinal
         v2psparban, # Party ban
         v2psoppaut, # Opposition parties autonomy
         v2csreprss, # CSO repression
         v2cscnsult, # CSO consultation
         
         v2psbars_nr, # Nr Coders
         v2psparban_nr, # Nr Coders
         v2psoppaut_nr, # Nr Coders
         v2csreprss_nr, # Nr Coders
         v2cscnsult_nr # Nr Coders
  ) %>%
  arrange(country_name, year)


# Truncate Coders at 5
RI_F = RI_F %>% 
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .)))

# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

RI_F = RI_F %>%
  mutate(intermediate_partyfounding_facto = cdf(scale_fun(v2psbars) * 0.5 + scale_fun(v2psparban) * 0.5),
         intermediate_partyact_facto = if_else(v2psbars_ord==0 & is.na(v2psoppaut) == T, 0, cdf(scale_fun(v2psoppaut))),
         intermediate_csofounding_facto = cdf(scale_fun(v2csreprss)),
         intermediate_csoact_facto = cdf(scale_fun(v2cscnsult)),
         intermediate_partyorganization_facto = (intermediate_partyfounding_facto * intermediate_partyact_facto)^(1/2),
         intermediate_csoorganization_facto = (intermediate_csofounding_facto * intermediate_csoact_facto)^(1/2),
         intermediate_freedom_core = intermediate_partyorganization_facto*(3/4) + intermediate_csoorganization_facto*(1/4)
  ) 


# Aggregate Number of Coders (Minimum)
RI_F = RI_F %>%
  rowwise() %>%
  mutate(intermediate_partyfounding_facto_nr = min_fun(c(v2psbars_nr, v2psparban_nr)),
         intermediate_partyact_facto_nr = min_fun(v2psoppaut_nr),
         intermediate_csofounding_facto_nr = min_fun(v2csreprss_nr),
         intermediate_csoact_facto_nr = min_fun(v2cscnsult_nr),
         intermediate_partyorganization_facto_nr = min_fun(c(intermediate_partyfounding_facto_nr, intermediate_partyact_facto_nr)),
         intermediate_csoorganization_facto_nr = min_fun(c(intermediate_csofounding_facto_nr, intermediate_csoact_facto_nr)),
         intermediate_freedom_core_nr = min_fun(c(intermediate_partyorganization_facto_nr, intermediate_csoorganization_facto_nr))
  ) %>%
  ungroup()



