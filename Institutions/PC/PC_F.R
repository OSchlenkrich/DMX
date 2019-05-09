# Public Communication Freedom ----

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source_c("Setup/Load&CleanDataset.R", "V_dem")

# See www.democracymatrix.com


# Select Variables

PC_F <- V_dem %>% 
  select(country_name, 
         year, 
         v2mecenefm, # Government censorship effort 
         v2mecenefi, # Internet censorship effort 
         v2meharjrn, # Harassment of journalists
         v2xcl_disc, # Freedom of discussion 
         v2clacfree, # Freedom of academic and cultural expression 
         v2clrelig,  # Freedom of religion 
         v2eldonate, # Disclosure of campaign donations 
         v2cldiscm,  # Freedom of discussion - men
         v2cldiscw,  # Freedom of discussion - women

         v2mecenefm_nr, # Nr Coders
         v2mecenefi_nr, # Nr Coders
         v2meharjrn_nr, # Nr Coders
         v2clacfree_nr, # Nr Coders
         v2clrelig_nr, # Nr Coders
         v2eldonate_nr, # Nr Coders
         v2cldiscm_nr, # Nr Coders
         v2cldiscw_nr # Nr Coders
  ) 

# Truncate Coders at 5
PC_F = PC_F %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .))) 

# Average v2cldiscm (male) and v2cldiscw (women) to freedom of discussion indicator
PC_F = PC_F %>%
  mutate(freedom_discussion = as.numeric(scale(v2cldiscm) * 0.5 + scale(v2cldiscw) * 0.5))

# Aggregation according to concept tree 
# https://www.democracymatrix.com/concept-tree-operationalisation/core-measurement

PC_F = PC_F %>%
  mutate(communication_press_facto = cdf(if_else(is.na(v2mecenefi)==T, scale_fun(v2mecenefm)*0.5 + scale_fun(v2meharjrn)*0.5, scale_fun(v2mecenefm)*(1/3) + scale_fun(v2meharjrn)*(1/3) + scale_fun(v2mecenefi)*(1/3))),
         communication_opinion_facto = cdf(scale_fun(freedom_discussion)*(1/3) + scale_fun(v2clacfree)*(1/3) + scale_fun(v2clrelig)*(1/3)),
         communication_expression_facto = (communication_press_facto * communication_opinion_facto)^(1/2),
         communication_freedom_core = communication_expression_facto
  ) 


# Aggregate Number of Coders (Minimum)

PC_F = PC_F %>%
  rowwise() %>%
  mutate(
    communication_press_facto_nr = min_fun(c(v2meharjrn_nr, v2mecenefi_nr, v2mecenefm_nr)),
    communication_opinion_facto_nr = min_fun(c(v2cldiscm_nr, v2cldiscw_nr, v2clacfree_nr, v2clrelig_nr)),
    communication_expression_facto_nr = min_fun(c(communication_press_facto_nr, communication_opinion_facto_nr)),
    communication_freedom_core_nr = communication_expression_facto_nr
  ) %>% 
  ungroup()
