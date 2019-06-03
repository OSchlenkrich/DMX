source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source("Setup/CheckFunctions.R")

source_c("Institutions/PD/PD_Dataset.R", "PD")
source_c("Institutions/RI/RI_Dataset.R", "RI")
source_c("Institutions/PC/PC_Dataset.R", "PC")
source_c("Institutions/GR/GR_Dataset.R", "GR")
source_c("Institutions/RS/RS_Dataset.R", "RS")



# Combine Datasets
core_measurement = PD %>% 
  left_join(RI, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) %>% 
  left_join(PC, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) %>% 
  left_join(GR, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) %>% 
  left_join(RS, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y")))  


# Calculate Dimensional and Institutional Values and Total Value
core_measurement = core_measurement %>%
  mutate(freedom_dim_index_core =  (decision_freedom_core * intermediate_freedom_core * communication_freedom_core * rights_freedom_core *rule_settlement_freedom_core)^(1/5),
         equality_dim_index_core =  (decision_equality_core * intermediate_equality_core * communication_equality_core * rights_equality_core *rule_settlement_equality_core)^(1/5),
         control_dim_index_core =  (decision_control_core * intermediate_control_core * communication_control_core * rights_control_core *rule_settlement_control_core)^(1/5),
         
         decision_inst_index_core = (decision_freedom_core * decision_equality_core * decision_control_core)^(1/3),
         intermediate_inst_index_core = (intermediate_freedom_core * intermediate_equality_core * intermediate_control_core)^(1/3),
         communication_inst_index_core = (communication_freedom_core * communication_equality_core * communication_control_core)^(1/3),
         rights_inst_index_core = (rights_freedom_core * rights_equality_core * rights_control_core)^(1/3),
         rule_settlement_inst_index_core = (rule_settlement_freedom_core * rule_settlement_equality_core * rule_settlement_control_core)^(1/3),
         
         total_index_core = (freedom_dim_index_core * equality_dim_index_core * control_dim_index_core)^(1/3)
  )  %>%
  arrange(country_name, year)


# Aggregate Number of Coders  for Dimensions and Institutions (Minimum)
core_measurement = core_measurement %>%
  rowwise() %>%
  mutate(
    freedom_dim_index_core_nr = min_fun(c(decision_freedom_core_nr, intermediate_freedom_core_nr, communication_freedom_core_nr, rights_freedom_core_nr,rule_settlement_freedom_core_nr)),
    equality_dim_index_core_nr = min_fun(c(decision_equality_core_nr, intermediate_equality_core_nr, communication_equality_core_nr, rights_equality_core_nr,rule_settlement_equality_core_nr)),
    control_dim_index_core_nr = min_fun(c(decision_control_core_nr, intermediate_control_core_nr, communication_control_core_nr, rights_control_core_nr,rule_settlement_control_core_nr)),
    
    decision_inst_index_core_nr = min_fun(c(decision_freedom_core_nr, decision_equality_core_nr, decision_control_core_nr)),
    intermediate_inst_index_core_nr = min_fun(c(intermediate_freedom_core_nr, intermediate_equality_core_nr, intermediate_control_core_nr)),
    communication_inst_index_core_nr = min_fun(c(communication_freedom_core_nr, communication_equality_core_nr, communication_control_core_nr)),
    rights_inst_index_core_nr = min_fun(c(rights_freedom_core_nr, rights_equality_core_nr, rights_control_core_nr)),
    rule_settlement_inst_index_core_nr = min_fun(c(rule_settlement_freedom_core_nr, rule_settlement_equality_core_nr, rule_settlement_control_core_nr)),
    
    total_index_core_nr = min_fun(c(freedom_dim_index_core_nr, equality_dim_index_core_nr, control_dim_index_core_nr)),
    
  ) %>% 
  ungroup()


# Classification
core_measurement$classification_core = classification(core_measurement, "core")


# Order Columns
core_measurement = core_measurement %>% 
  select(ordered_column_names(core_measurement))


# Cleaning
rm(PD, RI, PC, GR, RS)

