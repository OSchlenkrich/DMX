source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source("Setup/CheckFunctions.R")
source_c("Measurement_Levels/Core_Measurement.R", "core_measurement")


# Impact of Context Factors on Core Measurement
scale_min = 0.325
lower_bound = 1-scale_min

# Select Variables
broadband_indicators = V_dem %>%
  select(country_name,
         year,
         v2x_elecreg,  # Electoral regime index
         v2elpeace, # Election other electoral violence
         v2peedueq, # Educational equality
         v2mecorrpt, # Media corrupt
         v2jucorrdc, # Judicial corruption decision
         v2lgcrrpt, # Legislature corrupt activities
         v2exbribe, # Executive bribery and corrupt exchanges
         v2exembez, # Executive embezzlement and theft
         v2excrptps, # Public sector corrupt exchanges
         v2exthftps, # Public sector theft
         v2psprlnks, # Party linkages
         v2svindep, # Independent states
         v2lgbicam, # Legislature bicameral

         v2elpeace_nr, # Nr Coders
         v2peedueq_nr, # Nr Coders
         v2mecorrpt_nr, # Nr Coders
         v2jucorrdc_nr, # Nr Coders
         v2lgcrrpt_nr, # Nr Coders
         v2exbribe_nr, # Nr Coders
         v2exembez_nr, # Nr Coders
         v2excrptps_nr, # Nr Coders
         v2exthftps_nr, # Nr Coders
         v2psprlnks_nr # Nr Coders
         ) %>%
  arrange(country_name, year) 

# Fill in Values between elections
broadband_indicators = broadband_indicators %>%
  group_by(country_name) %>%
  mutate_at(vars(starts_with("v2el")), funs(fill_elections(., v2x_elecreg))) %>%
  ungroup() 

# Truncate Number of Coders at 5
broadband_indicators = broadband_indicators %>%
  mutate_at(vars(ends_with("_nr")), funs(ifelse(. > 5 , 5, .))) 

# Calculate and Scale Context Components
broadband_indicators = broadband_indicators %>%
  mutate(decision_context_facto = if_else(v2x_elecreg==0,0,cdf(scale_fun(v2elpeace))),
         intermediate_context_facto = cdf(scale_fun(v2psprlnks)),
         communication_context_facto = cdf(scale_fun(v2mecorrpt)),
         rights_context_facto = cdf(scale_fun(v2jucorrdc)),
         rules_settlement_parlnocorr_facto = if_else(v2lgbicam== 0, 0, cdf(scale_fun(v2lgcrrpt))),
         rules_settlement_exenocorr_facto = cdf(scale_fun(v2exbribe) * 0.25 + scale_fun(v2exembez) * 0.25 + scale_fun(v2excrptps) * 0.25 + scale_fun(v2exthftps) * 0.25),
         rules_settlement_context_facto = rules_settlement_parlnocorr_facto * 0.5 + rules_settlement_exenocorr_facto * 0.5,
         equality_context_facto = cdf(scale_fun(v2peedueq))
  ) %>%
  mutate(decision_context_facto = minmax(decision_context_facto, scale_min) + lower_bound,
         intermediate_context_facto = minmax(intermediate_context_facto, scale_min) + lower_bound,
         communication_context_facto = minmax(communication_context_facto, scale_min) + lower_bound,
         rights_context_facto = minmax(rights_context_facto , scale_min) + lower_bound,
         rules_settlement_context_facto = minmax(rules_settlement_context_facto, scale_min) + lower_bound,
         equality_context_facto = minmax(equality_context_facto, scale_min) + lower_bound
  ) 

# Aggregate Number of Coders (Minimum)
broadband_indicators = broadband_indicators %>%
  rowwise() %>%
  mutate(
    decision_context_facto_nr = min_fun(v2elpeace_nr),
    intermediate_context_facto_nr = min_fun(v2psprlnks_nr),
    communication_context_facto_nr = min_fun(v2mecorrpt_nr),
    rights_context_facto_nr = min_fun(v2jucorrdc_nr),
    rules_settlement_parlnocorr_facto_nr = min_fun(v2lgcrrpt_nr),
    rules_settlement_exenocorr_facto_nr = min_fun(c(v2exbribe_nr, v2exembez_nr, v2excrptps_nr, v2exthftps_nr)),
    rules_settlement_context_facto_nr = min_fun(c(rules_settlement_parlnocorr_facto_nr, rules_settlement_exenocorr_facto_nr)),
    equality_context_facto_nr = min_fun(v2peedueq_nr) 
  ) %>% 
  ungroup()



# Combine broadband_indicators and core_measuremt to context_measurement
context_measurement = core_measurement %>%
  left_join(broadband_indicators, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  mutate(decision_freedom_context =  decision_freedom_core * decision_context_facto,
         decision_equality_context =  decision_equality_core * ((decision_context_facto * equality_context_facto)^(1/2)),
         decision_control_context =  decision_control_core * decision_context_facto,
         
         intermediate_freedom_context =  intermediate_freedom_core * intermediate_context_facto,
         intermediate_equality_context =  intermediate_equality_core * ((intermediate_context_facto * equality_context_facto)^(1/2)),
         intermediate_control_context =  intermediate_control_core * intermediate_context_facto,
         
         communication_freedom_context =  communication_freedom_core * communication_context_facto,
         communication_equality_context =  communication_equality_core * ((communication_context_facto * equality_context_facto)^(1/2)),
         communication_control_context =  communication_control_core * communication_context_facto,
         
         rights_freedom_context =  rights_freedom_core * rights_context_facto,
         rights_equality_context =  rights_equality_core * ((rights_context_facto * equality_context_facto)^(1/2)),
         rights_control_context =  rights_control_core * rights_context_facto,
         
         rule_settlement_freedom_context =  rule_settlement_freedom_core * rules_settlement_context_facto,
         rule_settlement_equality_context =  rule_settlement_equality_core * ((rules_settlement_context_facto * equality_context_facto)^(1/2)),
         rule_settlement_control_context =  rule_settlement_control_core * rules_settlement_context_facto,
  ) 

# Aggregate Number of Coders for Matrix_Fields (Minimum)
context_measurement = context_measurement %>%
  rowwise() %>%
  mutate(
    decision_freedom_context_nr =  min_fun(c(decision_freedom_core_nr, decision_context_facto_nr)),
    decision_equality_context_nr =  min_fun(c(decision_equality_core_nr, decision_context_facto_nr, equality_context_facto_nr)),
    decision_control_context_nr =  min_fun(c(decision_control_core_nr, decision_context_facto_nr)),
    
    intermediate_freedom_context_nr =  min_fun(c(intermediate_freedom_core_nr, intermediate_context_facto_nr)),
    intermediate_equality_context_nr =  min_fun(c(intermediate_equality_core_nr, intermediate_context_facto_nr, equality_context_facto_nr)),
    intermediate_control_context_nr =  min_fun(c(intermediate_control_core_nr, intermediate_context_facto_nr)),
    
    communication_freedom_context_nr =  min_fun(c(communication_freedom_core_nr, communication_context_facto_nr)),
    communication_equality_context_nr =  min_fun(c(communication_equality_core_nr, communication_context_facto_nr, equality_context_facto_nr)),
    communication_control_context_nr =  min_fun(c(communication_control_core_nr, communication_context_facto_nr)),
    
    rights_freedom_context_nr =  min_fun(c(rights_freedom_core_nr, rights_context_facto_nr)),
    rights_equality_context_nr =  min_fun(c(rights_equality_core_nr, rights_context_facto_nr, equality_context_facto_nr)),
    rights_control_context_nr =  min_fun(c(rights_control_core_nr, rights_context_facto_nr)),
    
    rule_settlement_freedom_context_nr =  min_fun(c(rule_settlement_freedom_core_nr, rules_settlement_context_facto_nr)),
    rule_settlement_equality_context_nr =  min_fun(c(rule_settlement_equality_core_nr, rules_settlement_context_facto_nr, equality_context_facto_nr)),
    rule_settlement_control_context_nr =  min_fun(c(rule_settlement_control_core_nr, rules_settlement_context_facto_nr))
  ) %>% 
  ungroup()


# Calculate Dimensional and Institutional Values and Total Value
context_measurement = context_measurement %>%
  mutate(
    freedom_dim_index_context =  (decision_freedom_context * intermediate_freedom_context * communication_freedom_context * rights_freedom_context *rule_settlement_freedom_context)^(1/5),
    equality_dim_index_context =  (decision_equality_context * intermediate_equality_context * communication_equality_context * rights_equality_context *rule_settlement_equality_context)^(1/5),
    control_dim_index_context =  (decision_control_context * intermediate_control_context * communication_control_context * rights_control_context *rule_settlement_control_context)^(1/5),
    
    decision_inst_index_context = (decision_freedom_context * decision_equality_context * decision_control_context)^(1/3),
    intermediate_inst_index_context = (intermediate_freedom_context * intermediate_equality_context * intermediate_control_context)^(1/3),
    communication_inst_index_context = (communication_freedom_context * communication_equality_context * communication_control_context)^(1/3),
    rights_inst_index_context = (rights_freedom_context * rights_equality_context * rights_control_context)^(1/3),
    rule_settlement_inst_index_context = (rule_settlement_freedom_context * rule_settlement_equality_context * rule_settlement_control_context)^(1/3),
    
    total_index_context = (freedom_dim_index_context * equality_dim_index_context * control_dim_index_context)^(1/3)
  )  %>% 
  arrange(country_name, year)


# Aggregate Number of Coders for Dimensions and Institutions (Minimum)
context_measurement = context_measurement %>%
  rowwise() %>%
  mutate(
    freedom_dim_index_context_nr = min_fun(c(decision_freedom_context_nr, intermediate_freedom_context_nr, communication_freedom_context_nr, rights_freedom_context_nr,rule_settlement_freedom_context_nr)),
    equality_dim_index_context_nr = min_fun(c(decision_equality_context_nr, intermediate_equality_context_nr, communication_equality_context_nr, rights_equality_context_nr,rule_settlement_equality_context_nr)),
    control_dim_index_context_nr = min_fun(c(decision_control_context_nr, intermediate_control_context_nr, communication_control_context_nr, rights_control_context_nr,rule_settlement_control_context_nr)),
    
    decision_inst_index_context_nr = min_fun(c(decision_freedom_context_nr, decision_equality_context_nr, decision_control_context_nr)),
    intermediate_inst_index_context_nr = min_fun(c(intermediate_freedom_context_nr, intermediate_equality_context_nr, intermediate_control_context_nr)),
    communication_inst_index_context_nr = min_fun(c(communication_freedom_context_nr, communication_equality_context_nr, communication_control_context_nr)),
    rights_inst_index_context_nr = min_fun(c(rights_freedom_context_nr, rights_equality_context_nr, rights_control_context_nr)),
    rule_settlement_inst_index_context_nr = min_fun(c(rule_settlement_freedom_context_nr, rule_settlement_equality_context_nr, rule_settlement_control_context_nr)),
    
    total_index_context_nr = min_fun(c(freedom_dim_index_context_nr, equality_dim_index_context_nr, control_dim_index_context_nr)),
    
  ) %>% 
  ungroup()

# Classification
context_measurement$classification_context = classification(context_measurement, "context")

# Order Columns
context_measurement = context_measurement %>% 
  select(ordered_column_names(context_measurement))

# Cleaning
rm(broadband_indicators)