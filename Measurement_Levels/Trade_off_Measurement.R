source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source("Setup/CheckFunctions.R")
source_c("Measurement_Levels/Context_Measurement.R", "context_measurement")

# Impact of Trade-off Factors on Core Measurement
scale_min = 0.25
lower_bound = 1-scale_min

# Trade-offs are only calculated for democracies
Democracies = context_measurement %>%
  select(country_name, year, classification_core) %>%
  mutate(IsDemocracy = if_else(classification_core == "Deficient Democracy" | classification_core == "Working Democracy", 1, 0))


# Trade-offs Procedurs of Decision ----

# Select Variables
PD_to = V_dem %>%
  select(country_name,
         year,
         v2x_elecreg, # Electoral regime index
         v2elfrfair, # Election free and fair
         
         elec_vote_1lp = v2ellovtlg, #Lower chamber vote share of largest vote-getter
         elec_vote_2lp = v2ellovtsm, #Lower chamber vote share of second-largest vote-getter
         elec_seat_1lp = v2ellostsl, # Lower chamber seat share won by largest party
         elec_seat_2lp = v2ellostss, #Lower chamber seat share won by second largest party
         comp_voting1 = v2elcomvot, #  Compulsory voting
         gender_quota = v2lgqugen, # Lower chamber gender quota threshold 
         DD_init = v2xdd_cic, # Citizen-initiated component of direct popular vote index
         ) %>%
  arrange(country_name, year) 


# Fill in Values between elections
PD_to = PD_to%>%
  group_by(country_name) %>%
  mutate_at(vars(starts_with("elec")), funs(fill_elections_w_ref(., v2x_elecreg, v2elfrfair))) %>%
  ungroup() 

# Select Only Democracies
PD_to = PD_to %>%
  left_join(Democracies, by = c("country_name", "year")) %>% 
  filter(IsDemocracy == 1) %>% 
  select(-IsDemocracy)

# Caculate Gallagher Index
PD_to = PD_to %>%
  mutate(Gallagher = sqrt(0.5 *((elec_vote_1lp - elec_seat_1lp)^2 + (elec_vote_2lp - elec_seat_2lp)^2)),
         Gallagher_quant = cut(Gallagher, quantile(Gallagher, c(0,0.25, 0.5, 0.75,1), na.rm=T)),
         Gallagher_quant = 4 - as.numeric(Gallagher_quant)
  )


# Build Binary version of gender_quota and comp_voting1
PD_to = PD_to %>%
  mutate(gender_quota_bin = if_else(gender_quota>=1, 1, 0),
         comp_voting1_bin = if_else(comp_voting1>=1, 1, 0)
         )

# Build Trade-off Components for Procedures of Decision
# Quota and Compulsory Voting are malus
PD_to = PD_to %>%
  mutate(
    decision_genderquota_trade_off_facto = minmax(gender_quota_bin, scale_min) + lower_bound,
    decision_compulsory_trade_off_facto = minmax(comp_voting1_bin, scale_min) + lower_bound,
    decision_gallagher_trade_off_facto = minmax(Gallagher_quant, scale_min) + lower_bound, 
    
    Gallagher_Bonus_Malus = if_else(decision_genderquota_trade_off_facto==1 & decision_compulsory_trade_off_facto == 1, decision_gallagher_trade_off_facto*0.6 + decision_genderquota_trade_off_facto*0.2 + decision_compulsory_trade_off_facto*0.2, if_else(decision_genderquota_trade_off_facto==1 | decision_compulsory_trade_off_facto == 1, decision_gallagher_trade_off_facto*0.8 + 1*0.2, decision_gallagher_trade_off_facto)),
    
    # Equality vs. Freedom
    decision_equality_trade_off_facto = Gallagher_Bonus_Malus,
    decision_freedom_trade_off_facto  = 1 - decision_equality_trade_off_facto  + lower_bound,
    
    # Control vs. Freedom of Rules Settlement
    decision_control_trade_off_facto = minmax(DD_init, scale_min) + lower_bound,
    decision_rules_settlement_freedom_trade_off_facto  = 1 - decision_control_trade_off_facto + lower_bound
  ) 



# Trade-offs of RI, PC and GR ----
# Select Variables

RI_PC_GR_to = V_dem %>%
  select(country_name,
         year,
         v2x_elecreg, # Electoral regime index
         v2elfrfair, # Election free and fair
         
         v2elpubfin, # Public campaign finance
         v2elpaidig, # Election paid interest group media
         v2elpdcamp, # Election paid campaign advertisements
         v2elfrcamp, # Election free campaign media
         v2jureview # Judicial review
         ) %>%
  arrange(country_name, year) 


# Fill in Values between elections
RI_PC_GR_to = RI_PC_GR_to %>%
  group_by(country_name) %>%
  mutate_at(vars(starts_with("v2el"), -matches("v2elfrfair")), funs(fill_elections_w_ref(., v2x_elecreg, v2elfrfair))) %>%
  ungroup() 

# Select Only Democracies
RI_PC_GR_to = RI_PC_GR_to %>%
  left_join(Democracies, by = c("country_name", "year")) %>% 
  filter(IsDemocracy == 1) %>% 
  select(-IsDemocracy)

# Build Trade-off Components for RI, PC and GR
RI_PC_GR_to = RI_PC_GR_to %>%
  mutate(
    # RI: Equality vs. Freedom 
    intermediate_equality_trade_off_facto = minmax(cdf(scale_fun(v2elpubfin)), scale_min) + lower_bound,
    intermediate_freedom_trade_off_facto = 1-intermediate_equality_trade_off_facto + lower_bound,
         
    # PC: Equality vs. Freedom 
    communication_equality_trade_off_facto  = minmax(cdf(scale_fun(v2elpaidig) * 0.2 + scale_fun(v2elpdcamp) * 0.4 +  scale_fun(v2elfrcamp) * 0.4), scale_min) + lower_bound,
    communication_freedom_trade_off_facto  = 1- communication_equality_trade_off_facto  + lower_bound,
    
    # GR: Control vs. Freedom (Rules Settlement) 
    rights_control_trade_off_facto  = minmax(cdf(scale_fun(v2jureview)), scale_min) + lower_bound,
    rules_settlement_rights_freedom_trade_off_facto = 1-rights_control_trade_off_facto  + lower_bound
  ) 



# Trade-offs of Rules Settlement ----

# Select Variables
RS_to = V_dem %>%
  select(country_name,
         year,
         v2x_elecreg, # Electoral regime index
         v2elfrfair, # Election free and fair
         
         v2lgdomchm, # Legislature dominant chamber
         v2elncbpr, # Effective number of cabinet parties
         v2x_divparctrl, # Divided party control index
         v2psnatpar_ord, # National party control
         v2lgbicam  # Legislature bicameral
         ) %>%
  arrange(country_name, year) 


# Fill in Values between elections
RS_to = RS_to %>%
  group_by(country_name) %>%
  mutate_at(vars(starts_with("v2el"), -matches("v2elfrfair")), funs(fill_elections_w_ref(., v2x_elecreg, v2elfrfair))) %>%
  ungroup() 


# Select Only Democracies
RS_to = RS_to %>%
  left_join(Democracies, by = c("country_name", "year")) %>% 
  filter(IsDemocracy == 1) %>% 
  select(-IsDemocracy)


# Setting upper bound of v2elncbpr to 4 and remove 0
RS_to = RS_to %>% 
  mutate(v2elncbpr = as.numeric(v2elncbpr),
         v2elncbpr = if_else(v2elncbpr > 4, 4, v2elncbpr),
         v2elncbpr = if_else(v2elncbpr == 0, NA_real_, v2elncbpr)
         )

# Ordering v2psnatpar_ord
RS_to = RS_to %>% 
  mutate(
    v2psnatpar_ord_ordered = as.factor(v2psnatpar_ord),
    v2psnatpar_ord_ordered = fct_recode(v2psnatpar_ord_ordered, 
                            "Unified Coalition" = "0",
                            "Divided party control" = "1",
                            "Unified party control" = "2"),
    # new order:
    v2psnatpar_ord_ordered = fct_relevel(v2psnatpar_ord_ordered,
                "Unified party control",
                "Unified Coalition",
                "Divided party control"),
    v2psnatpar_ord_ordered = as.numeric(v2psnatpar_ord_ordered) - 1
  )

# Join with trade-offs from other Institutions (PD and GR)
RS_to = RS_to %>% 
  left_join(PD_to %>% select(country_name, year, decision_rules_settlement_freedom_trade_off_facto),
            by= c("country_name", "year")
  ) %>% 
  left_join(RI_PC_GR_to %>% select(country_name, year, rules_settlement_rights_freedom_trade_off_facto),
            by= c("country_name", "year")
  )


# Build Trade-off Components for RS
RS_to = RS_to %>% 
  mutate(
    rules_settlement_numberparties_trade_off_facto = minmax(v2elncbpr, scale_min) + lower_bound,
    rules_settlement_concentration_trade_off_facto = minmax(v2psnatpar_ord_ordered, scale_min) + lower_bound,
    rules_settlement_divide_coaltion_trade_off_facto = rules_settlement_numberparties_trade_off_facto * 0.6 + rules_settlement_concentration_trade_off_facto * 0.4,
    
    rules_settlement_bicameralism_trade_off_facto = minmax(cdf(scale_fun(v2lgdomchm)), scale_min) + lower_bound,
    rules_settlement_bicameralism_trade_off_facto = if_else(v2lgbicam== 1, lower_bound, rules_settlement_bicameralism_trade_off_facto),
    
    # RS: Freedom vs. Control
    rules_settlement_control_trade_off_facto = rules_settlement_divide_coaltion_trade_off_facto * 0.6 + rules_settlement_bicameralism_trade_off_facto * 0.4,
    rules_settlement_freedom_trade_off_facto = (1 - rules_settlement_control_trade_off_facto + lower_bound) * 0.4 + decision_rules_settlement_freedom_trade_off_facto  * 0.2 + rules_settlement_rights_freedom_trade_off_facto * 0.4
  ) 


# Merge all datasets to trade-offs measurement----

trade_off_measurement = context_measurement %>% 
  left_join(PD_to, by=c("country_name", "year"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  left_join(RI_PC_GR_to, by=c("country_name", "year"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  left_join(RS_to, by=c("country_name", "year"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>%
  left_join(Democracies, by=c("country_name", "year"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y")))


# Aggregate Trade-off Components and Core Components
trade_off_measurement = trade_off_measurement %>%
  mutate(
    decision_freedom_trade_off = decision_freedom_core * decision_freedom_trade_off_facto,
    decision_equality_trade_off = decision_equality_core * decision_equality_trade_off_facto,
    decision_control_trade_off = decision_control_core * decision_control_trade_off_facto,
    
    intermediate_freedom_trade_off = intermediate_freedom_core * intermediate_freedom_trade_off_facto,
    intermediate_equality_trade_off = intermediate_equality_core * intermediate_equality_trade_off_facto,
    intermediate_control_trade_off = if_else(IsDemocracy == 0, NA_real_, intermediate_control_core),
    
    communication_freedom_trade_off = communication_freedom_core * communication_freedom_trade_off_facto,
    communication_equality_trade_off = communication_equality_core * communication_equality_trade_off_facto,
    communication_control_trade_off = if_else(IsDemocracy == 0, NA_real_, communication_control_core),
    
    rights_freedom_trade_off = if_else(IsDemocracy == 0, NA_real_, rights_freedom_core),
    rights_equality_trade_off = if_else(IsDemocracy == 0, NA_real_, rights_equality_core),
    rights_control_trade_off = rights_control_core * rights_control_trade_off_facto,
    
    rule_settlement_freedom_trade_off = rule_settlement_freedom_core * rules_settlement_freedom_trade_off_facto,
    rule_settlement_equality_trade_off = if_else(IsDemocracy == 0, NA_real_, rule_settlement_equality_core),
    rule_settlement_control_trade_off = rule_settlement_control_core * rules_settlement_control_trade_off_facto
  )  



# Calculate Dimensional and Institutional Values and Total Value
trade_off_measurement = trade_off_measurement %>%
  mutate(
    freedom_dim_index_trade_off =  (decision_freedom_trade_off * intermediate_freedom_trade_off * communication_freedom_trade_off * rights_freedom_trade_off * rule_settlement_freedom_trade_off)^(1/5),
    equality_dim_index_trade_off =  (decision_equality_trade_off * intermediate_equality_trade_off * communication_equality_trade_off * rights_equality_trade_off *rule_settlement_equality_trade_off)^(1/5),
    control_dim_index_trade_off =  (decision_control_trade_off * intermediate_control_trade_off * communication_control_trade_off * rights_control_trade_off *rule_settlement_control_trade_off)^(1/5),
    
    decision_inst_index_trade_off = (decision_freedom_trade_off * decision_equality_trade_off * decision_control_trade_off)^(1/3),
    intermediate_inst_index_trade_off = (intermediate_freedom_trade_off * intermediate_equality_trade_off * intermediate_control_trade_off)^(1/3),
    communication_inst_index_trade_off = (communication_freedom_trade_off * communication_equality_trade_off * communication_control_trade_off)^(1/3),
    rights_inst_index_trade_off = (rights_freedom_trade_off * rights_equality_trade_off * rights_control_trade_off)^(1/3),
    rule_settlement_inst_index_trade_off = (rule_settlement_freedom_trade_off * rule_settlement_equality_trade_off * rule_settlement_control_trade_off)^(1/3),
    total_index_trade_off = (freedom_dim_index_trade_off * equality_dim_index_trade_off * control_dim_index_trade_off)^(1/3)
  ) %>%
  select(-IsDemocracy)  %>% 
  arrange(country_name, year)

# Cleaning
rm(PD_to, RI_PC_GR_to, RS_to, Democracies)
