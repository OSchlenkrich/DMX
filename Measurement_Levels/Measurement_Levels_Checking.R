# Checking Measurement Levels

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source("Setup/CheckFunctions.R")



# Core Measurement ----
source("Measurement_Levels/Core_Measurement.R")

# Check for zeros
which(core_measurement$freedom_dim_index_core==0)
which(core_measurement$equality_dim_index_core==0)
which(core_measurement$control_dim_index_core==0)

which(core_measurement$decision_inst_index_core==0)
which(core_measurement$intermediate_inst_index_core==0)
which(core_measurement$communication_inst_index_core==0)
which(core_measurement$rights_inst_index_core==0)
which(core_measurement$rule_settlement_inst_index_core==0)


# Missings
# V10: 2400 obs in total index core are missing
# V11: 2500 obs in total index core are missing
NA_plot(core_measurement, "Indices: Core Measurement", "index")
NA_year_plot(core_measurement, "Indices: Core Measurement", "index")

# Summary Tables
table_sum(core_measurement, "core")
table_sum(core_measurement, "core", rounding = T)

table_sum(core_measurement, "index")
table_sum(core_measurement, "index", rounding = T)

core_measurement %>%
  group_by(year, classification_core) %>%
  summarise(Nr_Regime = n()) %>%
  na.omit() %>% 
  ggplot((aes(x=year, y=Nr_Regime, col=classification_core))) + 
  geom_line(size=1) + theme_bw() + scale_x_continuous(breaks=seq(1900, 2020, 10)) + 
  scale_y_continuous(breaks=seq(0, 150, 10)) + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90), 
        legend.title = element_blank(), axis.title.x = element_blank()) + 
  ggtitle("Number of Regime Types over Time") + 
  ylab("Number of Regime Types")


create_plot("Germany", core_measurement, "_core")

# Context Measurement ----
source("Measurement_Levels/Context_Measurement.R")


# Check for zeros
which(context_measurement$freedom_dim_index_context==0)
which(context_measurement$equality_dim_index_context==0)
which(context_measurement$control_dim_index_context==0)

which(context_measurement$decision_inst_index_context==0)
which(context_measurement$intermediate_inst_index_context==0)
which(context_measurement$communication_inst_index_context==0)
which(context_measurement$rights_inst_index_context==0)
which(context_measurement$rule_settlement_inst_index_context==0)

# Missings
# V11: 2500 obs in total index context are missing
NA_plot(context_measurement, "Indices: Context Measurement", "index_context")
NA_year_plot(context_measurement, "Indices: Context Measurement", "index")

# Summary Tables
table_sum(context_measurement, "context")
table_sum(context_measurement, "context", rounding = T)

table_sum(context_measurement, "index")
table_sum(context_measurement, "index", rounding = T)


context_measurement %>%
  group_by(year, classification_context) %>%
  summarise(Nr_Regime = n()) %>%
  na.omit() %>% 
  ggplot((aes(x=year, y=Nr_Regime, col=classification_context))) + 
  geom_line(size=1) + theme_bw() + scale_x_continuous(breaks=seq(1900, 2020, 10)) + 
  scale_y_continuous(breaks=seq(0, 150, 10)) + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90), 
        legend.title = element_blank(), axis.title.x = element_blank()) + 
  ggtitle("Number of Regime Types over Time") + 
  ylab("Number of Regime Types")

create_plot("Germany", context_measurement, "_context")


# Trade-off Measurement ----
source("Measurement_Levels/Trade_off_Measurement.R")
# V11: 15000 obs in total index tradeoff are missing
NA_plot(trade_off_measurement, "Indices: Trade-Off Measurement", "index_trade_off")
NA_year_plot(trade_off_measurement, "Indices: Trade-Off Measurement", "index")

# V11: 800 obs in total index tradeoff are missing
NA_plot(trade_off_measurement %>% 
          filter(classification_core == "Working Democracy" | classification_core == "Deficient Democracy"), 
        "Indices: Trade-Off Measurement (Only Democracies)", "index_trade_off")
NA_year_plot(trade_off_measurement %>% 
          filter(classification_core == "Working Democracy" | classification_core == "Deficient Democracy"), 
        "Indices: Trade-Off Measurement (Only Democracies)", "index_trade_off")

# Summary Tables
table_sum(trade_off_measurement, "trade_off")
table_sum(trade_off_measurement, "trade_off", rounding = T)

Density_plot(trade_off_measurement, "Trade-off-Measurement", "rights_control_trade_off")

create_plot("Germany", trade_off_measurement, "_trade_off")
create_plot("United Kingdom", trade_off_measurement, "_trade_off")
