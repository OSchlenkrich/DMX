# Checking Measurement Levels

source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source("Setup/CheckFunctions.R")



# Core Measurement ----
source("Measurement_Levels/Core_Measurement.R")

NA_plot(core_measurement, "Indices", "index")
table_sum(core_measurement, "core")
table_sum(core_measurement, "index")

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

NA_plot(context_measurement, "Indices", "index_context")
table_sum(context_measurement, "context")

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

NA_plot(trade_off_measurement, "Indices", "index_trade_off")
table_sum(trade_off_measurement, "trade_off")

Density_plot(trade_off_measurement, "Trade-off-Measurement", "rights_control_trade_off")

create_plot("Germany", trade_off_measurement, "_trade_off")
create_plot("United Kingdom", trade_off_measurement, "_trade_off")
