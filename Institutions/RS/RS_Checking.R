# Rules Settlement

source("Setup/Packages.R")
source("Setup/CheckFunctions.R")

source("Institutions/RS/RS_F.R")
source("Institutions/RS/RS_E.R")
source("Institutions/RS/RS_C.R")

# Freedom  ----
summary(RS_F)

# check zeros
which(RS_F$rule_settlement_freedom_core == 0)
which(RS_F$rule_settlement_freedom_core < 0.001)

# Missings
NA_plot(RS_F, "RS_F")

# Coders
Coders_plot(RS_F, "RS_F")

# Density for each component
Density_plot(RS_F, "RS_F")

# Boxplot for each component
Box_plot(RS_F, "RS_F")

#Summary Plots
Plot_Countries(RS_F)
Plot_Regions(RS_F)


# Equality  ----
summary(RS_E)

# check zeros
which(RS_E$rule_settlement_equality_core == 0)
which(RS_E$rule_settlement_equality_core < 0.001)

# Missings
NA_plot(RS_E, "RS_E")

# Coders
Coders_plot(RS_E, "RS_E")

# Density for each component
Density_plot(RS_E, "RS_E")

# Boxplot for each component
Box_plot(RS_E, "RS_E")

#Summary Plots
Plot_Countries(RS_E)
Plot_Regions(RS_E)

# Control  ----
summary(RS_C)

# check zeros
which(RS_C$rule_settlement_control_core == 0)
which(RS_C$rule_settlement_control_core < 0.001)

# Missings
NA_plot(RS_C, "RS_C")

RS_C %>%
  select(v2lginvstp, v2lgbicam) %>%
  mutate(v2lgbicam = as.factor(v2lgbicam)) %>% 
  subset(is.na(v2lginvstp)) %>%
  mutate(NAs = 1) %>%
  group_by(v2lgbicam) %>%
  summarise(Nas_v2lginvstp = sum(NAs)) %>% 
  ggplot(aes(x=v2lgbicam, y=Nas_v2lginvstp)) + 
  geom_bar(stat="identity") +
  ylab("NAs of v2lginvstp") + 
  theme_bw() +
  ggtitle("Missings for v2lginvstp are explained by v2lgbicam") +
  theme(plot.title = element_text(hjust = 0.5))

RS_C %>%
  select(v2lgotovst, v2lgbicam) %>%
  mutate(v2lgbicam = as.factor(v2lgbicam)) %>% 
  subset(is.na(v2lgotovst)) %>%
  mutate(NAs = 1) %>%
  group_by(v2lgbicam) %>%
  summarise(Nas_v2lgotovst = sum(NAs)) %>% 
  ggplot(aes(x=v2lgbicam, y=Nas_v2lgotovst)) + 
  geom_bar(stat="identity") +
  ylab("NAs of v2lgotovst") + 
  theme_bw() +
  ggtitle("Missings for v2lgotovst are explained by v2lgbicam") +
  theme(plot.title = element_text(hjust = 0.5))

# Coders
Coders_plot(RS_C, "RS_C")

# Density for each component
Density_plot(RS_C, "RS_C")

# Boxplot for each component
Box_plot(RS_C, "RS_C")

#Summary Plots
Plot_Countries(RS_C)
Plot_Regions(RS_C)


# Complete Dataset ----
source("Institutions/RS/RS_Dataset.R")

Plot_Countries(RS)
Plot_Regions(RS)
table_sum(RS)
