# Procedures of Decision

source("Setup/Packages.R")
source("Setup/CheckFunctions.R")

source("Institutions/PD/PD_F.R")
source("Institutions/PD/PD_E.R")
source("Institutions/PD/PD_C.R")

# Freedom  ----
summary(PD_F)

# check zeros
which(PD_F$decision_freedom_core == 0)
which(PD_F$decision_freedom_core < 0.001)

# Missings
NA_plot(PD_F, "PD_F")

# Coders
Coders_plot(PD_F, "PD_F")

# Density for each component
Density_plot(PD_F, "PD_F")

# Boxplot for each component
Box_plot(PD_F, "PD_F")

#Summary Plots

Plot_Countries(PD_F)
Plot_Regions(PD_F)


# Equality  ----

summary(PD_E)

# check zeros
which(PD_E$decision_equality_core == 0)
which(PD_E$decision_equality_core < 0.001)

# Missings
NA_plot(PD_E, "PD_E")

# Coders
Coders_plot(PD_E, "PD_E")

# Density for each component
Density_plot(PD_E, "PD_E")

# Boxplot for each component
Box_plot(PD_E, "PD_E")

#Summary Plots
Plot_Countries(PD_E)
Plot_Regions(PD_E)

# Control  ----

summary(PD_C)

# check zeros
which(PD_C$decision_control_core == 0)
which(PD_C$decision_control_core < 0.001)

# Missings
NA_plot(PD_C, "PD_C")

PD_C %>% 
  select(year, decision_civilmonitors_facto) %>% 
  group_by(year) %>% 
  summarise_all(function(x) sum(is.na(x))) %>% 
  ungroup() %>% 
  ggplot(aes(x=year, y=decision_civilmonitors_facto)) +
  geom_bar(stat="identity", width=1) +
  scale_x_continuous(name="", breaks=seq(1900, 2020, 10)) + 
  ylab("Number of NAs") + 
  ggtitle(paste("decision_civilmonitors_facto", "- Missings for each year")) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90), plot.title = element_text(hjust = 0.5)) 

# Coders
Coders_plot(PD_C, "PD_C")

# Density for each component
Density_plot(PD_C, "PD_C")

# Boxplot for each component
Box_plot(PD_C, "PD_C")

#Summary Plots
Plot_Countries(PD_C)
Plot_Regions(PD_C)


# Complete Dataset ----
source("Institutions/PD/PD_Dataset.R")

Plot_Countries(PD)
Plot_Regions(PD)
table_sum(PD)