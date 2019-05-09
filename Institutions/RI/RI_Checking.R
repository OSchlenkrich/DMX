# Regulation of the Intermediate Sphere

source("Setup/Packages.R")
source("Setup/CheckFunctions.R")

source("Institutions/RI/RI_F.R")
source("Institutions/RI/RI_E.R")
source("Institutions/RI/RI_C.R")

# Freedom  ----
# Missings
NA_plot(RI_F, "RI_F")

RI_F %>%
  select(v2psoppaut, v2psbars_ord) %>%
  mutate(v2psbars_ord = as.factor(v2psbars_ord),
         v2psbars_ord = fct_recode(v2psbars_ord, 
                                      "No Parties allowed" = "0",
                                      "Impossible" = "1",
                                      "Significant obstacles" = "2",
                                      "Modest obstacles" = "3",
                                      "No barriers" = "4")
  ) %>% 
  subset(is.na(v2psoppaut)) %>%
  mutate(NAs = 1) %>%
  group_by(v2psbars_ord) %>%
  summarise(Nas_v2psoppaut = sum(NAs)) %>% 
  ggplot(aes(x=v2psbars_ord, y=Nas_v2psoppaut)) + 
  geom_bar(stat="identity") +
  ylab("NAs of v2psoppaut") + 
  theme_bw() +
  ggtitle("Missings for v2psoppaut are explained by v2psbars_ord") +
  theme(plot.title = element_text(hjust = 0.5))



# Coders
Coders_plot(RI_F, "RI_F")

# Density for each component
Density_plot(RI_F, "RI_F")

# Boxplot for each component
Box_plot(RI_F, "RI_F")

#Summary Plots
Plot_Countries(RI_F)
Plot_Regions(RI_F)


# Equality  ----
# Missings
NA_plot(RI_E, "RI_E")

# Coders
Coders_plot(RI_E, "RI_E")

# Density for each component
Density_plot(RI_E, "RI_E")

# Boxplot for each component
Box_plot(RI_E, "RI_E")

#Summary Plots
Plot_Countries(RI_E)
Plot_Regions(RI_E)


# Control  ----
# Missings
NA_plot(RI_C, "RI_C")

RI_C %>%
  select(v2lgoppart, v2lgbicam) %>%
  mutate(v2lgbicam = as.factor(v2lgbicam)) %>% 
  subset(is.na(v2lgoppart)) %>%
  mutate(NAs = 1) %>%
  group_by(v2lgbicam) %>%
  summarise(Nas_v2lgoppart = sum(NAs)) %>% 
  ggplot(aes(x=v2lgbicam, y=Nas_v2lgoppart)) + 
  geom_bar(stat="identity") +
  ylab("NAs of v2lgoppart") + 
  theme_bw() +
  ggtitle("Missings for v2lgoppart are explained by v2lgbicam") +
  theme(plot.title = element_text(hjust = 0.5))


# Coders
Coders_plot(RI_C, "RI_C")

# Density for each component
Density_plot(RI_C, "RI_C")

# Boxplot for each component
Box_plot(RI_C, "RI_C")

#Summary Plots
Plot_Countries(RI_C)
Plot_Regions(RI_C)


# Complete Dataset ----
source("Institutions/RI/RI_Dataset.R")

Plot_Countries(RI)
Plot_Regions(RI)
table_sum(RI)
