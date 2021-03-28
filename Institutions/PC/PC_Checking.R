# Public Communication

source("Setup/Packages.R")
source("Setup/CheckFunctions.R")

source("Institutions/PC/PC_F.R")
source("Institutions/PC/PC_E.R")
source("Institutions/PC/PC_C.R")

# Freedom  ----

summary(PC_F)

# check zeros
which(PC_F$communication_freedom_core == 0)
which(PC_F$communication_freedom_core < 0.001)


# Missings
NA_plot(PC_F, "PC_F")

# Coders
Coders_plot(PC_F, "PC_F")

# Density for each component
Density_plot(PC_F, "PC_F")

# Boxplot for each component
Box_plot(PC_F, "PC_F")

#Summary Plots
Plot_Countries(PC_F, random= T)
Plot_Regions(PC_F)


# Equality  ----

summary(PC_E)

# check zeros
which(PC_E$communication_equality_core == 0)
which(PC_E$communication_equality_core < 0.001)


# Missings
NA_plot(PC_E, "PC_E")

# Coders
Coders_plot(PC_E, "PC_E")

# Density for each component
Density_plot(PC_E, "PC_E")

# Boxplot for each component
Box_plot(PC_E, "PC_E")

#Summary Plots
Plot_Countries(PC_E, random= T)
Plot_Regions(PC_E)

# Control  ----

summary(PC_C)

# check zeros
which(PC_C$communication_control_core == 0)
which(PC_C$communication_control_core < 0.001)


# Missings
NA_plot(PC_C, "PC_C")

# Coders
Coders_plot(PC_C, "PC_C")

# Density for each component
Density_plot(PC_C, "PC_C")

# Boxplot for each component
Box_plot(PC_C, "PC_C")

#Summary Plots
Plot_Countries(PC_C, random= T)
Plot_Regions(PC_C)


# Complete Dataset ----
source("Institutions/PC/PC_Dataset.R")

Plot_Countries(PC)
Plot_Regions(PC)
table_sum(PC)
