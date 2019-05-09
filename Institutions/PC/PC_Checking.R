# Public Communication

source("Setup/Packages.R")
source("Setup/CheckFunctions.R")

source("Institutions/PC/PC_F.R")
source("Institutions/PC/PC_E.R")
source("Institutions/PC/PC_C.R")

# Freedom  ----
# Missings
NA_plot(PC_F, "PC_F")

# Coders
Coders_plot(PC_F, "PC_F")

# Density for each component
Density_plot(PC_F, "PC_F")

# Boxplot for each component
Box_plot(PC_F, "PC_F")

#Summary Plots
Plot_Countries(PC_F)
Plot_Regions(PC_F)


# Equality  ----
# Missings
NA_plot(PC_E, "PC_E")

# Coders
Coders_plot(PC_E, "PC_E")

# Density for each component
Density_plot(PC_E, "PC_E")

# Boxplot for each component
Box_plot(PC_E, "PC_E")

#Summary Plots
Plot_Countries(PC_E)
Plot_Regions(PC_E)

# Control  ----
# Missings
NA_plot(PC_C, "PC_C")

# Coders
Coders_plot(PC_C, "PC_C")

# Density for each component
Density_plot(PC_C, "PC_C")

# Boxplot for each component
Box_plot(PC_C, "PC_C")

#Summary Plots
Plot_Countries(PC_C)
Plot_Regions(PC_C)


# Complete Dataset ----
source("Institutions/PC/PC_Dataset.R")

Plot_Countries(PC)
Plot_Regions(PC)
table_sum(PC)
