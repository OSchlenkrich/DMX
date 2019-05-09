# Guarantee of Rights

source("Setup/Packages.R")
source("Setup/CheckFunctions.R")

source("Institutions/GR/GR_F.R")
source("Institutions/GR/GR_E.R")
source("Institutions/GR/GR_C.R")

# Freedom  ----
# Missings
NA_plot(GR_F, "GR_F")

# Coders
Coders_plot(GR_F, "GR_F")

# Density for each component
Density_plot(GR_F, "GR_F")

# Boxplot for each component
Box_plot(GR_F, "GR_F")

#Summary Plots
Plot_Countries(GR_F)
Plot_Regions(GR_F)


# Equality  ----
# Missings
NA_plot(GR_E, "GR_E")

# Coders
Coders_plot(GR_E, "GR_E")

# Density for each component
Density_plot(GR_E, "GR_E")

# Boxplot for each component
Box_plot(GR_E, "GR_E")

#Summary Plots
Plot_Countries(GR_E)
Plot_Regions(GR_E)


# Control  ----
# Missings
NA_plot(GR_C, "GR_C")

# Coders
Coders_plot(GR_C, "GR_C")

# Density for each component
Density_plot(GR_C, "GR_C")

# Boxplot for each component
Box_plot(GR_C, "GR_C")

#Summary Plots
Plot_Countries(GR_C)
Plot_Regions(GR_C)


# Complete Dataset ----
source("Institutions/GR/GR_Dataset.R")

Plot_Countries(GR)
Plot_Regions(GR)
table_sum(GR)
