source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source("Setup/Regions/create_regions_data.R")

#--- Load V-Dem-Dataset and Extract Contemporary V-Dem ----

# Coppedge, Michael, John Gerring, Carl Henrik Knutsen, 
# Staffan I. Lindberg, Svend-Erik Skaaning, Jan Teorell, David Altman, 
# Michael Bernhard, M. Steven Fish, Agnes Cornell, Sirianne Dahlum, Haakon Gjerløw, 
# Adam Glynn, Allen Hicken, Joshua Krusell, Anna Lührmann, Kyle L. Marquardt, 
# Kelly McMann, Valeriya Mechkova, Juraj Medzihorsky, Moa Olin, Pamela Paxton, 
# Daniel Pemstein, Josefine Pernes, Johannes von Römer, Brigitte Seim, Rachel Sigman, 
# Jeffrey Staton, Natalia Stepanova, Aksel Sundström, Eitan Tzelgov, Yi-ting Wang, 
# Tore Wig, Steven Wilson, and Daniel Ziblatt. 2018. "V-Dem [Country-Year/Country-Date] Dataset v8". 
# Varieties of Democracy (V-Dem) Project. https://doi.org/10.23696/vdemcy18


# Pemstein, Daniel, Kyle L. Marquardt, Eitan Tzelgov, Yi-ting Wang, Joshua Krusell and 
# Farhad Miri. 2018. “The V-Dem Measurement Model: Latent Variable Analysis for 
# Cross-National and Cross-Temporal Expert-Coded Data”. University of Gothenburg, 
# Varieties of Democracy Institute: Working Paper No. 21, 3d edition.

V_dem = fread("C:/RTest/V-Dem-CY+Others-v8.csv", encoding = "UTF-8") %>%
  filter(project == 0 | project == 2)


#--- Some Data Cleaning ----

# elections of some countries are coded even though v2x_elecreg == 0 (meaning there is no election)
table(V_dem$v2x_elecreg, is.na(V_dem$v2elfrfair==T))

V_dem %>% 
  select(country_name, year, v2x_elecreg, v2elfrfair) %>% 
  filter(v2x_elecreg == 0, is.na(v2elfrfair) == F)

# v2x_elecreg == 1 for these countries
V_dem = V_dem %>%
  mutate(v2x_elecreg = replace(v2x_elecreg, country_name == "Syria" & year==2014, 1),
         v2x_elecreg = replace(v2x_elecreg, country_name == "Syria" & year==2016, 1),
         v2x_elecreg = replace(v2x_elecreg, country_name == "Libya" & year==2014, 1),
         v2x_elecreg = replace(v2x_elecreg, country_name == "Peru" & year==1931, 1)
  )


# Matching Country Names

#unique(dem_matrix_regions$country_name)[which(unique(dem_matrix_regions$country_name) %!in% unique(V_dem$country_name))]
#unique(V_dem$country_name)[which(unique(V_dem$country_name) %!in% unique(dem_matrix_regions$country_name))]

V_dem$country_name[V_dem$country_name == "Democratic Republic of the Congo"] = "Democratic Republic of Congo"
V_dem$country_name[V_dem$country_name == "Vietnam"] = "Democratic Republic of Vietnam"


