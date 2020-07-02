source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source("Setup/Regions/create_regions_data.R")

#--- Load V-Dem-Dataset and Extract Contemporary V-Dem ----

# Coppedge, Michael, John Gerring, Carl Henrik Knutsen, Staffan I. Lindberg, 
# Jan Teorell, David Altman, Michael Bernhard, M. Steven Fish, 
# Adam Glynn, Allen Hicken, Anna Lührmann, Kyle L. Marquardt, Kelly McMann, 
# Pamela Paxton, Daniel Pemstein, Brigitte Seim, Rachel Sigman, 
# Svend-Erik Skaaning, Jeffrey Staton, Steven Wilson, Agnes Cornell,
# Lisa Gastaldi, Haakon Gjerløw, Nina Ilchenko, Joshua Krusell, Laura Maxwell,
# Valeriya Mechkova, Juraj Medzihorsky, Josefine Pernes, Johannes von Römer, Natalia Stepanova, 
# Aksel Sundström, Eitan Tzelgov, Yi-ting Wang, Tore Wig, and Daniel Ziblatt. 2019. 
# "V-Dem [Country-Year/Country-Date] Dataset v9", Varieties of Democracy (V-Dem) Project. 
# https://doi.org/10.23696/vdemcy19


# Pemstein, Daniel, Kyle L. Marquardt, Eitan Tzelgov, Yi-ting Wang, Juraj Medzihorsky, 
# Joshua Krusell, Farhad Miri, and Johannes von Römer. 2019. “The V-Dem Measurement Model: 
# Latent Variable Analysis for Cross-National and Cross-Temporal Expert-Coded Data”, 
# V-Dem Working Paper No. 21. 4th edition. University of Gothenburg: 
# Varieties of Democracy Institute.

# Change Working Directory To Load Your Copy of the V-Dem-Dataset
V_dem_v9 = fread("unzip -p C:/RTest/V-Dem-CY-Full+Others-v9.zip", encoding = "UTF-8") %>%
  filter(project == 0 | project == 2) %>% 
  as_tibble()
V_dem = fread("unzip -p C:/RTest/V-Dem-CY-Full+Others-v10.zip", encoding = "UTF-8") %>%
  filter(project == 0 | project == 2) %>% 
  as_tibble()


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
         v2x_elecreg = replace(v2x_elecreg, country_name == "Peru" & year==1931, 1),
         v2x_elecreg = replace(v2x_elecreg, country_name == "Sudan" & year==1974, 1),
         
  )


# Matching Country Names



#unique(dem_matrix_regions$country_name)[which(unique(dem_matrix_regions$country_name) %!in% unique(V_dem$country_name))]
#unique(V_dem$country_name)[which(unique(V_dem$country_name) %!in% unique(dem_matrix_regions$country_name))]
V_dem$country_name[V_dem$country_name == "Democratic Republic of the Congo"] = "Democratic Republic of Congo"
V_dem$country_name[V_dem$country_name == "Vietnam"] = "Democratic Republic of Vietnam"


# Sweden: VDem codes Sweden's HOS (King) as having strong legislative power in 2019 (v2exdfpphs_ord)
# Barbados: VDem codes Barbaods's HOS (Queen) as having strong legislative power in 2019 (v2exdfpphs_ord)

V_dem = V_dem %>% 
  mutate(v2exdfpphs_ord = replace(v2exdfpphs_ord, country_name == "Sweden" & year==2019, 2),
         v2exdfpphs_ord = replace(v2exdfpphs_ord, country_name == "Barbados" & year==2019, 2),
         v2x_elecreg = replace(v2x_elecreg, country_name == "Burkina Faso" & year==2019, 1)
         )
# V_dem$v2exdfpphs_ord[V_dem$country_name == "Sweden"]
# V_dem$v2exdfpphs_ord[V_dem$country_name == "Barbados"]
# V_dem$v2x_elecreg[V_dem$country_name == "Burkina Faso"]

