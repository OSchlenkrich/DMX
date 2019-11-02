# Create Regions Data

dem_matrix_regions = fread("Setup/Regions/demmatrix_regions.csv", encoding = "UTF-8") %>%
  mutate(regions = as.factor(regions),
         regions = fct_recode(regions, 
                              "Europe" = "1",
                              "North America incl. AUS+NZ" = "2",
                              "Latin America" = "3" ,
                              "Post-Soviet States (without EU-members)" = "4",
                              "Middle East and North Africa" = "5",
                              "Sub-Saharan Africa" = "6",
                              "South Asia" = "7",
                              "South-East Asia" = "8",
                              "East Asia" = "9",
                              "Small island states" = "10")
  )
