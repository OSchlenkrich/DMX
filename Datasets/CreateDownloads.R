source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source("Setup/CheckFunctions.R")
source("Setup/Regions/create_regions_data.R")
source_c("Measurement_Levels/Context_Measurement.R", "context_measurement")
source_c("Measurement_Levels/Trade_off_Measurement.R", "trade_off_measurement")



# Create Democracy Matrix Dataset incl. V-Dem ----
DemocracyMatrix_incl_VDem = context_measurement %>% 
  left_join(trade_off_measurement, by=c("country_name", "year"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>% 
  left_join(dem_matrix_regions, by=c("country_name")) %>%
  select_at(vars(country = country_name, 
                 year, 
                 regions, 
                 setdiff(starts_with("v2"), ends_with("_nr")),
                 ends_with("_facto"),
                 ends_with("_core"),
                 ends_with("_context"),
                 ends_with("_trade_off"),
                 ends_with("_nr")
    )
  ) %>%
  select_at(vars(-matches("oD"), -matches("tran"), -ends_with("fact"), -ends_with("_nr_ord"))) %>%
  rename_at(vars(starts_with("v2")), funs(sub('v2', 'vdem_v2', .))) 


write.csv(DemocracyMatrix_incl_VDem, "upload/DemocracyMatrix_v4_incl_VDem.csv", 
          fileEncoding = "UTF-8", 
          # na = "", 
          row.names = F)
zip::zipr(zipfile = 'upload/DemocracyMatrix_v4_incl_VDem.zip', files = 'upload/DemocracyMatrix_v4_incl_VDem.csv',
          include_directories = F)

# Create Democracy Matrix Dataset with base variables ----
Democracy_Matrix_Small = context_measurement %>% 
  left_join(trade_off_measurement, by=c("country_name", "year"), suffix = c("", ".y")) %>%
  select_at(vars(-ends_with(".y"))) %>% 
  left_join(dem_matrix_regions, by=c("country_name")) %>%
  select_at(vars(country = country_name, 
                 year, 
                 regions, 
                 ends_with("_core"),
                 ends_with("_context"),
                 ends_with("_trade_off")
    )
  ) %>%
  select_at(vars(-matches("oD"), -matches("tran"), -ends_with("fact"), -ends_with("_nr_ord"))) %>%
  rename_at(vars(starts_with("v2")), funs(sub('v2', 'vdem_v2', .)))


write.csv(Democracy_Matrix_Small, "upload/DemocracyMatrix_v4.csv", 
          fileEncoding = "UTF-8", 
          #na = "", 
          row.names = F)
zip::zipr(zipfile = 'upload/DemocracyMatrix_v4.zip', files = 'upload/DemocracyMatrix_v4.csv',
          include_directories = F)
