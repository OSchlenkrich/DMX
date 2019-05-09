# Create RS Dataset

source_c("Institutions/RS/RS_F.R", "RS_F")
source_c("Institutions/RS/RS_E.R", "RS_E")
source_c("Institutions/RS/RS_C.R", "RS_C")

# See www.democracymatrix.com

RS = RS_F %>% 
  left_join(RS_E, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) %>% 
  left_join(RS_C, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) 


# Order Columns
RS = RS %>% 
  select(ordered_column_names(RS))

# Cleaning
rm(RS_F, RS_E, RS_C)
