# Create RI Dataset

source_c("Institutions/RI/RI_F.R", "RI_F")
source_c("Institutions/RI/RI_E.R", "RI_E")
source_c("Institutions/RI/RI_C.R", "RI_C")

# See www.democracymatrix.com

RI = RI_F %>% 
  left_join(RI_E, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) %>% 
  left_join(RI_C, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) 


# Order Columns
RI = RI %>% 
  select(ordered_column_names(RI))

# Cleaning
rm(RI_F, RI_E, RI_C)
