# Create PD Dataset

source_c("Institutions/PD/PD_F.R", "PD_F")
source_c("Institutions/PD/PD_E.R", "PD_E")
source_c("Institutions/PD/PD_C.R", "PD_C")

# See www.democracymatrix.com

PD = PD_F %>% 
  left_join(PD_E, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) %>% 
  left_join(PD_C, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) 

# Order Columns
PD = PD %>% 
  select(ordered_column_names(PD))

# Cleaning
rm(PD_F, PD_E, PD_C)
