# Create PC Dataset

source_c("Institutions/PC/PC_F.R", "PC_F")
source_c("Institutions/PC/PC_E.R", "PC_E")
source_c("Institutions/PC/PC_C.R", "PC_C")

# See www.democracymatrix.com

PC = PC_F %>% 
  left_join(PC_E, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) %>% 
  left_join(PC_C, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) 


# Order Columns
PC = PC %>% 
  select(ordered_column_names(PC))

# Cleaning
rm(PC_F, PC_E, PC_C)
