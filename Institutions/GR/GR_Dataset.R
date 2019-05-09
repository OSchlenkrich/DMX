# Create GR Dataset

source_c("Institutions/GR/GR_F.R", "GR_F")
source_c("Institutions/GR/GR_E.R", "GR_E")
source_c("Institutions/GR/GR_C.R", "GR_C")

# See www.democracymatrix.com

GR = GR_F %>% 
  left_join(GR_E, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) %>% 
  left_join(GR_C, by=c("country_name", "year"), suffix = c("",".y")) %>%
  select_at(vars(-ends_with(".y"))) 


# Order Columns
GR = GR %>% 
  select(ordered_column_names(GR))

# Cleaning
rm(GR_F, GR_E, GR_C)
