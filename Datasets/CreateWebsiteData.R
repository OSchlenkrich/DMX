source("Setup/Packages.R")
source("Setup/BaseFunctions.R")
source("Setup/CheckFunctions.R")
source("Datasets/CreateDownloads.R")

# Create Specific Dataset for Website www.democracymatrix.com

# Numbering of regions 
dem_matrix_regions_web = dem_matrix_regions %>%
  mutate(
    regions = fct_recode(regions, 
                                   "01-Europe" = "Europe",
                                   "02-North America incl. AUS+NZ" = "North America incl. AUS+NZ",
                                   "03-Latin America" = "Latin America" ,
                                   "04-Post-Soviet States (without EU-members)" = "Post-Soviet States (without EU-members)",
                                   "05-Middle East and North Africa" = "Middle East and North Africa",
                                   "06-Sub-Saharan Africa" = "Sub-Saharan Africa",
                                   "07-South Asia" = "South Asia",
                                   "08-South-East Asia" = "South-East Asia",
                                   "09-East Asia" = "East Asia",
                                   "10-Small island states" = "Small island states")
  ) %>%
  select(country = country_name, regions)

 

# Core Measurement ----
core_measurement_website = DemocracyMatrix_incl_VDem %>% 
  select(-regions) %>% 
  left_join(dem_matrix_regions_web, by=c("country")) %>%
  select_at(vars(country, 
                 year, 
                 regions, 
                 ends_with("_core"))
  )  %>% 
  arrange(country, year)

# Mean core values for all regions
regions_core = core_measurement_website %>%
  select_at(vars(-starts_with("classification"))) %>%
  group_by(regions, year) %>%
  summarise_at(vars(ends_with("core")), funs(mean(., na.rm=T))) %>%
  filter(regions!="") %>%
  rename(country = regions)

# Mean global core values
global_core = core_measurement_website  %>%
  select_at(vars(-starts_with("classification"))) %>%
  group_by(year) %>%
  summarise_at(vars(ends_with("core")), funs(mean(., na.rm=T)))
global_core = cbind(country="Global", global_core)

# Combining regions and global
core_measurement_website = core_measurement_website %>%
  select(-regions) %>%
  bind_rows(regions_core) %>%
  bind_rows(global_core)


# Context Measurement ----

context_measurement_website = DemocracyMatrix_incl_VDem %>% 
  select(-regions) %>%  
  left_join(dem_matrix_regions_web, by=c("country")) %>%
  select_at(vars(country, 
                 year, 
                 regions, 
                 ends_with("_context"))
  )  %>% 
  arrange(country, year)

# Mean context values for all regions
regions_context = context_measurement_website %>%
  select_at(vars(-starts_with("classification"))) %>%
  group_by(regions, year) %>%
  summarise_at(vars(ends_with("_context")), funs(mean(., na.rm=T))) %>%
  filter(regions!="") %>%
  rename(country = regions)

# Mean global context values
global_context = context_measurement_website %>%
  select_at(vars(-starts_with("classification"))) %>%
  group_by(year) %>%
  summarise_at(vars(ends_with("_context")), funs(mean(., na.rm=T)))
global_context = cbind(country="Global", global_context)

# Combining regions and global
context_measurement_website = context_measurement_website %>%
  select(-regions) %>%
  bind_rows(regions_context) %>%
  bind_rows(global_context)


# Trade-Off-Measurement ----

trade_off_measurement_website = DemocracyMatrix_incl_VDem %>% 
  select(-regions) %>%  
  left_join(dem_matrix_regions_web, by=c("country")) %>%
  select_at(vars(country, 
                 year, 
                 regions, 
                 ends_with("_trade_off"))
  )  %>% 
  arrange(country, year)

# Mean trade-off values for all regions
regions_trade_off = trade_off_measurement_website %>%
  group_by(regions, year) %>%
  summarise_at(vars(ends_with("_trade_off")), funs(mean(., na.rm=T))) %>%
  filter(regions!="") %>%
  rename(country = regions)

# Mean global trade-off values
global_trade_off = trade_off_measurement_website %>%
  group_by(year) %>%
  summarise_at(vars(ends_with("_trade_off")), funs(mean(., na.rm=T)))
global_trade_off = cbind(country="Global", global_trade_off)

# Combining regions and global
trade_off_measurement_website = trade_off_measurement_website %>%
  select(-regions) %>%
  bind_rows(regions_trade_off) %>%
  bind_rows(global_trade_off)

# Main Website Data #----
website_data = core_measurement_website %>%
  left_join(context_measurement_website, by=c("country", "year")) %>%
  left_join(trade_off_measurement_website, by=c("country", "year")) 


# IMPORTANT: Create Frame to fill up all years for all countries
# Prerequisite for Website
make_frame = function()  {
  lengthyear = length(unique(website_data$year))
  minyear = min(unique(website_data$year), na.rm=T)
  maxyear = max(unique(website_data$year), na.rm=T)
  
  frame_year_append_df = data.frame(country=rep(unique(website_data$country), each=lengthyear), 
                              year=rep(seq(minyear,maxyear,1), length(unique(website_data$country))))
  
  frame_year_append_df = dem_matrix_regions_web %>%
    select(country, regions) %>%
    group_by(country) %>%
    distinct() %>%
    na.omit() %>%
    right_join(frame_year_append_df, by="country")
  
  return(frame_year_append_df)
}

year_append_df = make_frame()

selection = website_data %>% 
  select(-country, -year) %>% 
  colnames(.)


website_data = website_data %>%
  left_join(year_append_df, by=c("country", "year")) %>%
  arrange(country, year)  %>%
  select(country, year, regions, selection) %>%
  mutate(classification_core = if_else(classification_core == "Autocracy", 1,
                                       if_else(classification_core == "Hybrid Regime", 2,
                                               if_else(classification_core == "Deficient Democracy", 3,4)))
  ) %>%
  mutate(classification_context = if_else(classification_context == "Autocracy", 1,
                                          if_else(classification_context == "Hybrid Regime", 2,
                                                  if_else(classification_context == "Deficient Democracy", 3,4)))
  )


write.csv(website_data, "upload/website_data_v1_1.csv", fileEncoding = "UTF-8", na = "", row.names = F)
write.csv(website_data, "ShinyApp/website_data_v1_1.csv", fileEncoding = "UTF-8", na = "", row.names = F)




