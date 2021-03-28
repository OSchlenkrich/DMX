source("Setup/Packages.R")
source("Datasets/CreateWebsiteData.R")

# website_data = fread("upload/website_data_v4.csv", encoding = "UTF-8")

# Set current (max) year (edition) 
current_year = max(website_data$year)

# World Map ----

create_world_map= function(dataset, selected_var, selected_year, label) {
  dmy_year = dataset %>% 
    filter(year==selected_year) %>% 
    select(country, variable = selected_var) %>%
    mutate(country = as.character(country))
  
  dmy_year$country[dmy_year$country=="Burma/Myanmar"] = "Burma"
  dmy_year$country[dmy_year$country=="Republic of Vietnam"] = "Vietnam"
  dmy_year$country[dmy_year$country=="São Tomé and Príncipe"] = "Sao Tome and Principe"
  
  merged_map_data <- joinCountryData2Map(dmy_year,
                                         joinCode = "NAME",
                                         nameJoinColumn = "country",
                                         verbose = TRUE)
  
  
  
  cnt = as.character(merged_map_data$NAME[merged_map_data$NAME != "Antarctica"])
  cnt = as.character(cnt[cnt != "Greenland"])
  
  merged_map_data <- subset(merged_map_data, NAME  %in%  cnt)
  
  values_pal = dmy_year$variable


  pal = colorNumeric(
    palette = c("darkred", "#DA4F33", "#E6E02C", "#307210"),
    domain = 0:1
  )
  
  library(RColorBrewer)
  
  colourPalette <- brewer.pal(10,'RdYlGn')
  
  mapParams = mapCountryData(merged_map_data,nameColumnToPlot="variable",colourPalette=colourPalette,catMethod=seq(0,1,0.1), 
                             addLegend = F, lwd=1,mapTitle = paste(label, selected_year))
  do.call( addMapLegend, c(mapParams, legendWidth=0.5,legendMar = 4,horiz=T))
  
  # leaflet(merged_map_data, options = leafletOptions(minZoom = 1, maxZoom = 5, dragging = T)) %>%
  #   addTiles()  %>%
  #   addPolygons(fillColor =  ~pal(variable),
  #               stroke = TRUE, opacity = 1, smoothFactor = 0.5, weight=1,
  #               color = "black", fillOpacity = 0.9,
  #               popup = paste(merged_map_data$NAME, ": ",round(merged_map_data$variable,2), sep="")) %>%
  #   addLegend("bottomleft", pal = pal, values = 0:1, opacity = 1, na.label = "n/a", 
  #             title=paste(label, selected_year, sep=" "), bins=c(0,0.25,0.5,0.75,1.00))
}

png("WebsiteMaterial/Plots/World_Map_Context_Total_de.png", width=20, height=15, units="cm", res=300)
create_world_map(website_data, "total_index_context", current_year, "Gesamtwertindex \n Kontextmessung \n")
dev.off()

png("WebsiteMaterial/Plots/World_Map_Context_Total_en.png", width=20, height=15, units="cm", res=300)
create_world_map(website_data, "total_index_context", current_year, "Total Value Index \n Context Measurement \n")
dev.off()



# Number of Regimes over Time ----
summary_dim_inst_context = website_data %>%
  group_by(year, classification_context) %>%
  summarise(Nr_Regime = n()) %>%
  ungroup() %>% 
  na.omit() %>% 
  mutate(classification_context = as.factor(classification_context),
         classification_context_de = fct_recode(classification_context, 
                                                "Harte Autokratie" = "1",
                                                "Moderate Autokratie" = "2",
                                                "Hybrides Regime" = "3",
                                                "Defizitäre Demokratie" = "4",
                                                "Funktionierende Demokratie"="5"),
         classification_context_en = fct_recode(classification_context, 
                                                "Hard Autocracy" = "1", 
                                                "Moderate Autocracy" = "2",
                                                "Hybrid Regime" = "3",
                                                "Deficient Demokratie" = "4",
                                                "Working Demokratie"="5"),
         classification_context_de = fct_relevel(classification_context_de,
                                                 "Funktionierende Demokratie",
                                                 "Defizitäre Demokratie",
                                                 "Hybrides Regime",
                                                 "Moderate Autokratie",
                                                 "Harte Autokratie"),
         classification_context_en = fct_relevel(classification_context_en, 
                                                 "Working Demokratie",
                                                 "Deficient Demokratie",
                                                 "Hybrid Regime",
                                                 "Moderate Autocracy",
                                                 "Hard Autocracy")
         
  )  %>%
  na.omit()



ggplot(summary_dim_inst_context, (aes(x=year, y=Nr_Regime, fill=classification_context_de))) + 
  geom_area(stat="identity", col="black") + 
  scale_x_continuous(breaks=seq(1900, 2020, 10)) + 
  scale_y_continuous(breaks=seq(0, 500, 10)) +
  scale_fill_manual(values=rev(brewer.pal(5,'RdYlGn'))) +
  coord_cartesian(expand=0) + 
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(title = "Anzahl der Regimetypen im Zeitverlauf",
       subtitle = "Regimeklassifizierung (Kontextmessung)",
       caption = "Datensatz der Demokratiematrix V4") + 
  ylab("Anzahl Regimetypen") + 
  xlab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust=0.5)) 

# Save
ggsave("WebsiteMaterial/Plots/Regimes_Time_de.png", device = "png", width=20, height=15, units="cm")


ggplot(summary_dim_inst_context, (aes(x=year, y=Nr_Regime, fill=classification_context_en))) + 
  geom_area(stat="identity", col="black") + 
  scale_x_continuous(breaks=seq(1900, 2020, 10)) + 
  scale_y_continuous(breaks=seq(0, 500, 10)) +
  scale_fill_manual(values=rev(brewer.pal(5,'RdYlGn'))) +
  coord_cartesian(expand=0) + 
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(title = "Number of Regimes over Time",
       subtitle = "Regime Classification (Context Measurement)",
       caption = "Dataset of the Democracy Matrix V4") + 
  ylab("Number of Regimes") + 
  xlab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust=0.5)) 

# Save
ggsave("WebsiteMaterial/Plots/Regimes_Time_en.png", device = "png", width=20, height=15, units="cm")


# World Map Categorical ----
create_world_map_cat= function(dataset, selected_var, selected_year, label = NULL, cat_label) {
  dmy_year = dataset %>% 
    filter(year==selected_year) %>% 
    select(country, variable = selected_var) %>%
    mutate(country = as.character(country))
  
  dmy_year$country[dmy_year$country=="Burma/Myanmar"] = "Burma"
  dmy_year$country[dmy_year$country=="Republic of Vietnam"] = "Vietnam"
  dmy_year$country[dmy_year$country=="São Tomé and Príncipe"] = "Sao Tome and Principe"
  
  merged_map_data <- joinCountryData2Map(dmy_year,
                                         joinCode = "NAME",
                                         nameJoinColumn = "country",
                                         verbose = TRUE)
  
  
  
  cnt = as.character(merged_map_data$NAME[merged_map_data$NAME != "Antarctica"])
  cnt = as.character(cnt[cnt != "Greenland"])
  
  merged_map_data <- subset(merged_map_data, NAME  %in%  cnt)
  
  values_pal = dmy_year$variable
  

  colourPalette <- rev(brewer.pal(5,'RdYlGn'))
  
  if (is.null(label) == T) {
    mapParams = mapCountryData(merged_map_data,
                               nameColumnToPlot="variable",
                               colourPalette=colourPalette,
                               catMethod="categorical", 
                               addLegend = T, 
                               #lwd=1,
                               mapTitle = "")    
  } else {
    mapParams = mapCountryData(merged_map_data,
                               nameColumnToPlot="variable",
                               colourPalette=colourPalette,
                               catMethod="categorical", 
                               addLegend = T, 
                               #lwd=1,
                               mapTitle = paste(label, selected_year))
  }

  
  do.call( addMapLegendBoxes, c(mapParams, title=cat_label))
  
}

png("WebsiteMaterial/Plots/World_Map_Context_Classification_de.png", width=25, height=15, units="cm", res=300)
create_world_map_cat(website_data %>% 
                       mutate(classification_context = as.factor(classification_context),
                              classification_context_de = fct_recode(classification_context, 
                                                                     "Harte Autokratie" = "1",
                                                                     "Moderate Autokratie" = "2",
                                                                     "Hybrides Regime" = "3",
                                                                     "Defizitäre Demokratie" = "4",
                                                                     "Funktionierende Demokratie"="5"),
                              classification_context_de = fct_relevel(classification_context_de,
                                                                      "Funktionierende Demokratie",
                                                                      "Defizitäre Demokratie",
                                                                      "Hybrides Regime",
                                                                      "Moderate Autokratie",
                                                                      "Harte Autokratie")
                       ),
                     "classification_context_de", current_year, "Regimeklassifikation \n Kontextmessung \n", "Regimetypen")
dev.off()




png("WebsiteMaterial/Plots/World_Map_Context_Classification_en.png", width=25, height=15, units="cm", res=300)
create_world_map_cat(website_data %>% 
                       #rworldmap doesn't know 'Eswatini'
                       mutate(country = ifelse(country == "Eswatini", "Swaziland", country),
                              country = ifelse(country == "North Macedonia", "Macedonia", country),
                              country = ifelse(country == "Democratic Republic of Vietnam", "Vietnam", country),
                              classification_context = ifelse(country == "Canada" & year == 2019, "Working Democracy", classification_context),
                              classification_context = ifelse(country == "Bahrain" & year == 2019, "Hard Autocracy", classification_context),
                              classification_context = ifelse(country == "Moldova" & year == 2019, "Deficient Democracy", classification_context)) %>% 
                       mutate(classification_context = as.factor(classification_context),
                              classification_context_en = fct_recode(classification_context, 
                                                                     "Hard Autocracy" = "1", 
                                                                     "Moderate Autocracy" = "2",
                                                                     "Hybrid Regime" = "3",
                                                                     "Deficient Democracy" = "4",
                                                                     "Working Democracy"="5"),
                              classification_context_en = fct_relevel(classification_context_en, 
                                                                      "Working Democracy", 
                                                                      "Deficient Democracy",
                                                                      "Hybrid Regime",
                                                                      "Moderate Autocracy",
                                                                      "Hard Autocracy")
                              ),
                     "classification_context_en", current_year, "Regime Classification \n Context Measurement \n", "Regimetypes")
dev.off()

# Report 2019
png("WebsiteMaterial/Plots/World_Map_Context_Classification_report_en.png", width=25, height=15, units="cm", res=300)
create_world_map_cat(website_data %>% 
                       #rworldmap doesn't know 'Eswatini'
                       mutate(country = ifelse(country == "Eswatini", "Swaziland", country),
                              country = ifelse(country == "North Macedonia", "Macedonia", country),
                              country = ifelse(country == "Democratic Republic of Vietnam", "Vietnam", country),
                              classification_context = ifelse(country == "Canada" & year == 2019, "Working Democracy", classification_context),
                              classification_context = ifelse(country == "Bahrain" & year == 2019, "Hard Autocracy", classification_context),
                              classification_context = ifelse(country == "Moldova" & year == 2019, "Deficient Democracy", classification_context)) %>% 
                       mutate(classification_context = as.factor(classification_context),
                              classification_context_en = fct_recode(classification_context, 
                                                                     "Hard Autocracy" = "1", 
                                                                     "Moderate Autocracy" = "2",
                                                                     "Hybrid Regime" = "3",
                                                                     "Deficient Democracy" = "4",
                                                                     "Working Democracy"="5"),
                              classification_context_en = fct_relevel(classification_context_en, 
                                                                      "Working Democracy", 
                                                                      "Deficient Democracy",
                                                                      "Hybrid Regime",
                                                                      "Moderate Autocracy",
                                                                      "Hard Autocracy")
                       ),
                     "classification_context_en", current_year, label = NULL, "Regimetypes")
dev.off()
