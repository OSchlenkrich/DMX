source("Setup/Packages.R")
source("Datasets/CreateWebsiteData.R")

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

png("WebsiteMaterial/World_Map_Context_Total_de.png", width=20, height=15, units="cm", res=300)
create_world_map(website_data, "total_index_context", current_year, "Gesamtwertindex \n Kontextmessung \n")
dev.off()

png("WebsiteMaterial/World_Map_Context_Total_en.png", width=20, height=15, units="cm", res=300)
create_world_map(website_data, "total_index_context", current_year, "Total Value Index \n Context Measurement \n")
dev.off()

# Number of Regimes over Time ----
summary_dim_inst_context = website_data %>%
  group_by(year, classification_context) %>%
  summarise(Nr_Regime = n()) %>%
  na.omit() %>% 
  mutate(classification_context = as.factor(classification_context),
         classification_context_de = fct_recode(classification_context, 
                                                "Autokratie" = "1",
                                                "Hybrides Regime" = "2",
                                                "Defizitäre Demokratie" = "3",
                                                "Funktionierende Demokratie"="4"),
         classification_context_en = fct_recode(classification_context, 
                                                "Autocracy" = "1",
                                                "Hybrid Regime" = "2",
                                                "Deficient Demokratie" = "3",
                                                "Working Demokratie"="4"),
         
  )  %>%
  na.omit()



ggplot(summary_dim_inst_context, (aes(x=year, y=Nr_Regime, col=classification_context_de))) + 
  geom_line(size=1.1) + scale_x_continuous(breaks=seq(1900, 2020, 10)) + scale_y_continuous(breaks=seq(0, 150, 10)) + theme(axis.text.x = element_text(angle=90), legend.title = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(title = "Anzahl der Regimetypen im Zeitverlauf",
       subtitle = "Regimeklassifizierung (Kontextmessung)",
       caption = "Datensatz der Demokratiematrix V2") + 
  ylab("Anzahl Regimetypen") + 
  xlab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust=0.5)) 

# Save
ggsave("WebsiteMaterial/Regimes_Time_de.png", device = "png", width=20, height=15, units="cm")


ggplot(summary_dim_inst_context, (aes(x=year, y=Nr_Regime, col=classification_context_en))) + 
  geom_line(size=1.1) + scale_x_continuous(breaks=seq(1900, 2020, 10)) + scale_y_continuous(breaks=seq(0, 150, 10)) + theme(axis.text.x = element_text(angle=90), legend.title = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust=0.5)) + 
  labs(title = "Number of Regimes over Time",
       subtitle = "Regime Classification (Context Measurement)",
       caption = "Dataset of the Democracy Matrix V2") + 
  ylab("Number of Regimes") + 
  xlab("") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.title = element_blank(), plot.subtitle = element_text(hjust=0.5)) 

# Save
ggsave("WebsiteMaterial/Regimes_Time_en.png", device = "png", width=20, height=15, units="cm")
