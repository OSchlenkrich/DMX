source("Setup/Packages.R")
source("Datasets/CreateTypo3Identifier.R")


# identifier for translation in EN and DE
identification_classification_plots = typo3_identification_classification %>% 
  select(-class_identifier, classification_context = "EN", DE_Klassifikation="DE")
identification_plots = typo3_identification %>% 
  select(country=country_identifier, EN_country = "EN", DE_Land="DE")

# Set current (max) year (edition) 
current_year = max(Democracy_Matrix_Small$year) 


# Ranking ####

Democracy_Matrix_Small %>% 
  filter(year==current_year) %>% 
  select(country, total_index_context, classification_context) %>% 
  na.omit() %>% 
  arrange(-total_index_context) %>% 
  mutate(total_index_context = round(total_index_context,3),
         rank = 1:length(total_index_context),
         total_index_context = as.character(total_index_context),
         total_index_context = if_else(total_index_context=="0", "O", total_index_context)
         ) %>% 
  left_join(identification_classification_plots, by="classification_context") %>% 
  left_join(identification_plots, by="country") %>% 
  select(Rang=rank, Land=DE_Land, Gesamtwertindex=total_index_context,Klassifikation=DE_Klassifikation) %>% 
  write.csv("WebsiteMaterial/Ranking_DE.csv", row.names = F, fileEncoding = "UTF-8")

Democracy_Matrix_Small %>% 
  filter(year==current_year) %>% 
  select(country, total_index_context,classification_context) %>% 
  na.omit() %>% 
  arrange(-total_index_context) %>% 
  mutate(total_index_context = round(total_index_context,3),
         rank = 1:length(total_index_context),
         total_index_context = as.character(total_index_context),
         total_index_context = if_else(total_index_context=="0", "O", total_index_context)
  ) %>% 
  left_join(identification_plots, by="country") %>% 
  select(Rank=rank, Country=EN_country, "Total Value Index" =total_index_context, 
         Classification=classification_context) %>% 
  write.csv("WebsiteMaterial/Ranking_EN.csv", row.names = F, fileEncoding = "UTF-8")



# Improvers and Decliners ####

# global
Democracy_Matrix_Small %>% 
  filter(year>=current_year-1) %>%
  select(country, year, total_index_context) %>% 
  group_by(country) %>% 
  mutate(total_index_context_lag = dplyr::lag(total_index_context, 1),
         difference = total_index_context-total_index_context_lag) %>% 
  ungroup() %>% 
  filter(year==current_year) %>% 
  na.omit() %>% 
  top_n(-5, difference) %>% 
  mutate(direction = "worse") %>%
  bind_rows(Democracy_Matrix_Small %>% 
              filter(year>=current_year-1) %>%
              select(country, year, total_index_context) %>% 
              group_by(country) %>% 
              mutate(total_index_context_lag = dplyr::lag(total_index_context, 1),
                     difference = total_index_context-total_index_context_lag) %>% 
              ungroup() %>% 
              filter(year==current_year) %>% 
              na.omit() %>% 
              top_n(5, difference) %>% 
              mutate(direction = "improvement")
  ) %>% 
  left_join(identification_plots, by="country") %>% 
  mutate(DE_Land = fct_reorder(DE_Land, difference)) %>% 
  rename(Country=DE_Land) %>% 
  ggplot(aes(x=Country, y=difference, fill=direction)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste(current_year -1, " : ", round(total_index_context_lag, 3), "\n",
                            current_year, " : ", round(total_index_context,3), sep="")),
            hjust=c(-0.5,-0.5,-0.5,-0.1,-0.5,
                    1.5,1.5,1.1,1.5,1.5),
            size=2
            ) +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  coord_flip() +
  xlab("") +
  ylab(paste("Differenz", current_year, "-", current_year-1)) +
  geom_hline(yintercept = 0, size=1.5) +
  labs(title = paste("Top Aufsteiger und Absteiger", current_year-1, "-", current_year, "(weltweit)"), 
       subtitle = "Gesamtwertindex (Kontextmessung)",
       caption = "Positive Werte: Verbesserung;
       \n Negative Werte: Verschlechterung \n
       Datensatz der Demokratiematrix V3") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
        legend.position = "none") 

# Save
ggsave("WebsiteMaterial/Plots/Improvement_DE.png", device = "png", width=20, height=15, units="cm")



Democracy_Matrix_Small %>% 
  filter(year>=current_year-1) %>%
  select(country, year, total_index_context) %>% 
  group_by(country) %>% 
  mutate(total_index_context_lag = dplyr::lag(total_index_context, 1),
         difference = total_index_context-total_index_context_lag) %>% 
  ungroup() %>% 
  filter(year==current_year) %>% 
  na.omit() %>% 
  top_n(-5, difference) %>% 
  mutate(direction = "worse") %>%
  bind_rows(Democracy_Matrix_Small %>% 
              filter(year>=current_year-1) %>%
              select(country, year, total_index_context) %>% 
              group_by(country) %>% 
              mutate(total_index_context_lag = dplyr::lag(total_index_context, 1),
                     difference = total_index_context-total_index_context_lag) %>% 
              ungroup() %>% 
              filter(year==current_year) %>% 
              na.omit() %>% 
              top_n(5, difference) %>% 
              mutate(direction = "improvement")
  ) %>% 
  left_join(identification_plots, by="country") %>% 
  mutate(EN_country = fct_reorder(EN_country, difference)) %>% 
  rename(Country=EN_country) %>% 
  ggplot(aes(x=Country, y=difference, fill=direction)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste(current_year -1, " : ", round(total_index_context_lag, 3), "\n",
                            current_year, " : ", round(total_index_context,3), sep="")),
            hjust=c(-0.5,-0.5,-0.5,-0.1,-0.5,
                    1.5,1.5,1.1,1.5,1.5),
            size=2
  ) +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  coord_flip() +
  xlab("") +
  ylab(paste("Difference", current_year, "-", current_year-1)) +
  geom_hline(yintercept = 0, size=1.5) +
  labs(title = paste("Top Improvers and Decliners", current_year-1, "-", current_year, "(global)"), 
       subtitle = "Total Value Index (Context Measurement)",
       caption = "Positive Values: Improvement;
       \n Negative Values: Decline \n
       Dataset of the Democracy Matrix V3") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
        legend.position = "none") 

# Save
ggsave("WebsiteMaterial/Plots/Improvement_EN.png", device = "png", width=20, height=15, units="cm")


# Europe

Democracy_Matrix_Small %>% 
  filter(year>=current_year-1,
         regions == "Europe") %>%
  select(country, year, total_index_context) %>% 
  group_by(country) %>% 
  mutate(total_index_context_lag = dplyr::lag(total_index_context, 1),
         difference = total_index_context-total_index_context_lag) %>% 
  ungroup() %>% 
  filter(year==current_year) %>% 
  na.omit() %>% 
  top_n(-5, difference) %>% 
  mutate(direction = "worse") %>%
  bind_rows(Democracy_Matrix_Small %>% 
              filter(year>=current_year-1,
                     regions == "Europe") %>%
              select(country, year, total_index_context) %>% 
              group_by(country) %>% 
              mutate(total_index_context_lag = dplyr::lag(total_index_context, 1),
                     difference = total_index_context-total_index_context_lag) %>% 
              ungroup() %>% 
              filter(year==current_year) %>% 
              na.omit() %>% 
              top_n(5, difference) %>% 
              mutate(direction = "improvement")
  ) %>% 
  left_join(identification_plots, by="country") %>% 
  mutate(DE_Land = fct_reorder(DE_Land, difference)) %>% 
  rename(Country=DE_Land) %>% 
  ggplot(aes(x=Country, y=difference, fill=direction)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste(current_year -1, " : ", round(total_index_context_lag, 3), "\n",
                            current_year, " : ", round(total_index_context,3), sep="")),
            hjust=c(-0.5,-0.5,-0.5,-0.5,-0.5,
                    -0.1,-0.1,-0.1,1.2,-0.1),
            size=2
  ) +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  coord_flip() +
  xlab("") +
  ylab(paste("Differenz", current_year, "-", current_year-1)) +
  geom_hline(yintercept = 0, size=1.5) +
  labs(title = paste("Top Aufsteiger und Absteiger", current_year-1, "-", current_year, "(Europa)"), 
       subtitle = "Gesamtwertindex (Kontextmessung)",
       caption = "Positive Werte: Verbesserung;
       \n Negative Werte: Verschlechterung \n
       Datensatz der Demokratiematrix V3") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
        legend.position = "none") 

# Save
ggsave("WebsiteMaterial/Plots/Improvement_DE_Europe.png", device = "png", width=20, height=15, units="cm")


Democracy_Matrix_Small %>% 
  filter(year>=current_year-1,
         regions == "Europe") %>%
  select(country, year, total_index_context) %>% 
  group_by(country) %>% 
  mutate(total_index_context_lag = dplyr::lag(total_index_context, 1),
         difference = total_index_context-total_index_context_lag) %>% 
  ungroup() %>% 
  filter(year==current_year) %>% 
  na.omit() %>% 
  top_n(-5, difference) %>% 
  mutate(direction = "worse") %>%
  bind_rows(Democracy_Matrix_Small %>% 
              filter(year>=current_year-1,
                     regions == "Europe") %>%
              select(country, year, total_index_context) %>% 
              group_by(country) %>% 
              mutate(total_index_context_lag = dplyr::lag(total_index_context, 1),
                     difference = total_index_context-total_index_context_lag) %>% 
              ungroup() %>% 
              filter(year==current_year) %>% 
              na.omit() %>% 
              top_n(5, difference) %>% 
              mutate(direction = "improvement")
  ) %>% 
  left_join(identification_plots, by="country") %>% 
  mutate(EN_country = fct_reorder(EN_country, difference)) %>% 
  rename(Country=EN_country) %>% 
  ggplot(aes(x=Country, y=difference, fill=direction)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste(current_year -1, " : ", round(total_index_context_lag, 3), "\n",
                            current_year, " : ", round(total_index_context,3), sep="")),
            hjust=c(-0.5,-0.5,-0.5,-0.5,-0.5,
                    -0.1,-0.1,-0.1,1.2,-0.1),
            size=2
  ) +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  coord_flip() +
  xlab("") +
  ylab(paste("Difference", current_year, "-", current_year-1)) +
  geom_hline(yintercept = 0, size=1.5) +
  labs(title = paste("Top Improvers and Decliners", current_year-1, "-", current_year, "(Europe)"), 
       subtitle = "Total Value Index (Context Measurement)",
       caption = "Positive Values: Improvement;
       \n Negative Values: Decline \n
       Dataset of the Democracy Matrix V3") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
        legend.position = "none")

# Save
ggsave("WebsiteMaterial/Plots/Improvement_EN_Europe.png", device = "png", width=20, height=15, units="cm")

# Regimetype

regimetyp_topworse = Democracy_Matrix_Small %>% 
  filter(year>=current_year-1) %>%
  select(country, year, classification_context, total_index_context) %>% 
  group_by(country) %>% 
  mutate(total_index_context_lag = dplyr::lag(total_index_context, 1),
         difference = total_index_context-total_index_context_lag,
         classification_context = dplyr::lag(classification_context, 1)
         ) %>% 
  ungroup() %>% 
  filter(year==current_year) %>% 
  na.omit() %>%
  group_by(classification_context) %>% 
  top_n(-5, difference) %>%
  ungroup() %>% 
  mutate(direction = "worse") %>%
  bind_rows(Democracy_Matrix_Small %>% 
              filter(year>=current_year-1) %>%
              select(country, year, classification_context, total_index_context) %>% 
              group_by(country) %>% 
              mutate(total_index_context_lag = dplyr::lag(total_index_context, 1),
                     difference = total_index_context-total_index_context_lag,
                     classification_context = dplyr::lag(classification_context, 1)
                     ) %>% 
              ungroup() %>% 
              filter(year==current_year) %>% 
              na.omit() %>% 
              group_by(classification_context) %>% 
              top_n(5, difference) %>% 
              mutate(direction = "improvement") %>% 
              ungroup()
  ) %>% 
  left_join(identification_plots, by="country")

regimetyp_topworse %>% 
  rename(Country=DE_Land) %>% 
  mutate(Country = as.character(Country),
         Country = if_else(Country == "Kongo, Demokratische Republik", 
                           "Kongo, \nDemokratische Republik", Country)) %>% 
  mutate(Country = fct_reorder(Country, difference)) %>% 
  ggplot(aes(x=Country, y=difference, fill=direction)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  facet_wrap(classification_context ~ ., scales = "free_y") + 
  coord_flip() +
  xlab("") +
  ylab(paste("Difference", current_year, "-", current_year-1)) +
  geom_hline(yintercept = 0, size=1.5) +
  labs(title = paste("Top Aufsteiger und Absteiger", current_year-1, "-", current_year, "(Regimetypus)"), 
       subtitle = "Gesamtwertindex (Kontextmessung)",
       caption = "Positive Werte: Verbesserung;
       \n Negative Werte: Verschlechterung \n
       Regimetypus: Kontextmessung 2017\n
       Datensatz der Demokratiematrix V3") +  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
        legend.position = "none")

# Save
ggsave("WebsiteMaterial/Plots/Improvement_DE_Regimetyp.png", device = "png", width=25, height=20, units="cm")


regimetyp_topworse %>% 
  rename(Country=EN_country) %>% 
  mutate(Country = as.character(Country),
         Country = if_else(Country == "United States of America", 
                           "United States\nof America", Country),
         Country = if_else(Country == "Congo, Democratic Republic", 
                           "Congo, \nDemocratic Republic", Country)) %>% 
  mutate(Country = fct_reorder(Country, difference)) %>% 
  ggplot(aes(x=Country, y=difference, fill=direction)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  facet_wrap(classification_context ~ ., scales = "free_y") + 
  coord_flip() +
  xlab("") +
  ylab(paste("Difference", current_year, "-", current_year-1)) +
  geom_hline(yintercept = 0, size=1.5) +
  labs(title = paste("Top Improvers and Decliners", current_year-1, "-", current_year, "(Regimetyp)"),
       subtitle = "Total Value Index (Context Measurement)",
       caption = "Positive Values: Improvement;
       \n Negative Values: Decline \n
       Regimetype: Context Measurement 2017 \n
       Dataset of the Democracy Matrix V3") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
        legend.position = "none")

# Save
ggsave("WebsiteMaterial/Plots/Improvement_EN_Regimetyp.png", device = "png", width=25, height=20, units="cm")
