source("Setup/Packages.R")
source("Datasets/CreateWebsiteData.R")

Democracy_Matrix_Small = fread("upload/DemocracyMatrix_v1_1.csv")

# identifier for translation in EN and DE
typo3_identification_classification = fread("upload/typo3_identification_classification.csv", encoding = "UTF-8") %>% 
  select(-class_identifier, classification_context = "EN", DE_Klassifikation="DE")
typo3_identification = fread("upload/typo3_identification_1_1_UTF8.csv", encoding = "UTF-8") %>% 
  select(country=country_identifier, EN_country = "EN", DE_Land="DE")

# Set current (max) year (edition) 
current_year = 2017 


# Ranking ####

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
  left_join(typo3_identification_classification, by="classification_context") %>% 
  left_join(typo3_identification, by="country") %>% 
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
  left_join(typo3_identification, by="country") %>% 
  select(Rank=rank, Country=EN_country, "Total Value Index" =total_index_context, 
         Classification=classification_context) %>% 
  write.csv("WebsiteMaterial/Ranking_EN.csv", row.names = F, fileEncoding = "UTF-8")



# Improvers and Decliners ####

# global
Democracy_Matrix_Small %>% 
  filter(year>=current_year-1) %>%
  select(country, year, total_index_context) %>% 
  group_by(country) %>% 
  mutate(total_index_context_2016 = dplyr::lag(total_index_context, 1),
         difference = total_index_context-total_index_context_2016) %>% 
  ungroup() %>% 
  filter(year==current_year) %>% 
  na.omit() %>% 
  top_n(-5, difference) %>% 
  mutate(direction = "worse") %>%
  bind_rows(Democracy_Matrix_Small %>% 
              filter(year>=current_year-1) %>%
              select(country, year, total_index_context) %>% 
              group_by(country) %>% 
              mutate(total_index_context_2016 = dplyr::lag(total_index_context, 1),
                     difference = total_index_context-total_index_context_2016) %>% 
              ungroup() %>% 
              filter(year==current_year) %>% 
              na.omit() %>% 
              top_n(5, difference) %>% 
              mutate(direction = "improvement")
  ) %>% 
  left_join(typo3_identification, by="country") %>% 
  mutate(DE_Land = fct_reorder(DE_Land, difference)) %>% 
  select(Land=DE_Land, difference, direction) %>% 
  ggplot(aes(x=Land, y=difference, fill=direction)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  coord_flip() +
  xlab("") +
  ylab(paste("Differenz", current_year, "-", current_year-1)) +
  geom_hline(yintercept = 0, size=1.5) +
  labs(title = paste("Top Aufsteiger und Absteiger", current_year-1, "-", current_year, "(weltweit)"), 
       subtitle = "Gesamtwertindex (Kontextmessung)",
       caption = "Positive Werte: Verbesserung;
       \n Negative Werte: Verschlechterung \n
       Datensatz der Demokratiematrix V1.1") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
        legend.position = "none") 
# Save
ggsave("WebsiteMaterial/Improvement_DE.png", device = "png", width=20, height=15, units="cm")



Democracy_Matrix_Small %>% 
  filter(year>=current_year-1) %>%
  select(country, year, total_index_context) %>% 
  group_by(country) %>% 
  mutate(total_index_context_2016 = dplyr::lag(total_index_context, 1),
         difference = total_index_context-total_index_context_2016) %>% 
  ungroup() %>% 
  filter(year==current_year) %>% 
  na.omit() %>% 
  top_n(-5, difference) %>% 
  mutate(direction = "worse") %>%
  bind_rows(Democracy_Matrix_Small %>% 
              filter(year>=current_year-1) %>%
              select(country, year, total_index_context) %>% 
              group_by(country) %>% 
              mutate(total_index_context_2016 = dplyr::lag(total_index_context, 1),
                     difference = total_index_context-total_index_context_2016) %>% 
              ungroup() %>% 
              filter(year==current_year) %>% 
              na.omit() %>% 
              top_n(5, difference) %>% 
              mutate(direction = "improvement")
  ) %>% 
  left_join(typo3_identification, by="country") %>% 
  mutate(EN_country = fct_reorder(EN_country, difference)) %>% 
  select(Country=EN_country, difference, direction) %>% 
  ggplot(aes(x=Country, y=difference, fill=direction)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  coord_flip() +
  xlab("") +
  ylab(paste("Difference", current_year, "-", current_year-1)) +
  geom_hline(yintercept = 0, size=1.5) +
  labs(title = paste("Top Improvers and Decliners", current_year-1, "-", current_year, "(global)"), 
       subtitle = "Total Value Index (Context Measurement)",
       caption = "Positive Values: Improvement;
       \n Negative Values: Decline \n
       Dataset of the Democracy Matrix V1.1") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
        legend.position = "none") 

# Save
ggsave("WebsiteMaterial/Improvement_EN.png", device = "png", width=20, height=15, units="cm")


# Europe

Democracy_Matrix_Small %>% 
  filter(year>=current_year-1,
         regions == "Europe") %>%
  select(country, year, total_index_context) %>% 
  group_by(country) %>% 
  mutate(total_index_context_2016 = dplyr::lag(total_index_context, 1),
         difference = total_index_context-total_index_context_2016) %>% 
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
              mutate(total_index_context_2016 = dplyr::lag(total_index_context, 1),
                     difference = total_index_context-total_index_context_2016) %>% 
              ungroup() %>% 
              filter(year==current_year) %>% 
              na.omit() %>% 
              top_n(5, difference) %>% 
              mutate(direction = "improvement")
  ) %>% 
  left_join(typo3_identification, by="country") %>% 
  mutate(DE_Land = fct_reorder(DE_Land, difference)) %>% 
  select(Land=DE_Land, difference, direction) %>% 
  ggplot(aes(x=Land, y=difference, fill=direction)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  coord_flip() +
  xlab("") +
  ylab(paste("Differenz", current_year, "-", current_year-1)) +
  geom_hline(yintercept = 0, size=1.5) +
  labs(title = paste("Top Aufsteiger und Absteiger", current_year-1, "-", current_year, "(Europa)"), 
       subtitle = "Gesamtwertindex (Kontextmessung)",
       caption = "Positive Werte: Verbesserung;
       \n Negative Werte: Verschlechterung \n
       Datensatz der Demokratiematrix V1.1") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
        legend.position = "none") 

# Save
ggsave("WebsiteMaterial/Improvement_DE_Europe.png", device = "png", width=20, height=15, units="cm")


Democracy_Matrix_Small %>% 
  filter(year>=current_year-1,
         regions == "Europe") %>%
  select(country, year, total_index_context) %>% 
  group_by(country) %>% 
  mutate(total_index_context_2016 = dplyr::lag(total_index_context, 1),
         difference = total_index_context-total_index_context_2016) %>% 
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
              mutate(total_index_context_2016 = dplyr::lag(total_index_context, 1),
                     difference = total_index_context-total_index_context_2016) %>% 
              ungroup() %>% 
              filter(year==current_year) %>% 
              na.omit() %>% 
              top_n(5, difference) %>% 
              mutate(direction = "improvement")
  ) %>% 
  left_join(typo3_identification, by="country") %>% 
  mutate(EN_country = fct_reorder(EN_country, difference)) %>% 
  select(Country=EN_country, difference, direction) %>% 
  ggplot(aes(x=Country, y=difference, fill=direction)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#009E73", "#D55E00")) +
  coord_flip() +
  xlab("") +
  ylab(paste("Difference", current_year, "-", current_year-1)) +
  geom_hline(yintercept = 0, size=1.5) +
  labs(title = paste("Top Improvers and Decliners", current_year-1, "-", current_year, "(Europe)"), 
       subtitle = "Total Value Index (Context Measurement)",
       caption = "Positive Values: Improvement;
       \n Negative Values: Decline \n
       Dataset of the Democracy Matrix V1.1") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5),
        legend.position = "none")

# Save
ggsave("WebsiteMaterial/Improvement_EN_Europe.png", device = "png", width=20, height=15, units="cm")

