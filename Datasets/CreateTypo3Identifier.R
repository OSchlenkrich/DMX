# Typo 3 Identification Datasets for Website

source("Setup/Packages.R")
source("Datasets/CreateWebsiteData.R")

# Typo 3 Classification Dataset for Website (DE/EN)

typo3_identification_classification = data.frame(
  class_identifier = 1:4,
  DE = c("Autokratie", "Hybrides Regime", "Defizitäre Demokratie", "Funktionierende Demokratie"),
  EN = c("Autocracy", "Hybrid Regime", "Deficient Democracy", "Working Democracy")
)

write.csv(typo3_identification_classification, "upload/typo3_identification_classification.csv", fileEncoding = "UTF-8", na = "", row.names = F)


# Typo 3 Country Dataset for Website (DE/EN)

typo3_identification = website_data %>%
  select(country_identifier = country) %>%
  distinct() %>%
  mutate(
    EN = country_identifier
  )  %>%
  mutate(EN = fct_recode(EN, 
                         "Europe" = "01-Europe",
                         "North America incl. AUS+NZ" = "02-North America incl. AUS+NZ",
                         "Latin America" = "03-Latin America" ,
                         "Post-Soviet States (without EU-members)" = "04-Post-Soviet States (without EU-members)",
                         "Middle East and North Africa" = "05-Middle East and North Africa",
                         "Sub-Saharan Africa" = "06-Sub-Saharan Africa",
                         "South Asia" = "07-South Asia",
                         "South-East Asia" = "08-South-East Asia",
                         "East Asia" = "09-East Asia",
                         "Small island states" = "10-Small island states",
                         
                         "Congo, Democratic Republic" = "Democratic Republic of Congo",
                         "Vietnam, Democratic Republic" =	"Democratic Republic of Vietnam",
                         "Côte d'Ivoire" = "Ivory Coast",
                         "Congo, Republic" =  "Republic of the Congo",
                         "Vietnam" = "Republic of Vietnam"
    )
  )

custom_match = c(
  "Europe" = "Europe",
  "North America incl. AUS+NZ" = "Nordamerika inkl. AUS+NZ",
  "Latin America" = "Lateinamerika",
  "Post-Soviet States (without EU-members)" = "Postsowjetische Staaten (ohne EU-Mitglieder)",
  "Middle East and North Africa" = "Nahost und Nordafrika",
  "Sub-Saharan Africa" = "Sub-Sahara Afrika",
  "South Asia" = "Südasien",
  "South-East Asia" = "Südostasien",
  "East Asia" = "Ostasien",
  "Small island states" = "Kleine Inselstaaten",
  "Global" = "Global",
  "São Tomé and Príncipe" = "São Tomé und Príncipe",
  
  "Czech Republic" = "Tschechien",
  "Congo, Democratic Republic" =	"Kongo, Demokratische Republik",
  "Vietnam, Democratic Republic" =	"Vietnam, Demokratische Republik",
  "Côte d'Ivoire" =	"Côte d'Ivoire",
  
  "Laos" =	"Laos, Demokratische Volksrepublik",
  "Moldova"	=	"Moldau, Republik",
  "Congo, Republic" =	"Kongo, Republik",
  "Vietnam" =	"Vietnam",
  "Russia" =	"Russland",
  "South Yemen" =	"Jemen, Demokratische Volksrepublik",
  "Syria"	=	"Syrien, Arabische Republik",
  
  "Palestine/British Mandate" = "Palästina/Britisches Mandat",
  "Palestine/Gaza" = "Palästina/Gaza",
  "Palestine/West Bank" = "Palästina/Westjordanland",
  "Somaliland" = "Somaliland"
)


typo3_identification$DE= countrycode(typo3_identification$EN, origin="country.name.en", destination="country.name.de", custom_match = custom_match)

# #Check
# typo3_identification %>%
#   group_by(DE)  %>%
#   summarise(number = n()) %>%
#   arrange(-number)

write.csv(typo3_identification, "upload/typo3_identification_1_1_UTF8.csv", fileEncoding = "UTF-8", na = "", row.names = F)
