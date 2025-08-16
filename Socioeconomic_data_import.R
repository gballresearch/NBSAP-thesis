#GDP, EPI, Protected area number
library(wdpar)
library(WDI)
library(dplyr)
library(forcats)

economy_class_wb <- read_excel("CLASS.xlsx")

#Protected area for countries
countries <-c("AUS","AUT","AZE","BEL","BIH",
              "BRA","DNK","DMA","ERI","EST",
              "HUN","LAO","LSO","LBR","LIE",
              "LTU","MUS","MOZ","NPL","NZL",
              "PLW","VCT","SAU","SRB","SGP",
              "SOM","KNA","SUR","TTO","GBR","VUT")


#GDP per captia for countries
#Eritrea only has for 2011 so NA right now
gni_protected_area <- WDI(country = c("AU","AT","AZ","BE","BA",
                                      "BR","DK","DM","ER","EE",
                                      "HU","LA","LS","LR","LI",
                                      "LT","MU","MZ","NP","NZ",
                                      "PW","VC","SA","RS","SG",
                                      "SO","KN","SR","TT","GB","VU"), indicator = c("ER.LND.PTLD.ZS", "NY.GNP.PCAP.CD"), start = 2024, end = 2024)

gni_eritrea <- WDI(country = c("ER"), indicator = c("NY.GNP.PCAP.CD"), start = 2011, end = 2011)
gni_liechtenstein <- WDI(country = c("LI"), indicator = c("NY.GNP.PCAP.CD"), start = 2009, end = 2009)
gni_palau <-  WDI(country = c("PW"), indicator = c("NY.GNP.PCAP.CD"), start = 2023, end = 2023)

gdp_protected_area$NY.GNP.PCAP.CD[gdp_protected_area$country == "Eritrea"] <- gni_eritrea$NY.GNP.PCAP.CD
gdp_protected_area$NY.GNP.PCAP.CD[gdp_protected_area$country == "Liechtenstein"] <- gni_liechtenstein$NY.GNP.PCAP.CD
gdp_protected_area$NY.GNP.PCAP.CD[gdp_protected_area$country == "Palau"] <- gni_palau$NY.GNP.PCAP.CD




ggplot(gdp_protected_area, aes(x =  country, y = NY.GNP.PCAP.CD)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ country, scales = "free_x") +
  labs(title = "GDP of selected countries",
       x = "Country",
       y = "EPI") +
  theme_classic() 



#add economic class
economy_class_data <- economy_class_wb %>%
  select(Code, "Income group")

protected_area_income <- gdp_protected_area %>%
  left_join(economy_class_data, by = c("iso3c" = "Code")) %>%
  rename(income_group = "Income group")

protected_area_income <- protected_area_income %>%
  mutate(income_group = case_when(
    income_group %in% "High income" ~ "Higher income",
    income_group %in% c("Upper middle income", "Lower middle income") ~ "Middle Income",
    income_group %in% "Low income" ~ "Lower income"))



#EPI for countries 2024 from https://epi.yale.edu/measure/2024/EPI
EPI_scores <- data.frame(
  Country =c("AUS","AUT","AZE","BEL","BIH",
               "BRA","DNK","DMA","ERI","EST",
               "HUN","LAO","LSO","LBR","LIE",
               "LTU","MUS","MOZ","NPL","NZL",
               "PLW","VCT","SAU","SRB","SGP",
               "SOM","KNA","SUR","TTO","GBR","VUT"),
  EPI = c(63.1, 68.9, 40.5, 66.8, 46.0,
          53.0, 67.7, 49.3, 29.0, 75.7,
          59.8, 26.3, 36.9, 34.3, NA,
          64.1, 47.3, 39.0, 33.1, 57.3,
          NA, 54.2, 42.5, 49.8, 53.0,
          NA, NA, 56.9, 52.5, 72.6, 45.0))

#add to other dataset 
protected_area_income_epi <-protected_area_income %>%
  left_join(EPI_scores, by = c("iso3c" = "Country"))

#Change back to country names
protected_area_income_epi <- protected_area_income_epi %>%
  mutate(Country = case_when(
    iso3c == "AUS" ~ "Australia",
    iso3c == "AUT" ~ "Austria",
    iso3c == "AZE" ~ "Azerbaijan",
    iso3c == "BEL" ~ "Belgium",
    iso3c == "BIH" ~ "Bosnia_Herzegovina",
    iso3c == "BRA" ~ "Brazil",
    iso3c == "DNK" ~ "Denmark",
    iso3c == "DMA" ~ "Dominica",
    iso3c == "ERI" ~ "Eritrea",
    iso3c == "EST" ~ "Estonia",
    iso3c == "HUN" ~ "Hungary",
    iso3c == "LAO" ~ "Lao_PDR",
    iso3c == "LSO" ~ "Lesotho",
    iso3c == "LBR" ~ "Liberia",
    iso3c == "LIE" ~ "Liechtenstein",
    iso3c == "LTU" ~ "Lithuania",
    iso3c == "MUS" ~ "Mauritius",
    iso3c == "MOZ" ~ "Mozambique",
    iso3c == "NPL" ~ "Nepal",
    iso3c == "NZL" ~ "New_Zealand",
    iso3c == "PLW" ~ "Palau",
    iso3c == "VCT" ~ "Saint_Vincent_Grenadines",
    iso3c == "SAU" ~ "Saudi_arabia",
    iso3c == "SRB" ~ "Serbia",
    iso3c == "SGP" ~ "Singapore",
    iso3c == "SOM" ~ "Somalia",
    iso3c == "KNA" ~ "St_Kitts_Nevis",
    iso3c == "SUR" ~ "Suriname",
    iso3c == "TTO" ~ "Trinidad_Tobago",
    iso3c == "GBR" ~ "United_Kingdom",
    iso3c == "VUT" ~ "Vanuatu"))

protected_area_income_epi <- protected_area_income_epi[,-1] #get rid of other country column
income_group_data <- protected_area_income_epi[,c(6,8)]

protected_area_income_epi$simple_income_group <- ifelse(
  protected_area_income_epi$income_group == "Higher income",
  "Higher income",
  "Lower income"
)

income_group_counts <- protected_area_income_epi %>%
  count(simple_income_group, name = "count_of_rows")

print(income_group_counts)

pub_year_income_group <- protected_area_income_epi %>% left_join(pub_years, by = "Country" )
pub_year_income_group_high <- pub_year_income_group %>% filter(simple_income_group == "Higher income")
pub_year_income_group_low <- pub_year_income_group %>% filter(simple_income_group == "Lower income")
mean(pub_year_income_group_high$PubYear)
mean(pub_year_income_group_low$PubYear)


protected_area_income_epi_plot <- protected_area_income_epi %>%
  mutate(simple_income_group = factor(simple_income_group, levels = c("Lower income","Higher income")))

protected_area_income_epi_plot <- protected_area_income_epi_plot %>% mutate(Country = fct_reorder(Country, NY.GNP.PCAP.CD, .desc = FALSE))


ggplot(protected_area_income_epi_plot, aes(x = Country, y = NY.GNP.PCAP.CD, fill = simple_income_group)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "GNI per capita (USD)") +
  scale_fill_manual( values = c("Lower income" = "dodgerblue3", "Higher income" = "lightskyblue2")) +
  facet_grid(. ~ simple_income_group, scales = "free", space='free') +
  scale_x_discrete(expand = c(0, 0.1), labels = country_labels)+
  theme_classic() +
  theme(strip.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 13),
        panel.spacing.x = unit(1, "cm"),
        legend.position = "none")


