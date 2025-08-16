###Summary Stats###
library(dplyr)
library(readxl)
library(forcats)
library(patchwork)
library(ggplot2)
Thesis_dataset_actual <- read_excel("~/Library/CloudStorage/OneDrive-NewcastleUniversity/Thesis/Thesis_dataset_actual.xlsx")

### Number of References
##for all references countries
selected_countries <- c(
  "Australia", "Austria", "Azerbaijan", "Estonia",
  "Suriname", "Trinidad_Tobago", "Eritrea", "Lao_PDR",
  "Mozambique", "Nepal", "United_Kingdom")

#Filter the dataset
all_ref_countries <- Thesis_dataset_actual %>%
  filter(Country %in% selected_countries)

#Count the number of rows for each of the selected countries
country_row_counts <- all_ref_countries %>%
  count(Country, name = "RowCount")

print(country_row_counts)

#Average
average_row_count <- mean(country_row_counts$RowCount)

# Calculate the standard error
sd_row_count <- sd(country_row_counts$RowCount)
sample_size <- nrow(country_row_counts)

standard_error_row_count <- sd_row_count / sqrt(sample_size)




#Proportion Grey Lit
#Exclude NA values 
references_clean_lit_all_ref <- all_ref_countries %>% filter(Lit_type != "NA")

#Calculate frequencies of literature types for each country
ref_types_freq_all_ref <- as.data.frame(table(references_clean_lit_all_ref$Lit_type, references_clean_lit_all_ref$Country))
colnames(ref_types_freq_all_ref) <- c("Lit_type", "Country", "Frequency")
ref_types_freq_all_ref$Lit_type <- as.character(ref_types_freq_all_ref$Lit_type)

###Clean the grey_lit column
references_clean_grey_lit <- all_ref_countries %>% filter(Grey_lit_type != "NA")

#stacked bar chart
#Counts of each lit_type per country
grey_lit_freq <- references_clean_grey_lit %>%
  count(Country, Grey_lit_type)

grey_lit_freq_filtered <- grey_lit_freq %>%
  filter(!Grey_lit_type %in% c("Conference_paper", "Report", "Thesis", "Journal_npr"))


custom_country_order <- c(
  "Eritrea", "Liberia", "Mozambique", "Somalia",
  "Azerbaijan", "Bosnia_Herzegovina", "Brazil", "Dominica", "Lao_PDR", "Lesotho", "Mauritius", "Nepal", "Serbia", "Saint_Vincent_Grenadines", "Suriname", "Vanuatu",
  "Australia", "Austria", "Belgium", "Denmark", "Estonia", "Hungary", "Liechtenstein", "Lithuania", "New_Zealand", "Palau", "Saudi_arabia", "Singapore", "St_Kitts_Nevis", "Trinidad_Tobago", "United_Kingdom"
)




grey_lit_freq_filtered <- grey_lit_freq_filtered %>%
  mutate(Country = factor(Country, levels = custom_country_order))

# Create stacked bar chart
ggplot(grey_lit_freq_filtered, aes(x = Country, y = n, fill = Grey_lit_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Frequency", fill = "Literature Type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))


percentage_greylittype_by_country <- grey_lit_freq_filtered %>%
  group_by(Country) %>%
  mutate(total_count = sum(n)) %>%
  ungroup() %>%
  mutate(percentage = n / total_count * 100)


### Get top 3 grey lit types  per country type based on percentage
top3_grey_littype_percentage_by_country <- percentage_greylittype_by_country %>%
  group_by(Country) %>%
  slice_max(order_by = percentage, n = 3, with_ties = FALSE) %>%
  ungroup() 

ggplot(percentage_greylittype_by_country, aes(x = Country, y = Grey_lit_type, fill = percentage)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low ="yellow", high = "red") +
  labs(x = "Country", y = "Grey literature Type", fill = "Percentage of total Grey Literature References") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))

### Research vs other proportion 

#Include Book_chap in Book classificaiton
filtered_lit_refs_all_ref <- references_clean_lit_all_ref %>%
  mutate(Lit_type = ifelse(Lit_type == "Book_chap", "Book", Lit_type))


overall_research <- filtered_lit_refs_all_ref %>%
  mutate(overall_type = case_when(
    Lit_type %in% c("Book", "Journal") ~ "Research",
    Grey_lit_type %in% c("Report", "Conference_paper", "Journal_npr", "Thesis") ~ "Research",
    TRUE ~ "Other_Grey"
  ))


overall_research_freq <- overall_research %>%
  count(Country, overall_type)


percentage_research_by_country <- overall_research_freq %>%
  group_by(Country) %>%
  mutate(total_count = sum(n)) %>%
  ungroup() %>%
  mutate(percentage = n / total_count * 100)

percentage_research_by_selected_countries <- percentage_research_by_country %>% 
  filter(Country %in% c("Australia", "Austria","Azerbaijan", "Eritrea", "Estonia",
                        "Lao_PDR", "Mozambique", "Nepal", "Suriname", "Trinidad_Tobago", "United_Kingdom")) 

percentage_research_by_selected_countries <- percentage_research_by_selected_countries %>% #add socioeconomic data
  left_join(pub_year_income_group, by = c("Country" = "Country"))

percentage_research_by_selected_countries <- percentage_research_by_selected_countries %>% mutate(Country = fct_reorder(Country, NY.GNP.PCAP.CD, .desc = FALSE))

percentage_research_by_selected_countries <- percentage_research_by_selected_countries %>%
  mutate(facet_group = factor(simple_income_group, levels = c("Lower income", "Higher income")))


ggplot(percentage_research_by_selected_countries, aes(x = Country, y = percentage, fill = overall_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of references (%)", fill = "Literature Type") +
  scale_fill_manual(values = c("lightgrey", "azure4"), labels = c("Other Grey Literature", "Research Type Literature")) +
  scale_x_discrete(expand = c(0, 1), labels = country_labels)+
  facet_grid(. ~ facet_group, scales = "free", space='free') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 15),
        axis.title = element_text(size = 15),  # axis titles
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))

#Proportion vs GDP 
proportion_test <-percentage_research_by_country %>% #add socioeconomic data
  left_join(protected_area_income_epi, by = c("Country" = "Country"))

proportion_test1 <- proportion_test %>% filter(overall_type == "Research")
#insig
research_proportion_income_model <- lm(percentage ~ NY.GDP.PCAP.CD, data = proportion_test1)
summary(research_proportion_income_model)


##For all research ###
#Include Book_chap in Book classificaiton
filtered_research_ref_stat1 <- Thesis_dataset_actual %>%
  mutate(Lit_type = ifelse(Lit_type == "Book_chap", "Book", Lit_type))

#Add Report from Grey_lit_type as a lit type 
filtered_research_ref_stat2 <- Thesis_dataset_actual_clean_access %>%
  filter(Grey_lit_type %in%  c("Report", "Conference_paper","Journal_npr","Thesis")) %>%
  mutate(Lit_type = case_when(
    Grey_lit_type == "Report" ~ "Report",
    Grey_lit_type == "Conference_paper" ~ "Conference_paper",
    Grey_lit_type == "Journal_npr" ~ "Journal_npr",
    Grey_lit_type == "Thesis" ~ "Thesis"))

#Combine the two datasets
filtered_research_ref_combined_stat <- bind_rows(filtered_research_ref_stat1, filtered_research_ref_stat2)

# Filter for relevant literature types
filtered_lit_ref_stat <- filtered_research_ref_combined_stat %>%
  filter(Lit_type %in% c("Journal", "Book", "Report","Conference_paper","Journal_npr", "Thesis"))

#Count the number of rows for each of the selected countries
country_row_counts_research <- filtered_lit_ref_stat %>%
  count(Country, name = "RowCount")

new_row <- data.frame(Country = "Azerbaijan", RowCount = 0) #Add Azerbaijan's 0 in 
country_row_counts_research <- rbind(country_row_counts_research, new_row)

print(country_row_counts_research)

#Average
average_row_count_research <- mean(country_row_counts_research$RowCount)

# Calculate the standard error
sd_row_count_research <- sd(country_row_counts_research$RowCount)
sample_size_research <- nrow(country_row_counts_research)

standard_error_row_count_research <- sd_row_count_research / sqrt(sample_size_research)

###Research references per page 
page_version_pubyear <- read_excel("~/Library/CloudStorage/OneDrive-NewcastleUniversity/Thesis/CBD_Countries.xlsx", 
                                   sheet = "Number, Version, Pubyear")

country_row_counts_research_pages <-country_row_counts_research %>%
  left_join(page_version_pubyear, by = c("Country" = "Country"))

country_row_counts_research_pages <-country_row_counts_research_pages %>% #add socioeconomic data
  left_join(pub_year_income_group, by = c("Country" = "Country"))

sum_of_row_count <- country_row_counts_research_pages %>%
  filter(simple_income_group %in% c("Lower income", "Higher income")) %>%
  group_by(simple_income_group) %>%
  summarise(TotalRowCount = sum(RowCount))


country_row_counts_research_pages$Reference_per_page <- country_row_counts_research_pages$RowCount / country_row_counts_research_pages$Page_number


country_row_counts_research_pages <- country_row_counts_research_pages %>%
  mutate(Country = factor(Country, levels = custom_country_order))


country_row_counts_research_pages <- country_row_counts_research_pages %>%
  mutate(facet_group = factor(simple_income_group, levels = c("Lower income", "Higher income")))

country_row_counts_research_pages <- country_row_counts_research_pages %>% mutate(Country = fct_reorder(Country, NY.GNP.PCAP.CD, .desc = FALSE))

summaryplot1<- ggplot(country_row_counts_research_pages, aes(x = Country, y = RowCount, fill = simple_income_group)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Number of Research references") +
  facet_grid(. ~ facet_group, scales = "free", space='free') +
  scale_x_discrete(expand = c(0, 1), labels = country_labels)+
  scale_fill_manual( values = c("Lower income" = "dodgerblue3", "Higher income" = "lightskyblue2")) +
  theme_classic() +
  theme(strip.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "none")

summaryplot2 <- ggplot(country_row_counts_research_pages, aes(x = Country, y = Reference_per_page, fill = simple_income_group)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Number of Research references per page") +
  scale_x_discrete(expand = c(0, 1), labels = country_labels)+
  scale_fill_manual( values = c("Lower income" = "dodgerblue3", "Higher income" = "lightskyblue2")) +
  facet_grid(. ~ facet_group, scales = "free", space='free') +
  theme_classic() +
  theme(strip.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.position = "none")

(summaryplot1 / summaryplot2+
    plot_layout(axes = "collect"))

reference_country_model1 <- t.test(RowCount ~simple_income_group, data = country_row_counts_research_pages)

reference_country_model2 <- t.test(Reference_per_page~simple_income_group, data = country_row_counts_research_pages)


#Average referenes per page
mean(country_row_counts_research_pages$Reference_per_page)
sd(country_row_counts_research_pages$Reference_per_page) /30

#Labels
#Country Labels
country_labels <- c(
  "Australia" = "Australia",
  "Austria" = "Austria",
  "Belgium" = "Belgium",
  "Denmark" = "Denmark",
  "Estonia" = "Estonia",
  "Hungary" = "Hungary",
  "Liechtenstein" = "Liechtenstein",
  "Lithuania" = "Lithuania",
  "New_Zealand" = "New Zealand",
  "Palau" = "Palau",
  "Saudi_arabia" = "Saudi Arabia", 
  "St_Kitts_Nevis" = "St. Kitts & Nevis",
  "Trinidad_Tobago" = "Trinidad & Tobago", 
  "United_Kingdom" = "United Kingdom", 
  "Bosnia_Herzegovina" = "Bosnia & Herzegovina", 
  "Brazil" = "Brazil",
  "Dominica" = "Dominica",
  "Eritrea" = "Eritrea",
  "Lao_PDR" = "Lao PDR",
  "Lesotho" = "Lesotho",
  "Liberia" = "Liberia",
  "Mauritius" = "Mauritius",
  "Mozambique" = "Mozambique",
  "Nepal" = "Nepal",
  "Saint_Vincent_Grenadines" = "St. Vincent & Grenadines",
  "Serbia" = "Serbia",
  "Somalia" = "Somalia",
  "Suriname" = "Suriname",
  "Vanuatu" = "Vanuatu")

  
#Average and country labels
average_country_labels <- c(
  "Average" = "Average",
  "Australia" = "Australia",
  "Austria" = "Austria",
  "Belgium" = "Belgium",
  "Denmark" = "Denmark",
  "Estonia" = "Estonia",
  "Hungary" = "Hungary",
  "Liechtenstein" = "Liechtenstein",
  "Lithuania" = "Lithuania",
  "New_Zealand" = "New Zealand",
  "Palau" = "Palau",
  "Saudi_arabia" = "Saudi Arabia", 
  "St_Kitts_Nevis" = "St. Kitts & Nevis",
  "Trinidad_Tobago" = "Trinidad & Tobago", 
  "United_Kingdom" = "United Kingdom", 
  "Bosnia_Herzegovina" = "Bosnia & Herzegovina", 
  "Brazil" = "Brazil",
  "Dominica" = "Dominica",
  "Eritrea" = "Eritrea",
  "Lao_PDR" = "Lao PDR",
  "Lesotho" = "Lesotho",
  "Liberia" = "Liberia",
  "Mauritius" = "Mauritius",
  "Mozambique" = "Mozambique",
  "Nepal" = "Nepal",
  "Saint_Vincent_Grenadines" = "St. Vincent & Grenadines",
  "Serbia" = "Serbia",
  "Somalia" = "Somalia",
  "Suriname" = "Suriname",
  "Vanuatu" = "Vanuatu")
