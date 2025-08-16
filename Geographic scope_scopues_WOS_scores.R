library(readxl)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(tidyr)

#Import the data
Thesis_dataset_actual <- read_excel("~/Library/CloudStorage/OneDrive-NewcastleUniversity/Thesis/Thesis_dataset_actual.xlsx")

#Exclude NA values
references_clean_scope <- Thesis_dataset_actual %>% filter(Geographic_scope != "NA")

###Geographic scope of all references

# Count frequency of each geographic_scope category
scope_freq <- as.data.frame(table(references_clean_scope$Geographic_scope, references_clean_scope$Country))
colnames(scope_freq) <- c("Geographic_scope", "Country", "Frequency")
scope_freq$Geographic_scope <- as.character(scope_freq$Geographic_scope)

# Create stacked bar chart
ggplot(scope_freq, aes(x = Country, y = Frequency, fill = Geographic_scope)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Frequency", fill = "Geographic Scope") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))

#Percentage
percentage_scope_by_country <- scope_freq %>%
  group_by(Country) %>%
  mutate(total_count = sum(Frequency)) %>%
  ungroup() %>%
  mutate(percentage = Frequency / total_count * 100)

custom_country_order <- c(
  "Australia", "Austria", "Azerbaijan", "Belgium", "Bosnia_Herzegovina",
  "Brazil", "Denmark", "Dominica", "Estonia", "Hungary",
  "Liechtenstein", "Lithuania", "Mauritius", "New_Zealand", "Palau",
  "Saint_Vincent_Grenadines", "Saudi_arabia", "Serbia", "Singapore",
  "St_Kitts_Nevis", "Suriname", "Trinidad_Tobago", "United_Kingdom", # Higher Income Countries
  "Eritrea", "Lao_PDR", "Lesotho", "Liberia", "Mozambique",
  "Nepal", "Somalia", "Vanuatu" # Lower Income Countries
)

percentage_scope_by_country <- percentage_scope_by_country %>%
  mutate(Country = factor(Country, levels = custom_country_order))


ggplot(percentage_scope_by_country, aes(x = Country, y = percentage, fill = Geographic_scope)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage", fill = "Geographic Scope") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))



###What is the geographic scope of the journal articles?
references_lit_filtered <- references_clean_lit %>%
  filter(Lit_type == "Journal")

journal_geo_scope_by_country <- references_lit_filtered %>%
  group_by(Country, Geographic_scope) %>%
  summarise(journal_count = n(), .groups = "drop")

percentage_journalscope_by_country <- journal_geo_scope_by_country %>%
  group_by(Country) %>%
  mutate(total_count = sum(journal_count)) %>%
  ungroup() %>%
  mutate(percentage = journal_count / total_count * 100)


ggplot(journal_geo_scope_by_country, aes(x = Country, y = journal_count, fill = Geographic_scope)) +
  geom_bar(stat = "identity") +
  labs(title = "Journal Articles by Geographic Scope and Country", x = "Country", y = "Number of Journal References", fill = "Geographic Scope") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 15))

ggplot(percentage_journalscope_by_country, aes(x = Country, y = percentage, fill = Geographic_scope)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of Journal References (%)", fill = "Geographic Scope") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20))


###What is the geographic scope of reports?
references_greylit_filtered <- references_clean_grey_lit %>%
  filter(Grey_lit_type == "Report")

report_geo_scope_by_country <- references_greylit_filtered %>%
  group_by(Country, Geographic_scope) %>%
  summarise(report_count = n(), .groups = "drop")


ggplot(report_geo_scope_by_country, aes(x = Country, y = report_count, fill = Geographic_scope)) +
  geom_bar(stat = "identity") +
  labs(title = "Reports by Geographic Scope and Country", x = "Country", y = "Number of Journal References", fill = "Geographic Scope") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.text = element_text(size = 15))


###Journal names
lit_name_counts <- references_lit_filtered %>%
  count(`Lit_name`, sort = TRUE)

print(lit_name_counts)

###Journal score averages
references_lit_filtered <- references_lit_filtered %>%
  mutate(wos = as.numeric(na_if(Journal_impact_wos, "NA")),
          scopus = as.numeric(na_if(Journal_impact_scopus, "NA")))

references_lit_filtered %>%
  summarise(
    wos_missing = sum(is.na(wos)),
    wos_present = sum(!is.na(wos)),
    scopus_missing = sum(is.na(scopus)),
    scopus_present = sum(!is.na(scopus))
  )

avg_scores <- references_lit_filtered %>%
  group_by(Country) %>%
  summarise(avg_wos = mean(wos, na.rm = TRUE), #remove NA first before calculating
    avg_scopus = mean(scopus, na.rm = TRUE),  #remove NA first before calculating
    .groups = "drop")

avg_scores_long <- avg_scores %>%
  pivot_longer(cols = c(avg_wos, avg_scopus),
               names_to = "Database",
               values_to = "Average_score") %>%
  mutate(Database = recode(Database, avg_wos = "WOS", avg_scopus = "Scopus"))

ggplot(avg_scores_long, aes(x = Country, y = Average_score, fill = Database)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Country",
       y = "Average Score",
       fill = "Database") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))


#Boxplot of scores
excluded_counts <- references_lit_filtered %>%
  filter(Journal_impact_wos == "NA" | Journal_impact_scopus == "NA") %>%
  group_by(Country) %>%
  summarise(n_excluded = n())

scores_clean <- references_lit_filtered %>%
  filter(Journal_impact_wos != "NA", Journal_impact_scopus != "NA") %>%
  mutate(
    Journal_impact_wos = as.numeric(Journal_impact_wos),
    Journal_impact_scopus = as.numeric(Journal_impact_scopus))

scores_long <- scores_clean %>%
  pivot_longer(
    cols = c(Journal_impact_wos, Journal_impact_scopus),
    names_to = "Source",
    values_to = "Impact_score")


ggplot(scores_long, aes(x = Country, y = Impact_score, fill = Source)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Country", y = "Impact Score", fill = "Score database") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17))

scores_long_income <- left_join(scores_long, income_group_data, by = "Country")


ggplot(scores_long_income, aes(x = income_group, y = Impact_score, fill = Source)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Country", y = "Impact Score", fill = "Score database") +
  theme_classic() +
  theme(axis.title = element_text(size = 18),  # axis titles
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16))


###Density plot

scopus_scores <- scores_long_income %>% filter(Source == "Journal_impact_scopus")


wos_scores <-  scores_long_income %>% filter(Source == "Journal_impact_wos")

#country
ggplot(scopus_scores, aes(x = Impact_score, fill = Country)) +
  geom_density(alpha = 0.5)+
  theme_minimal() +
  labs(x = "Impact Score", y = "Density", fill = "Country") +
  theme_classic()

#income group
ggplot(scopus_scores, aes(x = Impact_score, fill = income_group)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(x = "Impact Score", y = "Density", fill = "Income group") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(0, 90, by = 5))

#country
ggplot(wos_scores, aes(x = Impact_score, fill = Country)) +
  geom_density(alpha = 0.5)+
  theme_minimal() +
  labs(x = "Impact Score", y = "Density", fill = "Country") +
  theme_classic()

#income group
ggplot(wos_scores, aes(x = Impact_score, fill = income_group)) +
  geom_density(alpha = 0.5)+
  theme_minimal() +
  labs(x = "Impact Score", y = "Density", fill = "Income group") +
  theme_classic()


###Correlation between scores and socioeconomic factors

combined_scores <- left_join(protected_area_income_epi, scores_long, by = c("Country" = "Country"))

model1 <- glm(wos ~ EPI, data = combined_scores) #significant
summary(model1)

model2 <- lm(scopus ~ EPI, data = combined_scores) #significant, higher EPI = higher scopus score
summary(model2)

model3 <- lm(wos~ NY.GDP.PCAP.CD, data = combined_scores) #significant
summary(model3)
model3 <- lm(scopus~ NY.GDP.PCAP.CD, data = combined_scores) #significant
summary(model3)

t.test(wos ~ income_group, data = combined_scores) #sig
t.test(scopus ~ income_group, data = combined_scores) #sig


###Percentage RESEARCH (was journal book and report geographic scope)

#MAKE SURE TO RUN
#Include Book_chap in Book classificaiton
filtered_lit_refs1 <- references_clean_scope %>%
  mutate(Lit_type = ifelse(Lit_type == "Book_chap", "Book", Lit_type))

#Add Report from Grey_lit_type as a lit type 
filtered_lit_refs2 <- references_clean_scope %>%
  filter(Grey_lit_type %in%  c("Report", "Conference_paper","Journal_npr","Thesis")) %>%
  mutate(Lit_type = case_when(
    Grey_lit_type == "Report" ~ "Report",
    Grey_lit_type == "Conference_paper" ~ "Conference_paper",
    Grey_lit_type == "Journal_npr" ~ "Journal_npr",
    Grey_lit_type == "Thesis" ~ "Thesis"))

#Combine the two datasets
filtered_lit_ref_combined <- bind_rows(filtered_lit_refs1, filtered_lit_refs2)

# Filter for relevant literature types
scope_filtered_lit_refs <- filtered_lit_ref_combined %>%
  filter(Lit_type %in% c("Journal", "Book", "Report","Conference_paper","Journal_npr", "Thesis"))

# Count frequency of each geographic_scope category
scope_freq_lit <- as.data.frame(table(scope_filtered_lit_refs$Geographic_scope, scope_filtered_lit_refs$Lit_type))
colnames(scope_freq_lit) <- c("Geographic_scope", "Lit_type", "Frequency")
scope_freq_lit$Geographic_scope <- as.character(scope_freq_lit$Geographic_scope)

# Create stacked bar chart
ggplot(scope_freq_lit, aes(x = Lit_type, y = Frequency, fill = Geographic_scope)) +
  geom_bar(stat = "identity") +
  labs(x = "Literature type ", y = "Frequency", fill = "Geographic Scope") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))

lit_type_sums <- scope_freq_lit %>%
  group_by(Lit_type) %>%
  summarize(total_frequency = sum(Frequency))
            
print(lit_type_sums)

#Percentage
percentage_scope_by_lit_type <- scope_freq_lit %>%
  group_by(Lit_type) %>%
  mutate(total_count = sum(Frequency)) %>%
  ungroup() %>%
  mutate(percentage = Frequency / total_count * 100)

percentage_scope_by_lit_type$Geographic_scope <- factor(percentage_scope_by_lit_type$Geographic_scope,
                          levels = c("Global", "International", "Regional", "National", "Subnational"))

percentage_scope_by_lit_type <- percentage_scope_by_lit_type %>%
  mutate(publication_group = case_when(
    Lit_type == "Report" ~ "Report",
    Lit_type == "Book" ~ "Book",
    TRUE ~ "Journal type"))

ggplot(percentage_scope_by_lit_type, aes(x = Lit_type, y = percentage, fill = Geographic_scope)) +
  scale_fill_manual(values = scope_colours) +
  geom_bar(stat = "identity") +
  facet_grid(publication_group ~ ., scales = "free", space='free') +
  labs(x = "Literature Type", y = "Percentage of Research references (%)", fill = "Geographic Scope") +
  coord_flip() +
  theme_classic() +
  scale_x_discrete(labels = lit_labels, expand = expansion(add = 0.8))+
  theme(strip.text = element_text(size = 13),
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 18),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))


######Per country grouped into income group using levels
scope_freq_country <- as.data.frame(table(scope_filtered_lit_refs$Geographic_scope, scope_filtered_lit_refs$Country))
colnames(scope_freq_country) <- c("Geographic_scope", "Country", "Frequency")
scope_freq_country$Geographic_scope <- as.character(scope_freq_country$Geographic_scope)

# Create stacked bar chart
ggplot(scope_freq_country, aes(x = Country, y = Frequency, fill = Geographic_scope)) +
  geom_bar(stat = "identity") +
  labs(x = "Literature type ", y = "Frequency", fill = "Geographic Scope") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))

Country_scope_sums <- scope_freq_country %>%
  group_by(Country) %>%
  summarize(total_frequency = sum(Frequency))

print(Country_scope_sums)


#Average
average_scope_type <- scope_freq_country %>%
  group_by(Geographic_scope) %>%
  summarise(Frequency = mean(Frequency)) %>%
  mutate(Country = "Average") # Assign a new "Country" value for the average bar


#Combine the original data with the average data
scope_freq_country <- bind_rows(scope_freq_country, average_scope_type)


#Percentage
percentage_scope_by_country <- scope_freq_country %>%
  group_by(Country) %>%
  mutate(total_count = sum(Frequency)) %>%
  ungroup() %>%
  mutate(percentage = Frequency / total_count * 100)

percentage_scope_by_country$Geographic_scope <- factor(percentage_scope_by_country$Geographic_scope,
                                                        levels = c("Global", "International", "Regional", "National", "Subnational"))

percentage_scope_by_country_income <-percentage_scope_by_country %>% #add socioeconomic data
  left_join(protected_area_income_epi, by = c("Country" = "Country"))

percentage_scope_by_country_income <- percentage_scope_by_country_income %>%
  mutate(Facet_Group_Derived = ifelse(Country == "Average", "Average", "Individual Countries"))

percentage_scope_by_country_income <- percentage_scope_by_country_income %>%
  mutate(
    facet_group = ifelse(is.na(income_group) & Country == "Average", 
                         Country, 
                         income_group) )

percentage_scope_by_country_income <- percentage_scope_by_country_income %>%
  mutate(facet_group = factor(facet_group, levels = c("Average", "Higher income", "Middle Income", "Lower income")))




colorBlindGrey8   <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(percentage_scope_by_country_income, aes(x = Country, y = percentage, fill = Geographic_scope)) +
  scale_fill_manual(values = colorBlindGrey8) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage (%)", fill = "Geographic Scope") +
  facet_grid(. ~ facet_group, scales = "free", space='free') +
  scale_x_discrete(expand = c(0, 0.1), labels = country_labels)+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))

#Two income groups
percentage_scope_by_country_income <- percentage_scope_by_country_income %>%
  mutate(
    facet_group = ifelse(is.na(simple_income_group) & Country == "Average", 
                         Country, 
                         simple_income_group) )

percentage_scope_by_country_income <- percentage_scope_by_country_income %>%
  mutate(facet_group = factor(facet_group, levels = c("Average", "Lower income", "Higher income")))

percentage_scope_by_country_income <- percentage_scope_by_country_income %>%
  mutate(Country = factor(Country, levels = ordered_country_average))


scope_colours <- c("Global" = "#0072B2",         
                    "International" = "#56B4E9",
                    "Regional" = "#009E73",     
                    "National" = "#D55E00", 
                    "Subnational" = "#E69F00")

ggplot(percentage_scope_by_country_income, aes(x = Country, y = percentage, fill = Geographic_scope)) +
  scale_fill_manual(values = scope_colours) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of Geographic Scope addressed (%)", fill = "Geographic Scope") +
  facet_grid(. ~ facet_group, scales = "free", space='free') +
  scale_x_discrete(expand = c(0, 0.1), labels = country_labels)+
  theme_classic() +
  theme(strip.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(size = 13),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        panel.spacing.x = unit(1, "cm"))




