library(readxl)
library(RColorBrewer)
library(ggplot2)


#Import the data
Thesis_dataset_actual <- read_excel("~/Library/CloudStorage/OneDrive-NewcastleUniversity/Thesis/Thesis_dataset_actual.xlsx")

#Exclude NA values 
references_clean_lit <- Thesis_dataset_actual %>% filter(Lit_type != "NA")

#Calculate frequencies of literature types for each country
ref_types_freq <- as.data.frame(table(references_clean_lit$Lit_type, references_clean_lit$Country))
colnames(ref_types_freq) <- c("Lit_type", "Country", "Frequency")
ref_types_freq$Lit_type <- as.character(ref_types_freq$Lit_type)

# Create the bar plot with facet_wrap
ggplot(ref_types_freq, aes(x = Lit_type, y = Frequency, fill = Country)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Country, ncol = 2) + # Creates separate plots for each country
  labs(
    x = "Literature Type",
    y = "Frequency",
    fill = "Country"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


#stacked bar chart
#Counts of each lit_type per country
lit_freq <- references_clean_lit %>%
  count(Country, Lit_type)

#Get percentage proportion
percentage_littype_by_country <- lit_freq %>%
  group_by(Country) %>%
  mutate(total_count = sum(n)) %>%
  ungroup() %>%
  mutate(percentage = n / total_count * 100)

# Create stacked bar chart
ggplot(percentage_littype_by_country, aes(x = Country, y = percentage, fill = Lit_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of references (%)", fill = "Literature Type") +
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))

###Clean the grey_lit column
references_clean_grey_lit1 <- Thesis_dataset_actual %>% filter(!is.na(Grey_lit_type))
references_clean_grey_lit <- references_clean_grey_lit1 %>% filter(Grey_lit_type != "NA")

#Calculate frequencies of grey literature types for each country
ref_types_freq <- as.data.frame(table(references_clean_lit$Grey_lit_type, references_clean_lit$Country))
colnames(ref_types_freq) <- c("Grey_lit_type", "Country", "Frequency")
ref_types_freq$Grey_lit_type <- as.character(ref_types_freq$Grey_lit_type)

#stacked bar chart
#Counts of each lit_type per country
grey_lit_freq <- references_clean_grey_lit %>%
  count(Country, Grey_lit_type)

# Create stacked bar chart
ggplot(grey_lit_freq, aes(x = Country, y = n, fill = Grey_lit_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Frequency", fill = "Literature Type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))

#Get percentage proportion of grey lit
percentage_greylittype_by_country <- grey_lit_freq %>%
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
#MAKE SURE TO RUN TO REPLACE OTHER FILTERED LIT

#Include Book_chap in Book classificaiton
filtered_lit_refs1 <- references_clean_lit %>%
  mutate(Lit_type = ifelse(Lit_type == "Book_chap", "Book", Lit_type))


overall_research <- filtered_lit_refs1 %>%
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


ggplot(percentage_research_by_selected_countries, aes(x = Country, y = percentage, fill = overall_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of references", fill = "Literature Type") +
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))


###Research types percentage by country ###
selected_lit_freq <- filtered_lit_refs %>%
  count(Country, Lit_type)

average_research_type <- selected_lit_freq %>%
  group_by(Lit_type) %>%
  summarise(n = mean(n)) %>%
  mutate(Country = "Average") # Assign a new "Country" value for the average bar


#Combine the original data with the average data
selected_lit_freq <- bind_rows(selected_lit_freq, average_research_type)


#Get percentage proportion of research literature types
percentage_research_types_by_country <- selected_lit_freq %>%
  group_by(Country) %>%
  mutate(total_count = sum(n)) %>%
  ungroup() %>%
  mutate(percentage = n / total_count * 100)


percentage_research_types_by_country <-percentage_research_types_by_country %>% #add socioeconomic data
  left_join(protected_area_income_epi, by = c("Country" = "Country"))



custom_country_order <- c(
  "Eritrea", "Liberia", "Somalia", "Mozambique",
  "Azerbaijan", "Bosnia_Herzegovina", "Brazil", "Dominica", "Lao_PDR", "Lesotho", "Mauritius", "Nepal", "Serbia", "Saint_Vincent_Grenadines", "Suriname", "Vanuatu",
  "Australia", "Austria", "Belgium", "Denmark", "Estonia", "Hungary", "Liechtenstein", "Lithuania", "New_Zealand", "Palau", "Saudi_arabia", "Singapore", "St_Kitts_Nevis", "Trinidad_Tobago", "United_Kingdom"
)


percentage_research_types_by_country <- percentage_research_types_by_country %>%
  mutate(Facet_Group_Derived = ifelse(Country == "Average", "Average", "Individual Countries"))



#Create stacked bar chart for percentages of 3 income groups
percentage_research_types_by_country3 <- percentage_research_types_by_country %>%
  mutate(
    facet_group = ifelse(is.na(income_group) & Country == "Average", 
                         Country, 
                         income_group) )

percentage_research_types_by_country3 <- percentage_research_types_by_country3 %>%
  mutate(facet_group = factor(facet_group, levels = c("Average", "Higher income", "Middle Income", "Lower income")))

ggplot(percentage_research_types_by_country3, aes(x = Country, y = percentage, fill = Lit_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of references (%)", fill = "Literature Type") +
  scale_fill_brewer(palette = "Paired") +
  facet_grid(. ~ facet_group, scales = "free", space='free') +
  scale_x_discrete(expand = c(0, 0.1))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        panel.spacing.x = unit(1, "cm"))



# Create stacked bar chart for percentages of 2 income groups
percentage_research_types_by_country2 <- percentage_research_types_by_country %>%
  mutate(
    facet_group = ifelse(is.na(simple_income_group) & Country == "Average", 
                         Country, 
                         simple_income_group) )

#make levels without average
ordered_country_no_average <- percentage_research_types_by_country2 %>%
  filter(Country != "Average") %>% # Exclude "Average" from the sorting process
  mutate(Country = fct_reorder(Country, NY.GNP.PCAP.CD, .desc = FALSE)) %>%
  pull(Country) %>% # Extract the factor column
  levels() # Get the ordered levels

#Add in average
ordered_country_average <- c(ordered_country_no_average, "Average")

percentage_research_types_by_country2 <- percentage_research_types_by_country2 %>%
  mutate(facet_group = factor(facet_group, levels = c("Average", "Lower income","Higher income")))

percentage_research_types_by_country2 <- percentage_research_types_by_country2 %>%
  mutate(Country = factor(Country, levels = ordered_country_average))


lit_legend_order <- c("Book",
  "Conference_paper",
  "Journal",
  "Journal_npr",
  "Thesis",
  "Report")

percentage_research_types_by_country2$Lit_type <- factor(percentage_research_types_by_country2$Lit_type, levels = lit_legend_order)

lit_colours <- c("Book" = "#2ecc71",         
  "Conference_paper" = "#1f77b4",
  "Journal" = "#4A90C2",     
  "Journal_npr" = "#7FB3D5", 
  "Thesis" = "#B2D0EA",
  "Report" = "#e74c3c")

lit_labels <- c("Book" = "Book",
  "Conference_paper" = "Conference Paper",
  "Journal" = "Journal",
  "Journal_npr" = "Journal NPR",
  "Thesis" = "Thesis",
  "Report" = "Report")

ggplot(percentage_research_types_by_country2, aes(x = Country, y = percentage, fill = Lit_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of Research references (%)", fill = "Literature Type") +
  scale_fill_manual(values = lit_colours, labels = lit_labels) +
  facet_grid(. ~ facet_group, scales = "free", space='free') +
  scale_x_discrete(expand = c(0, 0.1), labels = country_labels)+
  theme_classic() +
  theme(strip.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        panel.spacing.x = unit(1, "cm"))



  

