#Authors
library(tidyr)
library(ggplot2)
library(dplyr)

#Clean NAs from author
Thesis_dataset_actual_clean_author <- Thesis_dataset_actual %>% filter(Author_indiv != "NA") #should only need to filter one as NA should be accross all

Thesis_dataset_actual_clean_author <- Thesis_dataset_actual_clean_author %>%
  mutate(across(c(Author_indiv, Author_localngo, Author_localgov, Author_internationalgov, Author_INGO, Author_IGO), ~ as.numeric(.)))

### Authors For all references

#count different author cateogires
author_counts <- Thesis_dataset_actual_clean_author %>%
  group_by(Country) %>%
  summarise(across(c(Author_indiv, Author_localngo, Author_localgov, Author_internationalgov, Author_INGO, Author_IGO), sum, na.rm = TRUE))

author_counts_long <- author_counts %>%
  pivot_longer(cols = -Country, names_to = "author_type", values_to = "count")

#plot different author cateogries by country
ggplot(author_counts_long, aes(x = Country, y = count, fill = author_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Author types by Country", x = "Country", y = "Count") +
  theme_classic()

percentage_author_by_country <- author_counts_long %>%
  group_by(Country) %>%
  mutate(total_count = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = count / total_count * 100)

ggplot(percentage_author_by_country, aes(x = Country, y = percentage, fill = author_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage (%)", fill = "Author Type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))

###Accessibility for all references
Thesis_dataset_actual_clean_access <- Thesis_dataset_actual %>% filter(Accessibility != "NA")

access_freq <- as.data.frame(table(Thesis_dataset_actual_clean_access$Accessibility, Thesis_dataset_actual_clean_access$Country))
colnames(access_freq) <- c("Accessibility", "Country", "Frequency")
access_freq$Accessibility <- as.character(access_freq$Accessibility)

#Counts of each lit_type per country
access_freq <- Thesis_dataset_actual_clean_access %>%
  count(Country, Accessibility)

# Create stacked bar chart
ggplot(access_freq, aes(x = Country, y = n, fill = Accessibility)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Frequency", fill = "Accessibility of the Reference") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))


percentage_access_by_country <- access_freq %>%
  group_by(Country) %>%
  mutate(total_count = sum(n)) %>%
  ungroup() %>%
  mutate(percentage = n / total_count * 100)


ggplot(percentage_access_by_country, aes(x = Country, y = percentage, fill = Accessibility)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage", fill = "Accessibility of the Reference") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))

        
        

###Accessbility of Research

Thesis_dataset_actual_clean_access <- Thesis_dataset_actual %>% filter(Accessibility != "NA")
#Include Book_chap in Book classificaiton
filtered_lit_refs1 <- Thesis_dataset_actual_clean_access %>%
  mutate(Lit_type = ifelse(Lit_type == "Book_chap", "Book", Lit_type))

#Add Research from Grey_lit_type as a lit type 
filtered_lit_refs2 <- Thesis_dataset_actual_clean_access %>%
  filter(Grey_lit_type %in%  c("Report", "Conference_paper","Journal_npr","Thesis")) %>%
  mutate(Lit_type = case_when(
    Grey_lit_type == "Report" ~ "Report",
    Grey_lit_type == "Conference_paper" ~ "Conference_paper",
    Grey_lit_type == "Journal_npr" ~ "Journal_npr",
    Grey_lit_type == "Thesis" ~ "Thesis"))

#Combine the two datasets
filtered_lit_ref_combined <- bind_rows(filtered_lit_refs1, filtered_lit_refs2)



# Filter for relevant literature types
filtered_lit_refs <- filtered_lit_ref_combined %>%
  filter(Lit_type %in% c("Journal", "Book", "Report","Conference_paper","Journal_npr", "Thesis"))

accessibility_summary_selectedlit <- filtered_lit_refs %>%
  group_by(Lit_type, Accessibility) %>%
  summarise(Frequency = n(), .groups = "drop")

ggplot(accessibility_summary_selectedlit, aes(x = Lit_type, y = Frequency, fill = Accessibility)) +
  geom_bar(stat = "identity") +
  labs(x = "Literature Type", y = "Frequency", fill = "Accessibility of the Reference") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

accessibility_summary_selectedlit_country <- filtered_lit_refs %>%
  group_by(Lit_type, Accessibility,Country) %>%
  summarise(Frequency = n(), .groups = "drop")

percentage_access_by_country_selected_litmodel <- accessibility_summary_selectedlit_country %>%
  group_by(Lit_type) %>%
  mutate(total_count = sum(Frequency)) %>%
  ungroup() %>%
  mutate(percentage = Frequency / total_count * 100)

access_labels <- c(Free = "Free", Inaccessible = "Not available online", Paid = "Paid")

access_colours <- c(Free = "darkgreen", Inaccessible = "darkred", Paid = "orange2")

percentage_access_by_country_selected_litmodel$Accessibility <- factor(percentage_access_by_country_selected_litmodel$Accessibility, levels = c("Free", "Paid", "Inaccessible"))

#look at percentages of different lit types
accessibility_summary_selectedlit_extractpercentages <- filtered_lit_refs %>%
  group_by(Lit_type, Accessibility) %>%
  summarise(Frequency = n(), .groups = "drop")

percentage_access_selected_litmodel_extractpercentages <- accessibility_summary_selectedlit_extractpercentages %>%
  group_by(Lit_type) %>%
  mutate(total_count = sum(Frequency)) %>%
  ungroup() %>%
  mutate(percentage = Frequency / total_count * 100)

#Accessibility per Literature type percentage

#use scope order
percentage_access_by_country_selected_litmodel$Lit_type <- factor(percentage_access_by_country_selected_litmodel$Lit_type,
levels = combined_percentages$Lit_type[order(-combined_percentages$combined_percentage)])

ggplot(percentage_access_by_country_selected_litmodel, aes(x = Lit_type, y = percentage, fill = Accessibility)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = access_colours, labels = access_labels) +
  scale_x_discrete(expand = c(0, 0.5), labels = lit_labels) +
  labs(x = "Literature Type", y = "Percentage of Research references (%)", fill = "Accessibility of the Reference") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15))

average_access_type <- accessibility_summary_selectedlit_country %>%
  group_by(Accessibility) %>%
  summarise(Frequency = mean(Frequency)) %>%
  mutate(Country = "Average") # Assign a new "Country" value for the average bar

#Combine the original data with the average data
accessibility_summary_selectedlit_country2 <- bind_rows(accessibility_summary_selectedlit_country, average_access_type)


percentage_access_by_country_selected_litmodel2 <- accessibility_summary_selectedlit_country2 %>%
  group_by(Country) %>%
  mutate(total_count = sum(Frequency)) %>%
  ungroup() %>%
  mutate(percentage = Frequency / total_count * 100)

percentage_access_by_country_selected_litmodel2 <-percentage_access_by_country_selected_litmodel2 %>% #add socioeconomic data
  left_join(protected_area_income_epi, by = c("Country" = "Country"))


percentage_access_by_country_selected_litmodel2 <- percentage_access_by_country_selected_litmodel2 %>%
  mutate(Facet_Group_Derived = ifelse(Country == "Average", "Average", "Individual Countries"))

percentage_access_by_country_selected_litmodel2 <- percentage_access_by_country_selected_litmodel2 %>%
  mutate(
    facet_group = ifelse(is.na(simple_income_group) & Country == "Average", 
                         Country, 
                         simple_income_group) )

percentage_access_by_country_selected_litmodel2 <- percentage_access_by_country_selected_litmodel2 %>%
  mutate(facet_group = factor(facet_group, levels = c("Average", "Lower income", "Higher income")))

percentage_access_by_country_selected_litmodel2 <- percentage_access_by_country_selected_litmodel2 %>%
  mutate(Country = factor(Country, levels = ordered_country_average))

percentage_access_by_country_selected_litmodel2$Accessibility <- factor(percentage_access_by_country_selected_litmodel2$Accessibility, levels = c("Free", "Paid", "Inaccessible"))


ggplot(percentage_access_by_country_selected_litmodel2, aes(x = Country, y = percentage, fill = Accessibility)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of Research references (%)", fill = "Accessibility of the Reference") +
  facet_grid(. ~ facet_group, scales = "free", space='free') +
  scale_fill_manual(values = access_colours, labels = access_labels) +
  scale_x_discrete(expand = c(0, 0.5), labels = country_labels)+
  theme_classic() +
  theme(strip.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        panel.spacing.x = unit(1, "cm"))


#References location
filtered_lit_refs_reflocation <- filtered_lit_refs %>%
  mutate(in_text = as.numeric(In_text),
        ref_list = as.numeric(Ref_list))

filtered_lit_refs_reflocation <- filtered_lit_refs_reflocation %>%
  mutate(citation_type = case_when(
    In_text == 1 & Ref_list == 0 ~ "In-text only",
    In_text == 0 & Ref_list == 1 ~ "Reference list only",
    In_text == 1 & Ref_list == 1 ~ "In-text and reference list",
    TRUE ~ "Other"))  # Catch any unexpected case


citation_by_country <- filtered_lit_refs_reflocation %>%
  group_by(Country, citation_type) %>%
  summarise(count = n(), .groups = "drop")

citation_overall <- filtered_lit_refs_reflocation %>%
  group_by(citation_type) %>%
  summarise(count = n(), .groups = "drop")




# Create stacked bar chart
ggplot(citation_by_country, aes(x = Country, y = count, fill = citation_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Frequency", fill = "Location of reference") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))


#Keywords by author type

authors_long <- keywords_long %>%
  pivot_longer(cols = starts_with("Author_"), 
    names_to = "Author_type", 
    values_to = "is_present") %>%
  filter(is_present == 1)

keyword_counts_author <- authors_long %>%
  group_by(Author_type, keyword) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))


##Try just top 5
top5_keywords_by_author <- keyword_counts_author %>%
  group_by(Author_type) %>%
  slice_max(order_by = count, n = 5, with_ties = FALSE) %>%
  ungroup() 

ggplot(top5_keywords_by_author, aes(x = keyword, y = count, fill = keyword)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ Author_type, scales = "free_x") +
  labs(title = "Top 5 Keywords by Author Type",
       x = "Keyword",
       y = "Count") +
  theme_classic() 


#Try percentage
keyword_percentage_by_author <- keyword_counts_author %>%
  group_by(Author_type) %>%
  mutate(total_count = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = count / total_count * 100)

# Get top 5 keywords per literature type based on percentage
top5_keywords_by_author_percentage <- keyword_percentage_by_author %>%
  group_by(Author_type) %>%
  slice_max(order_by = percentage, n = 5, with_ties = FALSE) %>%
  ungroup() 

ggplot(top5_keywords_by_author_percentage, aes(x = keyword, y = percentage, fill = keyword)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ Author_type, scales = "free_x") +
  labs(title = "Top 5 Keywords by Author Type",
       x = "Keyword",
       y = "Percentage (%)") +
  theme_classic() 

###Author Type for selected lit types ## MUST RUN TO REPLACE OTHER FLILTERED LITS
#Include Book_chap in Book classificaiton

filtered_lit_refs1 <- Thesis_dataset_actual_clean_author %>%
  mutate(Lit_type = ifelse(Lit_type == "Book_chap", "Book", Lit_type))

#Add Report from Grey_lit_type as a lit type 
filtered_lit_refs2 <- Thesis_dataset_actual_clean_author %>%
  filter(Grey_lit_type %in%  c("Report", "Conference_paper","Journal_npr","Thesis")) %>%
  mutate(Lit_type = case_when(
    Grey_lit_type == "Report" ~ "Report",
    Grey_lit_type == "Conference_paper" ~ "Conference_paper",
    Grey_lit_type == "Journal_npr" ~ "Journal_npr",
    Grey_lit_type == "Thesis" ~ "Thesis"))

#Combine the two datasets
filtered_lit_ref_combined <- bind_rows(filtered_lit_refs1, filtered_lit_refs2)

# Filter for relevant literature types
filtered_lit_refs <- filtered_lit_ref_combined %>%
  filter(Lit_type %in% c("Journal", "Book", "Report","Conference_paper","Journal_npr", "Thesis"))

filtered_lit_refs_author <- filtered_lit_refs %>%
  mutate(across(c(Author_indiv, Author_localngo, Author_localgov, Author_internationalgov, Author_INGO, Author_IGO), ~ as.numeric(.)))

# Step 1 Calculate the sum of 1s for each row and then divide each 1 by that sum
proportional_author_contributions <- filtered_lit_refs_author %>%
  # Identify the author columns
  mutate(
    author_cols_sum = select(., starts_with("Author_")) %>% rowSums(na.rm = TRUE)
  ) %>%
  # Apply the division to each author column
  mutate(across(starts_with("Author_"), ~ if_else(author_cols_sum > 0, . / author_cols_sum, 0))) %>%# Handle cases where sum is 0 to avoid NaN
  select(-author_cols_sum)#remove author_cols_sum column


# Step 2 Group by Lit_type and sum the proportional contributions
selected_lit_author_proportions_summed <- proportional_author_contributions %>%
  group_by(Lit_type) %>%
  summarise(across(starts_with("Author_"), sum, na.rm = TRUE))


# Step 3 Pivot longer
selected_lit_author_proportions_long <- selected_lit_author_proportions_summed %>%
  pivot_longer(cols = -Lit_type, names_to = "author_type", values_to = "proportional_count")




#plot different author cateogries by country
ggplot(selected_lit_author_proportions_long, aes(x = Lit_type, y = proportional_count, fill = author_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Author types by Literature type", x = "Literature Type", y = "Frequency") +
  theme_classic()

selected_lit_author_counts_percentage <- selected_lit_author_counts_long %>%
  group_by(Lit_type) %>%
  mutate(total_count = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = count / total_count * 100)

ggplot(selected_lit_author_counts_percentage, aes(x = Lit_type, y = percentage, fill = author_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Literature Type", y = "Percentage of Author types (%)", fill = "Author Type") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15))


#Plot for author types regarding just these literature types by country
selected_lit_author_proportions_summed_country <- proportional_author_contributions %>%
  group_by(Country) %>%
  summarise(across(starts_with("Author_"), sum, na.rm = TRUE))


selected_lit_author_proportions_summed_country_long <- selected_lit_author_proportions_summed_country %>%
  pivot_longer(cols = -Country, names_to = "author_type", values_to = "proportional_count")



#Get average proportions
average_author_type <- selected_lit_author_proportions_summed_country_long %>%
  group_by(author_type) %>%
  summarise(proportional_count = mean(proportional_count)) %>%
  mutate(Country = "Average") # Assign a new "Country" value for the average bar


#Combine the original data with the average data
selected_lit_author_proportions_summed_country <- bind_rows(selected_lit_author_proportions_summed_country_long, average_author_type)

#make into percentage
selected_lit_author_proportions_percentage_country <- selected_lit_author_proportions_summed_country %>%
  group_by(Country) %>%
  mutate(total_count = sum(proportional_count)) %>%
  ungroup() %>%
  mutate(percentage = proportional_count / total_count * 100)

selected_lit_author_proportions_percentage_country <-selected_lit_author_proportions_percentage_country %>% #add socioeconomic data
  left_join(protected_area_income_epi, by = c("Country" = "Country"))



custom_country_order <- c(
  "Eritrea", "Liberia", "Somalia", "Mozambique",
  "Azerbaijan", "Bosnia_Herzegovina", "Brazil", "Dominica", "Lao_PDR", "Lesotho", "Mauritius", "Nepal", "Serbia", "Saint_Vincent_Grenadines", "Suriname", "Vanuatu",
  "Australia", "Austria", "Belgium", "Denmark", "Estonia", "Hungary", "Liechtenstein", "Lithuania", "New_Zealand", "Palau", "Saudi_arabia", "Singapore", "St_Kitts_Nevis", "Trinidad_Tobago", "United_Kingdom"
)




selected_lit_author_proportions_percentage_country <- selected_lit_author_proportions_percentage_country %>%
  mutate(Facet_Group_Derived = ifelse(Country == "Average", "Average", "Individual Countries"))

selected_lit_author_proportions_percentage_country <- selected_lit_author_proportions_percentage_country %>%
  mutate(
    facet_group = ifelse(is.na(income_group) & Country == "Average", 
                         Country, 
                         income_group) )



selected_lit_author_proportions_percentage_country <- selected_lit_author_proportions_percentage_country %>%
  mutate(facet_group = factor(facet_group, levels = c("Average", "Higher income", "Middle Income", "Lower income")))



ggplot(selected_lit_author_proportions_percentage_country, aes(x = Country, y = percentage, fill = author_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of Author Types", fill = "Author Type") +
  facet_grid(. ~ facet_group, scales = "free", space='free') +
  scale_x_discrete(expand = c(0, 0.1))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        panel.spacing.x = unit(1, "cm"))


###Reports only ###
selected_lit_author_proportions_summed_country_report <- proportional_author_contributions %>%
  filter(Lit_type == "Report") %>%
  group_by(Country) %>%
  summarise(across(starts_with("Author_"), sum, na.rm = TRUE))


selected_lit_author_proportions_summed_country_report_long <- selected_lit_author_proportions_summed_country_report %>%
  pivot_longer(cols = -Country, names_to = "author_type", values_to = "proportional_count")



#Get average proportions
average_author_type_report <- selected_lit_author_proportions_summed_country_report_long %>%
  group_by(author_type) %>%
  summarise(proportional_count = mean(proportional_count)) %>%
  mutate(Country = "Average") # Assign a new "Country" value for the average bar


#Combine the original data with the average data
selected_lit_author_proportions_summed_country_report <- bind_rows(selected_lit_author_proportions_summed_country_report_long, average_author_type_report)

#make into percentage
selected_lit_author_proportions_percentage_country_report <- selected_lit_author_proportions_summed_country_report %>%
  group_by(Country) %>%
  mutate(total_count = sum(proportional_count)) %>%
  ungroup() %>%
  mutate(percentage = proportional_count / total_count * 100)

selected_lit_author_proportions_percentage_country_report <-selected_lit_author_proportions_percentage_country_report %>% #add socioeconomic data
  left_join(protected_area_income_epi, by = c("Country" = "Country"))


selected_lit_author_proportions_percentage_country_report <- selected_lit_author_proportions_percentage_country_report %>%
  mutate(Facet_Group_Derived = ifelse(Country == "Average", "Average", "Individual Countries"))


#For 2 income groups
selected_lit_author_proportions_percentage_country_report <- selected_lit_author_proportions_percentage_country_report %>%
  mutate(
    facet_group = ifelse(is.na(simple_income_group) & Country == "Average", 
                         Country, 
                         simple_income_group) )

selected_lit_author_proportions_percentage_country_report <- selected_lit_author_proportions_percentage_country_report %>%
  mutate(facet_group = factor(facet_group, levels = c("Average", "Lower income", "Higher income")))
selected_lit_author_proportions_percentage_country_report <- selected_lit_author_proportions_percentage_country_report %>%
  mutate(Country = factor(Country, levels = ordered_country_average))


author_colours <- c("Author_IGO" = "#1f78b4",         
                "Author_internationalgov" = "#1f55b3",
                "Author_INGO" = "#33a02c",     
                "Author_localngo" = "#4bc90c", 
                "Author_localgov" = "#c24040",
                "Author_indiv" = "darkorchid4")

author_labels <- c("Author_IGO" = "Intergovernmental Organisation",
                "Author_indiv" = "Individual",
                "Author_INGO" = "International NGO",
                "Author_internationalgov" = "International Government",
                "Author_localgov" = "Local Government",
                "Author_localngo" = "Local NGO")

author_order <- c("Author_IGO", "Author_internationalgov","Author_INGO",
                  "Author_localngo","Author_localgov","Author_indiv")
selected_lit_author_proportions_percentage_country_report$author_type <- factor(selected_lit_author_proportions_percentage_country_report$author_type,
                                                                                levels = author_order)


ggplot(selected_lit_author_proportions_percentage_country_report, aes(x = Country, y = percentage, fill = author_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of Author Types", fill = "Author Type") +
  scale_fill_manual(values = author_colours, labels = author_labels)+
  facet_grid(. ~ facet_group, scales = "free", space='free') +
  scale_x_discrete(expand = c(0, 0.1), labels = country_labels)+
  theme_classic() +
  theme(strip.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.spacing.x = unit(1, "cm"))

### Simplified catagories
simplified_selected_lit_author_proportions_country_report <- selected_lit_author_proportions_summed_country_report %>%
  mutate(
    author_type = case_when(
      author_type %in% c("Author_IGO", "Author_internationalgov") ~ "International_gov",
      author_type == "Author_indiv" ~ "Author_indiv",
      author_type == "Author_localgov" ~ "Author_localgov",
      author_type %in% c("Author_INGO", "Author_localngo") ~ "NGO"
    )) %>%
  group_by(Country, author_type) %>%
  summarise(
    proportional_count = sum(proportional_count),
    .groups = "drop" ) %>%
  ungroup()


simplified_selected_lit_author_proportions_percentage_country_report <- simplified_selected_lit_author_proportions_country_report %>% 
  group_by(Country) %>%
  mutate(total_count = sum(proportional_count)) %>%
  ungroup() %>%
  mutate(percentage = proportional_count / total_count * 100)

simplified_selected_lit_author_proportions_percentage_country_report <-simplified_selected_lit_author_proportions_percentage_country_report %>% #add socioeconomic data
  left_join(protected_area_income_epi, by = c("Country" = "Country"))


simplified_selected_lit_author_proportions_percentage_country_report <- simplified_selected_lit_author_proportions_percentage_country_report %>%
  mutate(Facet_Group_Derived = ifelse(Country == "Average", "Average", "Individual Countries"))

simplified_selected_lit_author_proportions_percentage_country_report <- simplified_selected_lit_author_proportions_percentage_country_report %>%
  mutate(
    facet_group = ifelse(is.na(simple_income_group) & Country == "Average", 
                         Country, 
                         simple_income_group) )

simplified_selected_lit_author_proportions_percentage_country_report <- simplified_selected_lit_author_proportions_percentage_country_report %>%
  mutate(facet_group = factor(facet_group, levels = c("Average", "Lower income","Higher income")))

simplified_selected_lit_author_proportions_percentage_country_report <- simplified_selected_lit_author_proportions_percentage_country_report %>%
  mutate(Country = factor(Country, levels = ordered_country_average))

author_colours2 <- c("Author_indiv" = "#1f78b4",
                     "Author_localgov" = "#c24040",
                     "International_gov" = "#33a02c",
                     "NGO" = "darkorchid4")

author_labels2 <- c("Author_indiv" = "Individual",
                    "Author_localgov" = "Local Government",
                   "International_gov" = "International Government",
                   "NGO" = "NGO")


ggplot(simplified_selected_lit_author_proportions_percentage_country_report, aes(x = Country, y = percentage, fill = author_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Percentage of Author Types (%)", fill = "Author Type") +
  scale_fill_manual(values = author_colours2, labels = author_labels2)+
  facet_grid(. ~ facet_group, scales = "free", space='free') +
  theme_classic() +
  scale_x_discrete(expand = c(0, 0.1), labels = country_labels)+
  theme_classic() +
  theme(strip.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.spacing.x = unit(1, "cm"))
