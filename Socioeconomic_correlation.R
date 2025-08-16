library(vcd)
library(rcompanion)

#proportion of Journal and book references 
target_types <- c("Book", "Book_chap","Journal")

proportions_df <- Thesis_dataset_actual %>%
  group_by(Country) %>%
  summarize(
    total_refs = n(),
    target_refs = sum(Lit_type %in% target_types),
    proportion = target_refs / total_refs
  ) %>%
  ungroup()



#Proportion vs GDP #issue with azerbaijan being 0%? insig
proportion_test <-proportions_df %>% #add socioeconomic data
  left_join(protected_area_income_epi, by = c("Country" = "Country"))

#insig
journalbook_proportion_income_model <- lm(proportion ~ NY.GDP.PCAP.CD, data = proportion_test)
summary(journalbook_proportion_income_model)

#Proportion vs income group insig
t.test(proportion ~ income_group, data = proportion_test)


#proportion just journal 
target_journal <- "Journal"

proportions_df_journal <- Thesis_dataset_actual %>%
  group_by(Country) %>%
  summarize(
    total_refs = n(),
    target_refs = sum(Lit_type %in% target_journal),
    proportion = target_refs / total_refs
  ) %>%
  ungroup()

#Proportion vs GDP #issue with azerbaijan being 0%? insig
proportion_test_journal <-proportions_df_journal %>% #add socioeconomic data
  left_join(protected_area_income_epi, by = c("Country" = "Country"))

#insig
journal_proportion_income_model <- lm(proportion ~ NY.GDP.PCAP.CD, data = proportion_test_journal)
summary(journal_proportion_income_model)

#Proportion vs income group insig
journal_proportion_income_model2 <- lm(proportion ~ income_group, data = proportion_test_journal)
summary(journal_proportion_income_model2)

#proportion just book
target_book <- c("Book", "Book_chap")

proportions_df_book <- Thesis_dataset_actual %>%
  group_by(Country) %>%
  summarize(
    total_refs = n(),
    target_refs = sum(Lit_type %in% target_book),
    proportion = target_refs / total_refs
  ) %>%
  ungroup()

#Proportion vs GDP #issue with azerbaijan being 0%? insig
proportion_test_book <-proportions_df_book %>% #add socioeconomic data
  left_join(protected_area_income_epi, by = c("Country" = "Country"))

#insig
book_proportion_income_model <- lm(proportion ~ NY.GDP.PCAP.CD, data = proportion_test_book)
summary(book_proportion_income_model)

#Proportion vs income group insig
t.test(proportion ~ income_group, data = proportion_test_book)







#Proportion of International authors (IGO INGO Intenational_gov) 

#for all refs
summed_author_percentages <- selected_lit_author_proportions_percentage_country %>%
  filter(author_type %in% c("Author_INGO", "Author_internationalgov", "Author_IGO")) %>%
  filter(Country != "Average") %>%
  group_by(Country) %>%
  summarize(total_percentage = sum(percentage))

proportion_author_test <-protected_area_income_epi %>% #add socioeconomic data
  left_join(summed_author_percentages, by = c("Country" = "Country"))
 
#insig
international_author_model <- lm(total_percentage ~ NY.GDP.PCAP.CD, data = proportion_author_test)
summary(international_author_model)

#Proportion vs income group #sig slightly higher in higher income

t.test(total_percentage ~ income_group, data = proportion_author_test)


###All 'research' literature

summed_selectedlit_author_percentages<- selected_lit_author_counts_percentage %>%
  filter(author_type %in% c("Author_INGO", "Author_internationalgov", "Author_IGO")) %>%
  group_by(Country) %>%
  summarize(total_percentage = sum(percentage))

proportion_selectedlit_author_test <-protected_area_income_epi %>% #add socioeconomic data
  left_join(summed_selectedlit_author_percentages, by = c("Country" = "Country"))

#insig
international_author_selectedlit_model <- lm(total_percentage ~ NY.GDP.PCAP.CD, data = proportion_selectedlit_author_test)
plot(international_author_selectedlit_model)
summary(international_author_selectedlit_model)

plot(NY.GDP.PCAP.CD ~ total_percentage, data = proportion_selectedlit_author_test)
#Proportion vs income group #insig
t.test(total_percentage ~ income_group, data = proportion_selectedlit_author_test)

### Chi square author###
count_author_model <-protected_area_income_epi %>% #add socioeconomic data
  left_join(selected_lit_author_proportions_summed_country_report, by = c("Country" = "Country"))  %>%
  filter(!is.na(proportional_count)) 

aggregated_author_count <- count_author_model %>%
  filter(Country != "Average") %>% # Exclude the 'Average' group
  group_by(simple_income_group, author_type) %>%
  summarise(
    Total_count = sum(proportional_count, na.rm = TRUE), # Sum up the percentages
    .groups = 'drop')

contingency_table_for_chi2_author <- aggregated_author_count %>%
  pivot_wider(
    names_from = author_type,
    values_from = Total_count,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "simple_income_group") 

chi_square_result_author <- chisq.test(contingency_table_for_chi2_author)
print(chi_square_result_author) 
chi_square_result_author$residuals


### Chi square author simple### MAKE SURE TO RERUN HERE
count_author_model <-protected_area_income_epi %>% #add socioeconomic data
  left_join(simplified_selected_lit_author_proportions_country_report, by = c("Country" = "Country"))  %>%
  filter(!is.na(proportional_count)) 

aggregated_author_count <- count_author_model %>%
  filter(Country != "Average") %>% # Exclude the 'Average' group
  group_by(simple_income_group, author_type) %>%
  summarise(
    Total_count = sum(proportional_count, na.rm = TRUE), # Sum up the percentages
    .groups = 'drop')

contingency_table_for_chi2_author <- aggregated_author_count %>%
  pivot_wider(
    names_from = author_type,
    values_from = Total_count,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "simple_income_group") 

chi_square_result_author <- chisq.test(contingency_table_for_chi2_author)
print(chi_square_result_author) 
chi_square_result_author$residuals



###proportion of free references 
accessibility_summary_research <- filtered_lit_refs %>%
  group_by(Country,Accessibility) %>%
  summarise(Frequency = n(), .groups = "drop")

percentage_access_by_country_research <- accessibility_summary_research  %>%
  group_by(Country) %>%
  mutate(total_count = sum(Frequency)) %>%
  ungroup() %>%
  mutate(percentage = Frequency / total_count * 100)

percentage_access_by_country_free <- filter(percentage_access_by_country_research, Accessibility == "Free")
proportion_free_test <- protected_area_income_epi %>% #add socioeconomic data
  left_join(percentage_access_by_country_free, by = c("Country" = "Country"))

#insig
accessibility_model <- lm(percentage ~ income_group, data = proportion_free_test)
summary(accessibility_model)

plot(percentage ~ NY.GDP.PCAP.CD, data = proportion_free_test)

#Proportion vs income group #insig
t.test(percentage ~ income_group, data = proportion_free_test)

#proportion of free journals per country
accessibility_summary_research_littype <- filtered_lit_refs %>%
  group_by(Country,Lit_type,Accessibility) %>%
  summarise(Frequency = n(), .groups = "drop")

accessibility_summary_research_journal <- filter(accessibility_summary_research_littype, Lit_type =="Journal")

percentage_access_by_country_journal<- accessibility_summary_research_journal  %>%
  group_by(Country) %>%
  mutate(total_count = sum(Frequency)) %>%
  ungroup() %>%
  mutate(percentage = Frequency / total_count * 100)



#---- Percentage access by country journals

ggplot(percentage_access_by_country_journal, aes(x = Country, y = percentage, fill = Accessibility)) +
  geom_bar(stat = "identity") +
  labs(x = "Country",
       y = "Percentage of references") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13))

percentage_access_by_country_journal_free <- filter(percentage_access_by_country_journal, Accessibility == "Free")
percentage_access_by_country_journal_free_model <-protected_area_income_epi %>% #add socioeconomic data
  left_join(percentage_access_by_country_journal_free, by = c("Country" = "Country"))

accessibility_model <- lm(percentage ~ income_group, data = percentage_access_by_country_journal_free_model)
summary(accessibility_model) #insig but also low #

###Geographic scope with counts
count_scope_model <- scope_freq_country %>%
  filter(Country != "Average") %>% # Exclude the 'Average' group
  mutate( # Recode the "Geographic Scope" column
    Geographic_scope = case_when(
      Geographic_scope == "Subnational" ~ "National",
      Geographic_scope == "International" ~ "Global",
      # "Regional" keep them as they are
      TRUE ~ Geographic_scope)
  ) %>%
  group_by(Country, Geographic_scope) %>%
  summarise(
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = 'drop')
    

count_scope_model <-protected_area_income_epi %>% #add socioeconomic data
  left_join(count_scope_model, by = c("Country" = "Country"))  %>%
  filter(!is.na(Frequency)) 



#3 income groups
aggregated_scope_count <- count_scope_model %>%
  group_by(income_group, Geographic_scope) %>%
  summarise(
    Total_count = sum(Frequency, na.rm = TRUE), # Sum up the percentages
    .groups = 'drop'
  )

contingency_table_scope<- aggregated_scope_count %>%
  pivot_wider(
    names_from = Geographic_scope,
    values_from = Total_count,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "income_group") 

chi_square_scope_result <- chisq.test(contingency_table_scope)
chi_square_scope_result$residuals

#2 income groups (rerun)
aggregated_scope_count <- count_scope_model %>%
  group_by(simple_income_group, Geographic_scope) %>%
  summarise(
    Total_count = sum(Frequency, na.rm = TRUE), # Sum up the percentages
    .groups = 'drop'
  )

contingency_table_scope<- aggregated_scope_count %>%
  pivot_wider(
    names_from = Geographic_scope,
    values_from = Total_count,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "simple_income_group") 

chi_square_scope_result <- chisq.test(contingency_table_scope)
chi_square_scope_result$residuals


#2 income groups 5 scope types
scope_freq_country_model <-protected_area_income_epi %>% #add socioeconomic data
  left_join(scope_freq_country, by = c("Country" = "Country"))  %>%
  filter(!is.na(Frequency)) 

aggregated_scope_count_5 <- scope_freq_country_model %>%
  group_by(simple_income_group, Geographic_scope) %>%
  summarise(
    Total_count = sum(Frequency, na.rm = TRUE), # Sum up the percentages
    .groups = 'drop'
  )

contingency_table_scope_5<- aggregated_scope_count_5 %>%
  pivot_wider(
    names_from = Geographic_scope,
    values_from = Total_count,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "simple_income_group") 

chi_square_scope_result_5 <- chisq.test(contingency_table_scope_5)
chi_square_scope_result_5$residuals


#2 income groups 2 scope types
count_scope_model_two_types <- scope_freq_country %>%
  filter(Country != "Average") %>% # Exclude the 'Average' group
  mutate( # Recode the "Geographic Scope" column
    Geographic_scope = case_when(
      Geographic_scope == "Subnational" ~ "National",
      Geographic_scope %in% c("International", "Regional") ~ "Global",
      TRUE ~ Geographic_scope) # This line is a fallback, though less critical now
  ) %>%
  group_by(Country, Geographic_scope) %>%
  summarise(
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = 'drop'
  )

count_scope_model_two_types <-protected_area_income_epi %>% #add socioeconomic data
  left_join(count_scope_model_two_types, by = c("Country" = "Country"))  %>%
  filter(!is.na(Frequency)) 

aggregated_scope_count <- count_scope_model_two_types %>%
  group_by(simple_income_group, Geographic_scope) %>%
  summarise(
    Total_count = sum(Frequency, na.rm = TRUE), # Sum up the percentages
    .groups = 'drop'
  )

contingency_table_scope<- aggregated_scope_count %>%
  pivot_wider(
    names_from = Geographic_scope,
    values_from = Total_count,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "simple_income_group") 

chi_square_scope_result <- chisq.test(contingency_table_scope)
chi_square_scope_result$residuals



###Scope by lit type
count_scope_by_lit_type_model <- scope_freq_lit %>%
  mutate( # Recode the "Geographic Scope" column
    Geographic_scope = case_when(
      Geographic_scope == "Subnational" ~ "National",
      Geographic_scope == "International" ~ "Global",
      # "Regional" keep them as they are
      TRUE ~ Geographic_scope)
  ) %>%
  group_by(Lit_type, Geographic_scope) %>%
  summarise(
    Frequency = sum(Frequency, na.rm = TRUE),
    .groups = 'drop')

aggregated_scope_lit_count <- count_scope_by_lit_type_model %>%
  mutate(
    Lit_type_simplified = case_when(
      Lit_type %in% c("Journal", "Journal_npr", "Thesis", "Conference_paper") ~ "Journal",
      Lit_type == "Report" ~ "Report",
      Lit_type == "Book" ~ "Book")) %>%
  group_by(Geographic_scope, Lit_type_simplified) %>%
  summarise(
    Frequency = sum(Frequency),
    .groups = "drop" 
  ) %>%
  ungroup() 


contingency_table_scope_lit_type<- aggregated_scope_lit_count %>%
  pivot_wider(
    names_from = Geographic_scope,
    values_from = Frequency,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "Lit_type_simplified") 

chi_square_scope_result_lit_type <- chisq.test(contingency_table_scope_lit_type)
print(chi_square_scope_result_lit_type)
chi_square_scope_result_lit_type$residuals

scope_lit_type_matrix_table <- as.matrix(contingency_table_scope_lit_type)
cramerV(scope_lit_type_matrix_table, ci= TRUE) #from rcompanion package)


###Research types chi squared tests
#counts of different reference types
selected_lit_freq <- filtered_lit_refs %>%
  count(Country, Lit_type)

selected_lit_freq <-protected_area_income_epi %>% #add socioeconomic data
  left_join(selected_lit_freq, by = c("Country" = "Country")) %>%
  filter(!is.na(n)) 

aggregated_lit_count <- selected_lit_freq %>%
  filter(Country != "Average") %>% # Exclude the 'Average' group
  group_by(income_group, Lit_type) %>%
  summarise(
    Total_count = sum(n, na.rm = TRUE), # Sum up the percentages
    .groups = 'drop')

#3 income groups
contingency_table_for_chi2_research <- aggregated_lit_count %>%
  pivot_wider(
    names_from = Lit_type,
    values_from = Total_count,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "income_group") 

chi_square_result_research <- chisq.test(contingency_table_for_chi2_research)
print(chi_square_result_research) # sig but how interpret?
chi_square_result_research$residuals

#2 income groups
aggregated_lit_count2 <- selected_lit_freq %>%
  filter(Country != "Average") %>% # Exclude the 'Average' group
  group_by(simple_income_group, Lit_type) %>%
  summarise(
    Total_count = sum(n, na.rm = TRUE), # Sum up the percentages
    .groups = 'drop')

contingency_table_for_chi2_research2 <- aggregated_lit_count2 %>%
  pivot_wider(
    names_from = Lit_type,
    values_from = Total_count,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "simple_income_group") 

chi_square_result_research2 <- chisq.test(contingency_table_for_chi2_research2)
print(chi_square_result_research2)
 chi_square_result_research2$residuals



#Combined Literature type categories
aggregated_lit_count3 <- aggregated_lit_count2 %>%
  mutate(
    Lit_type_simplified = case_when(
      Lit_type %in% c("Journal", "Journal_npr", "Thesis", "Conference_paper") ~ "Journal",
      Lit_type == "Report" ~ "Report",
      Lit_type == "Book" ~ "Book",
    )
  ) %>%
  group_by(simple_income_group, Lit_type_simplified) %>%
  summarise(
    Total_count = sum(Total_count),
    .groups = "drop" 
  ) %>%
  ungroup() 


contingency_table_for_chi2_research3 <- aggregated_lit_count3 %>%
  pivot_wider(
    names_from = Lit_type_simplified,
    values_from = Total_count,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "simple_income_group") 

chi_square_result_research3 <- chisq.test(contingency_table_for_chi2_research3)
print(chi_square_result_research3) 
chi_square_result_research3$residuals

#Cramer's V
research_type_matrix_table <- as.matrix(contingency_table_for_chi2_research3)
cramerV(research_type_matrix_table, ci= TRUE) #from rcompanion package)

###Accessibility by income group
accessibility_summary_selectedlit_country <- filtered_lit_refs %>%
  group_by(Accessibility,Country) %>%
  summarise(Frequency = n(), .groups = "drop")

accessibility_summary_selectedlit_country <-protected_area_income_epi %>% #add socioeconomic data
  left_join(accessibility_summary_selectedlit_country, by = c("Country" = "Country")) %>%
  filter(!is.na(Accessibility)) 

aggregated_access_count <- accessibility_summary_selectedlit_country %>%
  filter(Country != "Average") %>% # Exclude the 'Average' group
  group_by(simple_income_group, Accessibility) %>%
  summarise(
    Total_count = sum(Frequency, na.rm = TRUE), # Sum up the percentages
    .groups = 'drop')

contingency_table_for_chi2_access <- aggregated_access_count %>%
  pivot_wider(
    names_from = Accessibility,
    values_from = Total_count,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "simple_income_group") 

chi_square_result_access <- chisq.test(contingency_table_for_chi2_access)
print(chi_square_result_access) 
chi_square_result_access$residuals

#Cramer's V
access_income_matrix_table <- as.matrix(contingency_table_for_chi2_access)
cramerV(access_income_matrix_table, ci= TRUE) #from rcompanion package)

#Accessbility by lit type

aggregated_access_lit_count <- accessibility_summary_selectedlit %>%
  mutate(
    Lit_type_simplified = case_when(
      Lit_type %in% c("Journal", "Journal_npr", "Thesis", "Conference_paper") ~ "Journal",
      Lit_type == "Report" ~ "Report",
      Lit_type == "Book" ~ "Book")) %>%
  group_by(Accessibility, Lit_type_simplified) %>%
  summarise(
    Frequency = sum(Frequency),
    .groups = "drop" 
  ) %>%
  ungroup() 

contingency_table_for_chi2_access2 <- aggregated_access_lit_count %>%
  pivot_wider(
    names_from = Accessibility,
    values_from = Frequency,
    values_fill = 0 # Fill combinations that don't exist with 0
  ) %>%
  tibble::column_to_rownames(var = "Lit_type_simplified") 

chi_square_result_access2 <- chisq.test(contingency_table_for_chi2_access2)
print(chi_square_result_access2) 
chi_square_result_access2$residuals

access_lit_matrix_table <- as.matrix(contingency_table_for_chi2_access2)
cramerV(access_lit_matrix_table, ci= TRUE) #from rcompanion package)


