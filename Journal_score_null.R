#Calculating null journal scores
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

scores_dataset_null <- read_excel("scores_dataset_try.xlsx")

categories_to_keep <- c(
  "Agricultural and Biological Sciences",
  "Biochemistry",
  "Genetics and Molecular Biology",
  "Business Management and Accounting",
  "Chemical Engineering",
  "Chemistry",
  "Decision Sciences",
  "Earth and Planetary Sciences",
  "Economics, Econometrics and Finance",
  "Energy",
  "Engineering",
  "Environmental Science",
  "Multidisciplinary",
  "Pharmacology, Toxicology and Pharmaceutics",
  "Psychology",
  "Social Sciences"
)


#Use grepl with paste(collapse = "|") to create a regex pattern for multiple matches within a string.
filtered_scores_dataset_null <- scores_dataset_null %>%
  filter(
    grepl(paste(categories_to_keep, collapse = "|"), Areas, ignore.case = TRUE)
  )


#'Total Docs. (3years)' is the column for weighting, make sure no 0 and no NA so weight_by works? 
# dont know if valid to do, but had less 0 values

#Make sure its numeric
filtered_scores_dataset_null$`Total Docs. (3years)` <- as.numeric(filtered_scores_dataset_null$`Total Docs. (3years)`)

filtered_scores_dataset_null_sampleset <- filtered_scores_dataset_null %>%
  filter(!is.na(`Total Docs. (3years)`)) %>%
  filter (`Total Docs. (3years)` > 0)


# --- Set a seed for reproducibility ---
set.seed(123)
# ------------------------------------

n_samples <- 100 

# Perform weighted sampling
sampled_scores_null <- filtered_scores_dataset_null_sampleset %>%
  slice_sample(n = n_samples, weight_by = `Total Docs. (3years)`, replace = TRUE)

head(sampled_scores_null)

ggplot(sampled_scores_null, aes(x = `H index`)) +
  geom_density(alpha = 0.5)+
  labs(x = "H index score", y = "Density") +
  theme_classic()






#Import the data
Thesis_dataset_actual <- read_excel("~/Library/CloudStorage/OneDrive-NewcastleUniversity/Thesis/Thesis_dataset_actual.xlsx")

journal_references <- Thesis_dataset_actual %>% filter(Lit_type == "Journal")

journal_references <- journal_references %>%
  mutate(SJR = as.numeric(na_if(SJR, "NA")),
         HINDEX = as.numeric(na_if(HINDEX, "NA")))

journal_references %>%
  summarise(
    sjr_missing = sum(is.na(SJR)),
    sjr_present = sum(!is.na(SJR)),
    h_missing = sum(is.na(HINDEX)),
    h_present = sum(!is.na(HINDEX)),
    both_missing = sum(is.na(SJR) & is.na(HINDEX))
  )

###Where are NA scores from?
journal_references_NA <- left_join(journal_references, protected_area_income_epi, by = "Country")

#Hindex
journal_references_NA1 <-journal_references_NA %>% filter(is.na(HINDEX))

sample_size_higher_income1 <- sum(journal_references_NA1$simple_income_group == "Higher income")

sample_size_lower_income1 <- sum(journal_references_NA1$simple_income_group == "Lower income")

#SJR
journal_references_NA2 <-journal_references_NA %>% filter(is.na(SJR))

sample_size_higher_income2 <- sum(journal_references_NA2$simple_income_group == "Higher income")

sample_size_lower_income2 <- sum(journal_references_NA2$simple_income_group == "Lower income")

###Clean the scores
journal_H_clean <- journal_references %>%
  filter(HINDEX != "NA") %>%
  mutate(HINDEX = as.numeric(HINDEX))

journal_H_long <- journal_H_clean %>%
  pivot_longer(
    cols = HINDEX,
    names_to = "Source",
    values_to = "Impact_score")

ggplot(journal_H_long, aes(x = Impact_score, fill = Country)) +
  geom_density(alpha = 0.5)+
  labs(x = "Impact Score", y = "Density", fill = "Country") +
  theme_classic()

#Compare distributions
wilcox_result <- wilcox.test(journal_H_clean$HINDEX, sampled_scores_null$`H index`)

# Print the results
print(wilcox_result)

combined_h_index_data <- tibble(
  h_index = c(journal_H_clean$HINDEX, sampled_scores_null$`H index`),
  group = factor(c(rep("Sample", length(journal_H_clean$HINDEX)), rep("Null", length(sampled_scores_null$`H index`))))
)

ggplot(combined_h_index_data, aes(x = h_index, color = group)) +
  geom_density(linewidth = 1) +
  labs(title = "Smoothed Density of H-Index by Group", x = "H-Index", y = "Density") +
  theme_minimal() +
  scale_color_manual(values = c("Sample" = "steelblue", "Null" = "firebrick"))


###SJR
journal_SJR_clean <- journal_references %>%
  filter(SJR != "NA") %>%
  mutate(SJR = as.numeric(SJR))

journal_SJR_long <- journal_SJR_clean %>%
  pivot_longer(
    cols = SJR,
    names_to = "Source",
    values_to = "Impact_score")

ggplot(journal_SJR_long, aes(x = Impact_score, fill = Country)) +
  geom_density(alpha = 0.5)+
  labs(x = "Impact Score", y = "Density", fill = "Country") +
  theme_classic()

#Compare distributions
wilcox_result2 <- wilcox.test(journal_SJR_clean$SJR, sampled_scores_null$SJR)

# Print the results
print(wilcox_result2)

combined_SJR_data <- tibble(
  SJR = c(journal_SJR_clean$SJR, sampled_scores_null$SJR),
  group = factor(c(rep("Sample", length(journal_SJR_clean$SJR)), rep("Null", length(sampled_scores_null$SJR))))
)

ggplot(combined_SJR_data, aes(x = SJR, color = group)) +
  geom_density(linewidth = 1) +
  labs(title = "Smoothed Density of SJR by Group", x = "SJR", y = "Density") +
  theme_minimal() +
  scale_color_manual(values = c("Sample" = "steelblue", "Null" = "firebrick"))





### Scores comparison income groups

journal_H_long <- journal_H_clean %>%
  pivot_longer(
    cols = HINDEX,
    names_to = "HINDEX",
    values_to = "Impact_score")


ggplot(journal_H_long, aes(x = Country, y = Impact_score)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Country", y = "Impact Score") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17))

journal_H_long_income <- left_join(journal_H_long, protected_area_income_epi, by = "Country")


ggplot(journal_H_long_income, aes(x = simple_income_group, y = Impact_score)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Country", y = "Impact Score", fill = "Score database") +
  theme_classic() +
  theme(axis.title = element_text(size = 18),  # axis titles
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16))

ggplot(journal_H_long_income, aes(x = Impact_score, color = simple_income_group)) +
  geom_density(linewidth = 1) +
  labs(x = "H Index", y = "Density") +
  theme_classic() 


journal_H_long_income_low <- journal_H_long_income %>% filter(simple_income_group == "Lower income")
journal_H_long_income_high <- journal_H_long_income %>% filter(simple_income_group == "Higher income")
wilcox_result_income <- wilcox.test(journal_H_long_income_low$Impact_score, journal_H_long_income_high$Impact_score, conf.int = TRUE)

testresult<-lm(journal_H_long_income$Impact_score ~ simple_income_group, data = journal_H_long_income)
summary(testresult)


combined_h_index_data2 <- tibble(
  h_index = c(journal_H_long_income$Impact_score, sampled_scores_null$`H index`),
  Group = factor(c(
    as.character(journal_H_long_income$simple_income_group), # Convert to character first
    rep("Null", length(sampled_scores_null$`H index`))
  ))
)

scores_colours <- c(
  "Higher income" = "skyblue",
  "Lower income" = "deepskyblue4",
  "Null" = "red3")

ggplot(combined_h_index_data2, aes(x = h_index, color = Group )) +
  geom_density(linewidth = 1) +
  scale_color_manual(values = scores_colours)+
  labs( x = "H Index", y = "Density") +
  theme_classic() +
  theme(axis.title = element_text(size =18),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))


wilcox_result_low_null <- wilcox.test(journal_H_long_income_low$Impact_score, sampled_scores_null$`H index`, conf.int = TRUE)
wilcox_result_high_null <- wilcox.test(journal_H_long_income_high$Impact_score, sampled_scores_null$`H index`, conf.int = TRUE)

median(journal_H_long_income_low$Impact_score)
median(journal_H_long_income_high$Impact_score)
median(sampled_scores_null$`H index`)

NROW(journal_H_long_income_low$Impact_score)
NROW(journal_H_long_income_high$Impact_score)
NROW(sampled_scores_null$`H index`)

ggplot(combined_h_index_data2, aes(x = Group, y= h_index, fill = Group)) +
  geom_boxplot(notch = TRUE)+
  scale_fill_manual(values = scores_colours)+
  labs(x = "Group", y = "H Index") +
  theme_classic() +
  theme(axis.title = element_text(size =15),  # axis titles
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))

###SJR
journal_SJR_long_income <- left_join(journal_SJR_long, protected_area_income_epi, by = "Country")


ggplot(journal_SJR_long_income, aes(x = simple_income_group, y = Impact_score)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Country", y = "Impact Score", fill = "Score database") +
  theme_classic() +
  theme(axis.title = element_text(size = 18),  # axis titles
        axis.text = element_text(size = 17),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16))

ggplot(journal_SJR_long_income, aes(x = Impact_score, color = simple_income_group)) +
  geom_density(linewidth = 1) +
  labs( x = "SJR", y = "Density") +
  theme_classic() 


journal_SJR_long_income_low <- journal_SJR_long_income %>% filter(simple_income_group == "Lower income")
journal_SJR_long_income_high <- journal_SJR_long_income %>% filter(simple_income_group == "Higher income")
wilcox_result_income <- wilcox.test(journal_SJR_long_income_low$Impact_score, journal_SJR_long_income_high$Impact_score, conf.int = TRUE)

testresult2<-lm(journal_SJR_long_income$Impact_score ~ simple_income_group, data = journal_H_long_income)
summary(testresult2)


combined_SJR_data2 <- tibble(
  SJR = c(journal_SJR_long_income$Impact_score, sampled_scores_null$SJR),
  Group = factor(c(
    as.character(journal_SJR_long_income$simple_income_group), # Convert to character first
    rep("Null", length(sampled_scores_null$SJR))
  ))
)

scores_colours <- c(
  "Higher income" = "skyblue",
  "Lower income" = "deepskyblue4",
  "Null" = "red3")

ggplot(combined_SJR_data2, aes(x = SJR, color = Group )) +
  geom_density(linewidth = 1) +
  scale_color_manual(values = scores_colours)+
  scale_y_continuous(breaks = c(0, 0.0002, 0.0004, 0.0006, 0.0008), 
                     limits = c(0, 0.0008),
                     labels = scales::label_number()) +
  labs( x = "SJR", y = "Density") +
  theme_classic() +
  theme(axis.title = element_text(size =18),  # axis titles
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))


wilcox_result_low_null <- wilcox.test(journal_SJR_long_income_low$Impact_score, sampled_scores_null$SJR, conf.int = TRUE)
wilcox_result_high_null <- wilcox.test(journal_SJR_long_income_high$Impact_score, sampled_scores_null$SJR, conf.int = TRUE)

median(journal_SJR_long_income_low$Impact_score)
median(journal_SJR_long_income_high$Impact_score)
median(sampled_scores_null$SJR)

NROW(journal_SJR_long_income_low$Impact_score)
NROW(journal_SJR_long_income_high$Impact_score)
NROW(sampled_scores_null$SJR)

#Replicate the notch function to create custom bars for 95% CI
notch_ci_fun <- function(x) {
  median_val <- median(x)
  iqr_val <- IQR(x)
  n_val <- length(x)
  ci_range <- 1.58 * iqr_val / sqrt(n_val) # same constant as other notches
  data.frame(y = median_val,
             ymin = median_val - ci_range,
             ymax = median_val + ci_range)
}


ggplot(combined_SJR_data2, aes(x = Group, y= SJR, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = scores_colours) +
  scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000), 
                     limits = c(0, 20000)) +
  stat_summary(fun.data = notch_ci_fun, geom = "errorbar", color = "red", width = 0.2) + #Add error bars
  labs(x = "Group", y = "SJR") +
  theme_classic() +
  theme(axis.title = element_text(size =15),  # axis titles
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15))
