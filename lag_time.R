#Packages to load in
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)
library(tidyr)
library(ggbreak)
library(scales)
library(sjPlot)
library(nlme)

#Import the data
Thesis_dataset_actual <- read_excel("~/Library/CloudStorage/OneDrive-NewcastleUniversity/Thesis/Thesis_dataset_actual.xlsx")

#Exclude NA values (didnt have a year listed or unclear)
references_clean_year <- Thesis_dataset_actual %>% filter(Year != "NA")

#Include Book_chap in Book classificaiton
filtered_lit_refs1 <- references_clean_year %>%
  mutate(Lit_type = ifelse(Lit_type == "Book_chap", "Book", Lit_type))

#Add Report from Grey_lit_type as a lit type 
filtered_lit_refs2 <- references_clean_year %>%
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

#calculate time lag
filtered_lit_refs$Year <- as.numeric(filtered_lit_refs$Year)


pub_years <- data.frame(
  Country = c("Australia", "Austria", "Azerbaijan", "Belgium", "Bosnia_Herzegovina",
              "Brazil", "Denmark", "Dominica", "Eritrea", "Estonia",
              "Hungary", "Lao_PDR", "Lesotho", "Liberia", "Liechtenstein",
              "Lithuania", "Mauritius", "Mozambique", "Nepal", "New_Zealand",
              "Palau", "Saint_Vincent_Grenadines", "Saudi_arabia", "Serbia",
              "Singapore", "Somalia", "St_Kitts_Nevis", "Suriname",
              "Trinidad_Tobago", "United_Kingdom", "Vanuatu"),
  PubYear = c(2024, 2022, 2016, 2025, 2016,
              2017, 2024, 2013, 2015, 2012,
              2023, 2016, 2000, 2017, 2024,
              2015, 2017, 2015, 2014, 2020,
              2016, 2017, 2005, 2021,2019, 
              2015, 2014, 2024, 2018, 2025, 2018)
)



references_lag <- filtered_lit_refs %>% 
  left_join(pub_years, by = "Country") %>%
  mutate(lag = PubYear - Year)

#Average lag
mean(references_lag$lag)

#Standard error 
# Calculate the standard error
sd_row_count_lag <- sd(references_lag$lag)
sample_size_lag <- NROW(references_lag$lag) #unsure why it needs to be captial to work

standard_error_row_count <- sd_row_count_lag / sqrt(sample_size_lag)

hist(references_lag$lag)
# Create bins (e.g., 0-5, 5-10, 10-15 years, etc.)
lag_bins <- cut(references_lag$lag,
                breaks = c(0, 5, 10, 15, 20, Inf),
                right = FALSE, # Include the lower bound, exclude the upper
                labels = c("0-4", "5-9", "10-14", "15-19", "20+"))

# Create a frequency table
lag_counts <- table(lag_bins)

# Calculate proportions
lag_proportions <- prop.table(lag_counts)

# Calculate cumulative proportions
cumulative_proportions_lag <- cumsum(lag_proportions)


#Significance of lit_type
references_lag <- references_lag %>%
  mutate(
    Lit_type_simplified = case_when(
      Lit_type %in% c("Journal", "Journal_npr", "Thesis", "Conference_paper") ~ "Journal",
      Lit_type == "Report" ~ "Report",
      Lit_type == "Book" ~ "Book")) 

lit_lag_result <- aov(references_lag$lag ~ references_lag$Lit_type)
Tukey_lag_result<- TukeyHSD(lit_lag_result)
summary(lit_lag_result)

simplified_lit_lag_result <- aov(references_lag$lag ~ references_lag$Lit_type_simplified)
Tukey_lag_result<- TukeyHSD(simplified_lit_lag_result)
summary(simplified_lit_lag_result)


###Multinomial Logistic Regression Model to look at imapct of lag on accessiblity
library(nnet)
references_lag$Accessibility[references_lag$Accessibility == "NA"] <- "Unknown"

references_lag$Accessibility <- factor(references_lag$Accessibility,
                                       levels = c("Free","Paid", "Inaccessible"))
access_by_year_multi_model <- multinom(Accessibility ~ lag, data = references_lag, Hess = TRUE) # Hess=TRUE for standard errors

# View the model summary
summary(access_by_year_multi_model)
z <- summary(access_by_year_multi_model)$coefficients / summary(access_by_year_multi_model)$standard.errors
p_values <- (1 - pnorm(abs(z))) * 2 # Two tailed p
print(z)
print(p_values)

#confidence intervals
conf_intervals_access_lag <- confint(access_by_year_multi_model)



#Significance of income group
lag_socioeconomic<- left_join(references_lag, protected_area_income_epi, by = "Country")

#Use to relevel to Middle first
lag_socioeconomic$income_group <- as.factor(lag_socioeconomic$income_group)
lag_socioeconomic$income_group <- relevel(lag_socioeconomic$income_group, ref = "Middle Income")


##Higher income average
higher_lag<- lag_socioeconomic %>% filter(simple_income_group == "Higher income")
mean(higher_lag$lag)

# Calculate the standard error
sd_row_count_higher_lag <- sd(higher_lag$lag)
sample_size_higher_lag <- NROW(higher_lag$lag)

standard_error_row_count_higher <- sd_row_count_higher_lag / sqrt(sample_size_higher_lag)


##Lower income average
lower_lag <- lag_socioeconomic %>% filter(simple_income_group == "Lower income")
mean(lower_lag$lag)

# Calculate the standard error
sd_row_count_lower_lag <- sd(lower_lag$lag)
sample_size_lower_lag <- NROW(lower_lag$lag) #unsure why it needs to be captial to work

standard_error_row_count_lower <- sd_row_count_lower_lag / sqrt(sample_size_lower_lag)


#Check levels
levels(lag_socioeconomic$income_group)

#significance while adding country as a random variable
try_result2 <-lme(lag ~ income_group, random = ~1 | Country, data = lag_socioeconomic) 
summary(try_result2) #sig

#two income groups
try_result3 <-lme(lag ~ simple_income_group, random = ~1 | Country, data = lag_socioeconomic) 
summary(try_result3) #sig

#insig
try_result4 <-lme(lag ~ NY.GDP.PCAP.CD, random = ~1 | Country, data = lag_socioeconomic)
summary(try_result4)

#Add simplified_lit_type as a  variable
lag_socioeconomic_littype <- lag_socioeconomic %>%
  mutate(
    Lit_type_simplified = case_when(
      Lit_type %in% c("Journal", "Journal_npr", "Thesis", "Conference_paper") ~ "Journal",
      Lit_type == "Report" ~ "Report",
      Lit_type == "Book" ~ "Book")) 

#Non-interaction lit type, testing variance structures
try_result1 <-lme(lag ~ simple_income_group + Lit_type_simplified, random = ~1 | Country, data = lag_socioeconomic_littype, weights = varIdent(form = ~1 | Lit_type_simplified))
try_result2 <-lme(lag ~ simple_income_group + Lit_type_simplified, random = ~1 | Country, data = lag_socioeconomic_littype, weights = varIdent(form = ~1 | simple_income_group))
try_result3 <-lme(lag ~ simple_income_group + Lit_type_simplified, random = ~1 | Country, data = lag_socioeconomic_littype)

AIC(try_result1, try_result2, try_result3)

# testing interactions
try_result1 <-lme(lag ~ simple_income_group + Lit_type_simplified, random = ~1 | Country, data = lag_socioeconomic_littype, weights = varIdent(form = ~1 | Lit_type_simplified))
try_result4 <-lme(lag ~ simple_income_group * Lit_type_simplified, random = ~1 | Country, data = lag_socioeconomic_littype, weights = varIdent(form = ~1 | Lit_type_simplified))
try_result1_ML <- update(try_result1, method = "ML")
try_result4_ML <- update(try_result4, method = "ML")
anova(try_result1_ML, try_result4_ML)

summary(try_result1)
plot(try_result1)
eemeans<- emmeans(try_result1, pairwise ~ Lit_type_simplified)
contrast(eemeans, method = "pairwise")

intervals(try_result1)

contrast(eemeans, "pairwise", adjust = "t") |> confint()

#---- calculate lag frequencies ----
lit_ref_lag_freq <- references_lag %>%
  group_by(lag, Country, Lit_type_simplified) %>%
  summarize(Frequency = n(), .groups = "drop") 

average_lag_per_lit_type <- lit_ref_lag_freq %>%
  group_by(Lit_type_simplified) %>%
  summarise(mean_lag = mean(lag))

lit_ref_lag_freq$Lit_type_simplified <- as.factor(lit_ref_lag_freq$Lit_type_simplified)

# Reorder the levels of Lit_type_simplified factor based on mean_lag
lit_ref_lag_freq$Lit_type_simplified <- factor(lit_ref_lag_freq$Lit_type_simplified,
                                    levels = average_lag_per_lit_type$Lit_type_simplified[order(average_lag_per_lit_type$mean_lag)])

# Create a complete data frame with all lag-country combinations
lit_ref_year_lag_extended <- lit_ref_lag_freq %>%
  group_by(Country, Lit_type_simplified) %>%
  complete(lag = seq(min_lag_lit, max_lag_lit, by = 1), 
  fill = list(Frequency = 0)) %>%
  ungroup()

ggplot(lit_ref_year_lag_extended, aes(x = lag, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Years of publication by selected literature types",
       x = "Lag time",
       y = "Number of references") +
  theme_classic()  +
  facet_wrap(~ Lit_type_simplified, ncol = 1, scales = "free_y")


library(multcompView)
#---- Add Significant letters ----

cld_data <- multcompLetters(Tukey_lag_result$"references_lag$Lit_type_simplified"[,"p adj"])# [,4] extracts the p-values

#make from list to df
cld_df <- as.data.frame(cld_data$Letters) %>%
  rename(letters = `cld_data$Letters`) %>%
  mutate(Lit_type_simplified = rownames(.))

plot_position_data <- lit_ref_lag_freq %>%
  group_by(Lit_type_simplified) %>%
  summarise(
    max_lag = max(lag, na.rm = TRUE) # Calculate max lag for each Lit_type_simplified
  ) %>%
  left_join(cld_df, by = "Lit_type_simplified") # Join with  significance letters


ggplot(lit_ref_lag_freq, aes(x = Lit_type_simplified, y = lag, color = Lit_type_simplified)) +
  geom_boxplot() +
  # Add geom_text for the significance letters
  geom_text(data = plot_position_data,
            aes(x = Lit_type_simplified, y = -7, label = letters), 
            color = "black", 
            vjust = -0.5,   
            size = 4,        
            fontface = "bold", # Make the text bold
            inherit.aes = FALSE) + #Prevents geom_text from inheriting color aesthetic from main plot
  labs(x = "Literature type",
       y = "Lag in years") +
  coord_cartesian(ylim = c(0, 150))+ #apply max for better visual but without impacting boxplot
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.position="none")

ggplot(lit_ref_lag_freq, aes(x = Lit_type_simplified, y = lag, color = Lit_type_simplified)) +
geom_boxplot() +
  # Add geom_text for the significance letters
  geom_text(data = plot_position_data,
            aes(x = Lit_type_simplified, y = -15, label = letters), 
            color = "black", 
            vjust = -0.5,   
            size = 4,        
            fontface = "bold", # Make the text bold
            inherit.aes = FALSE) + #Prevents geom_text from inheriting color aesthetic from main plot
  labs(x = "Literature type",
       y = "Lag in years") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 15),
        legend.position="none")

#---
simple_lit_colours <- c("Report"= "red3",
                        "Journal" ="deepskyblue3",
                        "Book" = "lightgreen")


#without letters
p2<- ggplot(lit_ref_lag_freq, aes(x = Lit_type_simplified, y = lag, fill = Lit_type_simplified)) +
  scale_fill_manual(values = simple_lit_colours) +
  geom_boxplot() +
  labs(x = "Literature type",
       y = "Lag in years") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.position="none")


#---- year vs selected lit types ----
try_result3 <-lme(lag ~ Lit_type_simplified, random = ~1 | Country, data = lit_ref_lag_freq)
summary(try_result3)

#Year vs Country for only selected literature
lit_ref_lag_income <- left_join(lit_ref_lag_freq, protected_area_income_epi, by = "Country")

#need to add + country?
lit_lag_model2 <- lm(lag ~ income_group, data = lit_ref_lag_income) 
summary(lit_lag_model2)

lit_lag_model3 <- lm(lag ~ NY.GDP.PCAP.CD, data = lit_ref_lag_income) 
summary(lit_lag_model3)


lit_ref_lag_income$income_group <- factor(
  lit_ref_lag_income$income_group,
  levels = c("Higher income", "Middle Income", "Lower income")
)

ggplot(lit_ref_lag_income, aes(x = simple_income_group, y = lag)) +
  geom_boxplot() +
  labs(x = "Income group",
       y = "Lag in years") +
  coord_cartesian(ylim = c(0, 150))+ #apply max for better visual but without impacting boxplot
  theme_classic() +
  theme(axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15))

p1 <- ggplot(lit_ref_lag_income, aes(x = simple_income_group, y = lag, fill = simple_income_group)) +
  scale_fill_brewer(palette="Paired")+
  geom_boxplot() +
  labs(x = "Income group",
       y = "Lag in years") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.position="none")


ggplot(lit_ref_lag_income, aes(x = income_group, y = lag)) +
  geom_boxplot() +
  labs(x = "Income group",
       y = "Lag in years") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15))

#try to combine literature type and income group plots using patchwork
(p1 / p2) + plot_layout(axes = "collect")


###Log transform
log1p_transformation <- trans_new(
  name = "log1p_transformation",
  transform = function(x) log1p(x),
  inverse = function(x) expm1(x))


lag_plot1 <- ggplot(lit_ref_lag_income, aes(x = simple_income_group, y = lag, fill = simple_income_group)) +
  scale_fill_brewer(palette="Paired")+
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(trans = log1p_transformation,
                     labels = scales::comma,
                     breaks = c(0, 5, 25, 50, 100, 200, 300, 400)) + 
  labs(x = "Income group",
       y = "Lag in years") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.position="none")


lag_plot2<- ggplot(lit_ref_lag_freq, aes(x = Lit_type_simplified, y = lag, fill = Lit_type_simplified)) +
  scale_fill_manual(values = simple_lit_colours) +
  geom_boxplot(notch = TRUE) +
  scale_y_continuous(trans = log1p_transformation,
                     labels = scales::comma,
                     breaks = c(0, 5, 25, 50, 100, 200, 300, 400)) + 
  labs(x = "Literature type",
       y = "Lag in years") +
  theme_classic() +
  theme(axis.title = element_text(size = 16),  # axis titles
        axis.text = element_text(size = 15),
        legend.position="none")

(lag_plot1 | lag_plot2)


###journal-income age differences
journal_ref_lag_income <- lit_ref_lag_income %>% filter(Lit_type_simplified == "Journal")
mean(journal_ref_lag_income$lag)

journal_lag_model <-lme(lag ~ simple_income_group, random = ~1 | Country, data = journal_ref_lag_income) 
summary(journal_lag_model)

journal_ref_lag_income_high <- journal_ref_lag_income %>% filter(simple_income_group == "Higher income")
mean(journal_ref_lag_income_high$lag)

journal_ref_lag_income_low <- journal_ref_lag_income %>% filter(simple_income_group == "Lower income")
mean(journal_ref_lag_income_low$lag)

#book-income age differences
book_ref_lag_income <- lit_ref_lag_income %>% filter(Lit_type_simplified == "Book")
mean(book_ref_lag_income$lag)

book_lag_model <-lme(lag ~ simple_income_group, random = ~1 | Country, data = book_ref_lag_income) 
summary(book_lag_model)

book_ref_lag_income_high <- book_ref_lag_income %>% filter(simple_income_group == "Higher income")
mean(journal_ref_lag_income_high$lag)

book_ref_lag_income_low <- book_ref_lag_income %>% filter(simple_income_group == "Lower income")
mean(journal_ref_lag_income_low$lag)

#Report-income age differences
report_ref_lag_income <- lit_ref_lag_income %>% filter(Lit_type_simplified == "Report")
mean(report_ref_lag_income$lag)

report_lag_model <-lme(lag ~ simple_income_group, random = ~1 | Country, data = report_ref_lag_income) 
summary(report_lag_model)

report_ref_lag_income_high <- report_ref_lag_income %>% filter(simple_income_group == "Higher income")
mean(report_ref_lag_income_high$lag)

report_ref_lag_income_low <- report_ref_lag_income %>% filter(simple_income_group == "Lower income")
mean(report_ref_lag_income_low$lag)








