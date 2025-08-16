
#Packages to load in
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(patchwork)
library(tidyr)
library(ggbreak)

#Import the data
Thesis_dataset_actual <- read_excel("~/Library/CloudStorage/OneDrive-NewcastleUniversity/Thesis/Thesis_dataset_actual.xlsx")

#Count number of references per country
ref_counts <- Thesis_dataset_actual %>%
  count(Country, name = "num_references")



#Exclude NA values (didnt have a year listed or unclear)
references_clean_year <- Thesis_dataset_actual %>% filter(Year != "NA")

#Average age of reference 
references_clean_year$Year <- as.numeric(references_clean_year$Year)
average_year_by_country <- references_clean_year %>% group_by(Country) %>% summarize(references_clean_year = mean(Year))


#Calculate frequencies of publishing years per year for each country by lit_type
ref_year_freq <- as.data.frame(table(references_clean_year$Year, references_clean_year$Country))
colnames(ref_year_freq) <- c("Year", "Country", "Frequency")
ref_year_freq$Year <- as.numeric(ref_year_freq$Year)


# Find the overall range of years present in the data
min_year <- min(ref_year_freq$Year)
max_year <- max(ref_year_freq$Year)

# Create a complete data frame with all year-country combinations
ref_year_freq_extended <- ref_year_freq %>%
  complete(Country, Year = min_year:max_year, fill = list(Frequency = NA)) %>%
  mutate(Frequency = ifelse(is.na(Frequency), 0, Frequency))


###OLD###
#Assign Publication years
pub_years <- data.frame(
  Country = c("Eritrea", "Lao_PDR", "Mozambique", "Nepal", "Estonia", 
              "Azerbaijan", "Austria", "Australia", "Suriname", "Trinidad_Tobago"),
  PubYear = c(2015, 2016, 2015, 2014, 2012, 2016, 2022, 2024, 2024, 2017)
)


#Special plot for Eritrea due to outlier

#filter for Eritrea
eritrea_df <- ref_year_freq_extended %>%
  filter(Country == "Eritrea")

eritrea_pub_years <- pub_years %>%
  filter(Country == "Eritrea")

# Create the  plot for Eritrea
p_eritrea <- ggplot(eritrea_df, aes(x = Year, y = Frequency)) +
  geom_line(linewidth = 1, color = "deepskyblue3") +
  geom_vline(data = eritrea_pub_years, aes(xintercept = PubYear),
             linetype = "dashed", linewidth = 0.5, color = "lightblue3") +
  labs(title = "Eritrea",
       x = "Year of reference publication",
       y = "Frequency") +
  theme_classic()+
  theme(axis.title = element_text(size = 20),  # axis titles
        axis.text = element_text(size = 20),   # axis labels
        plot.title = element_text(size = 30))

# Apply the axis break with ggbreak (need to cite)
p_eritrea_x_break <- p_eritrea +
  scale_x_break(breaks = c(1862, 1925),
    scales = 1.5,
    space=1) +
  theme(axis.title.y.right = element_blank(), # Remove right y-axis title
        axis.text.y.right = element_blank(),  # Remove right y-axis text
        axis.line.y.right = element_blank(),  # Remove right y-axis line
        axis.ticks.y.right = element_blank()) # Remove right y-axis ticks

print(p_eritrea_x_break)


###List them out individually (all except Eritrea)
# Get the x-axis and y-axis limits from the EU data


other_countries <- unique(ref_year_freq_extended$Country)[unique(ref_year_freq_extended$Country) != "Eritrea"] #select all but Eritrea

country_colors <- c("firebrick", "darkgreen", "mediumpurple", "darkorange2", "coral4", "hotpink", "darkcyan", "darkmagenta", "gold3")


for (i in seq_along(other_countries)) {
  country <- other_countries[i] #cycle through countries
  current_color <- country_colors[(i - 1) %% length(country_colors) + 1] # Cycle through colors
  
  plot <- ggplot(ref_year_freq_extended %>% filter(Country == country), #make plot
                 aes(x = Year, y = Frequency, color = Country)) +
    geom_line(linewidth = 1, color = current_color) +
    geom_vline(data = pub_years %>% filter(Country == country),
               aes(xintercept = PubYear),
               linetype = "dashed", linewidth = 0.5) +
    scale_x_continuous(limits = c(1925, NA)) +
    labs(title = paste(country),
         x = "Year of reference publication",
         y = "Frequency") +
    theme_classic() +
    theme(axis.title = element_text(size = 20),  # axis titles
          axis.text = element_text(size = 20),   # axis labels
          plot.title = element_text(size = 30))
  print(plot) # Display each plot
}


#### Lag time instead

all_references_lag <- references_clean_year %>% 
  left_join(pub_years, by = "Country") %>%
  mutate(lag = PubYear - Year)

#calculate lag frequencies
all_ref_lag_freq <- all_references_lag %>%
  group_by(lag, Country, Lit_type) %>%
  summarize(Frequency = n(), .groups = "drop") 

# Find the overall range of years present in the data
min_lag <- min(all_ref_lag_freq$lag)
max_lag <- max(all_ref_lag_freq$lag)

# Create a complete data frame with all year-country combinations
all_ref_year_lag_extended <- all_ref_lag_freq %>%
  group_by(Country, Lit_type) %>%
  complete(lag = seq(min_lag, max_lag, by = 1), 
           fill = list(Frequency = 0)) %>%
  ungroup()

ggplot(all_ref_year_lag_extended, aes(x = lag, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Years of publication by selected literature types",
       x = "Lag time",
       y = "Number of references") +
  theme_classic()  +
  facet_wrap(~ Lit_type, ncol = 1, scales = "free_y")



ggplot(all_ref_lag_freq, aes(x = Lit_type, y = lag, color = Lit_type)) +
  geom_boxplot() +
  labs(title = "Years of publication by selected literature types ",
       x = "Year of reference publication",
       y = "Year") +
  theme_classic()


#Lag vs selected lit types
all_lag_model1 <- lm(lag ~ Lit_type, data = all_ref_lag_freq) 
summary(all_lag_model1)


###Lag vs Country
all_ref_lag_income <- left_join(all_ref_lag_freq, income_group_data, by = "Country")

#is this doing what I want it to do?
all_lag_model2 <- lm(lag ~ income_group, data = all_ref_lag_income) 
summary(all_lag_model2)

ggplot(all_ref_lag_income, aes(x = income_group, y = lag)) +
  geom_boxplot() +
  labs(x = "Income group",
       y = "lag") +
  theme_classic()






#Try percentage
keyword_percentage_by_year <- keyword_counts_year %>%
  group_by(Year) %>%
  mutate(total_count = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = count / total_count * 100)

# Get top keyword per year based on percentage
top_keyword_by_year_percentage <- keyword_percentage_by_year %>%
  group_by(Year) %>%
  slice_max(order_by = percentage, n = 1, with_ties = FALSE) %>%
  ungroup() 

ggplot(top_keyword_by_year_percentage, aes(x = keyword, y = percentage, fill = keyword)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ Year, scales = "free_x") +
  labs(title = "Top Keyword by Year",
       x = "Keyword",
       y = "Percentage (%)") +
  theme_classic() 



