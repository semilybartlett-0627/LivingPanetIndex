
data <- LPD_2024_public

# convert categorical variables to factors
data$Class <- as.factor(data$Class)
data$Order <- as.factor(data$Order)
data$Family <- as.factor(data$Family)
data$Genus <- as.factor(data$Genus)
data$Country <- as.factor(data$Country)
data$Region <- as.factor(data$Region)
data$Native <- as.factor(data$Native)
data$System <- as.factor(data$System)
names(LPD_2024_public)
# make categorical variable from a continuous variable
data$Latitude <- as.numeric(data$Latitude)

data$Lat_Category <- cut(
  data$Latitude,
  breaks = 3,
  labels = c("Low", "Medium", "High")
)
# make ordinal variable from a different contiuous variable
data$Longitude <- as.numeric(data$Longitude)

data$Longitude_Rank <- cut(
  data$Longitude,
  breaks = 4,
  labels = c("Very Low", "Low", "High", "Very High"),
  ordered_result = TRUE
)
# make multiple objects from one categorical variable
freshwater_data <- subset(data, System == "Freshwater")
marine_data <- subset(data, System == "Marine")
terrestrial_data <- subset(data, System == "Terrestrial")

#for the native vs invasive species 
native_data <- subset(data, Native == 1)
nonnative_data <- subset(data, Native == 0)

# for question 1 
terrestrial_distribution <- terrestrial_data[, c("Genus", "Country")]
# for question 2
native_distribution <- data[, c("Native", "Region", "Country")]
# for question 3
data$`2020` <- as.numeric(data$`2020`)
region_abundance <- data[, c("Region", "2020")]


#descriptive stats for question 3 
library(dplyr)
region_stats <- region_abundance %>%
  group_by(Region) %>%
  summarise(
    mean_abundance = mean(`2020`, na.rm = TRUE),
    median_abundance = median(`2020`, na.rm = TRUE),
    sd_abundance = sd(`2020`, na.rm = TRUE),
    sample_size = n()
  )

region_stats

#histogram
library(ggplot2)
#original histogram from code was very skewed left, had to work around that
region_abundance_clean <- region_abundance %>%
  filter(!is.na(`2020`))

ggplot(region_abundance_clean, aes(x = log10(`2020`))) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Log10 Population Abundance in 2020",
    x = "Log10 Population Abundance",
    y = "Frequency"
  ) +
  theme_minimal()

#boxplot by region, original data was too wide, removed outliers
ggplot(region_abundance, aes(x = Region, y = `2020`)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10() +
  labs(
    title = "Population Abundance by Region",
    x = "Region",
    y = "Log10 Population Abundance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#bar graph of abundance by region to compare to boxplot
ggplot(region_stats, aes(x = Region, y = mean_abundance)) +
  geom_col() +
  labs(
    title = "Mean Population Abundance by Region",
    x = "Region",
    y = "Mean Abundance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))


sum(is.na(region_abundance_clean$`2020`))

#chi-square test for native distribution

native_table <- table(native_distribution$Native,
                      native_distribution$Region)
native_table
chi_result <- chisq.test(native_table)

chi_result
chi_result$expected
chi_result$residuals

#ANOVA

anova_result <- aov(`2020` ~ Region, data = region_abundance_clean)
summary(anova_result)

TukeyHSD(anova_result)

aggregate(`2020` ~ Region,
          data = region_abundance_clean,
          mean)
