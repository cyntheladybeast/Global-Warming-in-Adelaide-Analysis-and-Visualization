#Temperature in Adelaide Data Analysis and Visualization 
###Author: Cynthia Mueni Kiiti 

# Read Data Files
file_path <- "C:/Users/amwikali/Desktop/MSc. DATA SCIENCE/S.P for Data Science/Assignment 2 - 29th Oct/IDCJAC0010_023034_1800_Data.csv"
maximum_data<- read.csv(file_path)
filepath <- "C:/Users/amwikali/Desktop/MSc. DATA SCIENCE/S.P for Data Science/Assignment 2 - 29th Oct/IDCJAC0011_023034_1800_Data.csv"
minimum_data<- read.csv(filepath)

# Maximum temperature data descriptive statistics
max_temp_data <- maximum_data$Maximum.temperature..Degree.C.

# Summary statistics
summary(max_temp_data)

# Minimum temperature data descriptive statistics
min_temp_data <- minimum_data$Minimum.temperature..Degree.C.

# Summary statistics
summary(min_temp_data)

# Find the month when the highest day temperature was observed
max_temp_date <- maximum_data$Month[which.max(max_temp_data)]
cat("Month of the highest temperature:", max_temp_date, "\n")

# Find the month when the lowest day temperature was observed
max_temp_date <- maximum_data$Month[which.min(max_temp_data)]
cat("Month of the highest temperature:", max_temp_date, "\n")

# Find the date when the highest night temperature was observed
min_temp_date <- minimum_data$Month[which.max(min_temp_data)]
cat("Month of the lowest temperature:", min_temp_date, "\n")


# Find the date when the lowest night temperature was observed
min_temp_date <- minimum_data$Month[which.min(min_temp_data)]
cat("Month of the lowest temperature:", min_temp_date, "\n")

# Question 2
# Load necessary libraries to create the function

library(dplyr) # for data manipulation and filtering.
library(readr) # for reading and parsing data from CSV files.
library(moments) # for calculating statistical moments, such as skewness and kurtosis.
library(ggplot2) #  for creating data visualizations.

# Function to calculate and present descriptive statistics
calculate_descriptive_stats <- function(data, year, month) {
  subset_data <- data %>% 
    filter(Year == year, Month == month) # filters the data to create a subset of the data, filtered data is stored in 'subset_data'
  
  if (nrow(subset_data) == 0) {
    cat("No data found for the specified year and month.")
    return(NULL)
    # checks if there is no data in the subset_data DataFrame,  it prints a message stating that no data was found for the specified year and month, and it returns NULL
  }
  
  mean_temp <- mean(subset_data$Maximum.temperature..Degree.C., na.rm = TRUE)
  sd_temp <- sd(subset_data$Maximum.temperature..Degree.C., na.rm = TRUE)
  skewness_temp <- skewness(subset_data$Maximum.temperature..Degree.C., na.rm = TRUE)
  kurtosis_temp <- kurtosis(subset_data$Maximum.temperature..Degree.C., na.rm = TRUE)
  median_temp <- median(subset_data$Maximum.temperature..Degree.C., na.rm = TRUE)
  iqr_temp <- IQR(subset_data$Maximum.temperature..Degree.C., na.rm = TRUE)
  # calculating various descriptive statistics for the "Maximum Temperature" column within the subset_data DataFrame. These statistics include the mean, standard deviation, skewness, kurtosis, median, and interquartile range (IQR)
  
  cat("Descriptive Statistics for", month, "/", year, ":\n")
  cat("Mean:", round(mean_temp, 2), "\n")
  cat("Standard Deviation:", round(sd_temp, 2), "\n")
  cat("Skewness:", round(skewness_temp, 2), "\n")
  cat("Kurtosis:", round(kurtosis_temp, 2), "\n")
  cat("Median:", round(median_temp, 2), "\n")
  cat("IQR:", round(iqr_temp, 2), "\n")
  # printing the calculated descriptive statistics, along with the specified month and year, and rounding them to two decimal places for better interpretation.
  
  return(subset_data)
}

# Read maximum temperature data
maximum_data$Year <- as.numeric(maximum_data$Year)
maximum_data$Month <- as.numeric(maximum_data$Month)
maximum_data$Maximum.temperature..Degree.C. <- as.numeric(ifelse(is.na(maximum_data$Maximum.temperature..Degree.C.), NA, maximum_data$Maximum.temperature..Degree.C.))

# Select and analyze data for the specified years and months (1961 and 2021)
year_1961 <- 1961
year_2021 <- 2021
month_to_analyze <- 7  # July is the chosen month

data_1961 <- calculate_descriptive_stats(maximum_data, year_1961, month_to_analyze)
data_2021 <- calculate_descriptive_stats(maximum_data, year_2021, month_to_analyze)

# Data visualization to compare temperatures in 1961 and 2021
if (!is.null(data_1961) && !is.null(data_2021)) {
  combined_data <- rbind(data_1961, data_2021)
  ggplot(combined_data, aes(x = Year, y = Maximum.temperature..Degree.C., color = as.factor(Year))) +
    geom_boxplot() +
    labs(title = paste("Comparison of Maximum Temperatures for Month", month_to_analyze, "in 1961 and 2021"),
         x = "Year",  # Change the x-axis label
         y = "Maximum Temperature in C")  # Change the y-axis label
}


# Question 3
# Because the two datasets have different number of rows we cannot combine them by column names, so we merge them instead
# Merge the data frames by date (Year, Month, Day)
combined_data <- merge(maximum_data, minimum_data, by = c("Year", "Month", "Day"))

# Calculate and insert the daily temperature range into the dataset
combined_data$Temperature_Range <- combined_data$Maximum.temperature..Degree.C. - combined_data$Minimum.temperature..Degree.C.

# Visualize the distribution and changes in temperature range over time
ggplot(combined_data, aes(x = Year, y = Temperature_Range)) +
  geom_boxplot() +
  labs(title = "Distribution and Changes in Daily Temperature Range Over Time",
       x = "Year",
       y = "Temperature Range (°C)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a histogram to visualize the distribution of temperature range
hist_plot <- ggplot(combined_data, aes(x = Temperature_Range)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Daily Temperature Range",
       x = "Temperature Range (°C)",
       y = "Frequency")
hist_plot

# Question 4
# Count number of days per year with maximal temperature equal or above 35 degrees
hot_days <- maximum_data[maximum_data$Maximum.temperature..Degree.C. > 35.0, ]
hot_days_count <- table(hot_days$Year)
hot_days_count

barplot(hot_days_count, main="Number of Hot Days per Year (Max Temp >= 35°C)",
        xlab="Year", ylab="Count", col="blue")

# Repeating the process for a minimum temp of 10°C
cold_days <- minimum_data[minimum_data$Minimum.temperature..Degree.C. < 10, ]
cold_days_count <- table(cold_days$Year)
cold_days_count

barplot(cold_days_count, main="Number of Cold Days per Year (Min Temp < 10°C)",
        xlab="Year", ylab="Count", col="red")

# Combining the two:
# Count the number of hot days per year
hot_days_count <- table(hot_days$Year)

# Count the number of cold days per year
cold_days_count <- table(cold_days$Year)

# Create a bar plot to visualize both hot and cold days
barplot(rbind(hot_days_count, cold_days_count), beside = TRUE,
        legend.text = c("Hot Days", "Cold Days"), main = "Hot and Cold Days per Year",
        xlab = "Year", ylab = "Count", col = c("blue", "red"))

