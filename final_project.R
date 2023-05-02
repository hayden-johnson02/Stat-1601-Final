library('ggplot2')

# Read, clean column names, and remove columns
allData <- read.csv("mxmh_survey_results.csv")
cleanedData <- allData[,c(2,4,6,8,11,28,29,30,31,32)]
cleanedData$Fav.genre <- gsub("Video game music", "Games", cleanedData$Fav.genre)

# Create age categories and filter rows by their age
cleanedData$Age.Category <- cut(cleanedData$Age, breaks = c(10, 30, 50, 70, 90), labels = c("10-30", "30-50", "50-70", "70-90"), include.lowest = TRUE)
cleanedData <- cleanedData[!is.na(cleanedData$Age.Category),]

# Create hours per day categories and filter rows by their hours per day
cleanedData$Hours.Category <- cut(cleanedData$Hours.per.day, breaks = seq(0, 15, length.out = 5), labels = c("0-3.75", "3.75-7.5", "7.5-11.25", "11.25-15"), include.lowest = TRUE)
cleanedData <- cleanedData[!is.na(cleanedData$Hours.Category),]

# Remove all values in the instrumentalist column that aren't Yes or No
filteredData <- cleanedData[cleanedData$Instrumentalist == "Yes" | cleanedData$Instrumentalist == "No",]

# Filter data for only Folk and Latin genres
folk_latin_data <- cleanedData[cleanedData$Fav.genre %in% c("Folk", "Latin"),]

# Create anxiety bar chart
anxietyChart <- ggplot(cleanedData, aes(x = reorder(Fav.genre, -Anxiety, FUN = mean), y = Anxiety)) + 
  ylab("Anxiety (Out of 10)") +
  xlab("Favorite Genre") + 
  labs(color = "Favorite Genre", fill = "Favorite Genre") +
  geom_bar(stat = "summary", fun = "mean", width = 0.4, color = "red", fill = "red") +
  ggtitle("Average Anxiety Based on Favorite Genre of Music") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Create box plot comparing Folk and Latin genres
folk_latin_plot <- ggplot(folk_latin_data, aes(x = Fav.genre, y = Anxiety, fill = Fav.genre)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue"), name = "Favorite Genre") +
  xlab("Genre") +
  ylab("Anxiety (Out of 10)") +
  ggtitle("Comparison of Anxiety between Folk and Latin Genres") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Create the box plot for anxiety by age range
anxiety_age_range <- ggplot(cleanedData, aes(x = Age.Category, y = Anxiety)) +
  geom_boxplot() +
  labs(title = "Comparison of Anxiety by Age Range", x = "Age Range", y = "Anxiety (Out of 10)") +
  theme(plot.title = element_text(hjust = 0.5))

# Set the desired fraction of data points to display in the regression chart
fraction <- 0.15

# Sample a fraction of the data points; will always be the same
set.seed(42)
sampledData <- cleanedData[sample(nrow(cleanedData), floor(nrow(cleanedData) * fraction)),]

# Create a regression chart with individual responses shown
age_anxiety_plot <- ggplot(sampledData, aes(x = Age, y = Anxiety)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid") +
  labs(title = "Anxiety vs Age", x = "Age", y = "Anxiety (Out of 10)") +
  theme(plot.title = element_text(hjust = 0.5))

# Create a density plot based on instrumentalist status
instrumentalist_density_plot <- ggplot(filteredData, aes(x = Anxiety, fill = Instrumentalist)) +
  geom_density(alpha = 0.5) +
  labs(title = "Anxiety Distribution by Instrumentalist Status", x = "Anxiety Level", y = "Density") +
  facet_wrap(~Instrumentalist, ncol = 2, strip.position = "bottom", labeller = label_both) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing.x = unit(0.5, "cm"))

# Create box plot for hours listening to music per day
hours_per_day <- ggplot(cleanedData, aes(x = Hours.Category, y = Anxiety)) +
  geom_boxplot() +
  labs(title = "Anxiety by Hours Listening to Music per Day", x = "Hours Spent Listening to Music Per Day", y = "Anxiety (Out of 10)") +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plots
print(anxietyChart)
print(folk_latin_plot)
print(anxiety_age_range)
print(instrumentalist_density_plot)
print(hours_per_day)
