# Data Import and Initial Exploration 
#Import the AirbnbLA_2023_Cleaned.csv file from your folder.
library(readr)
library(dplyr)
view(read_csv("data/AirbnbLA_2023_Cleaned.csv"))
data_cleaned<- read_csv ("data/AirbnbLA_2023_Cleaned.csv")


#1. Analyzing Average Price
#Average price by property type
average_price_by_type <- data_cleaned %>%
  group_by(`RoomType`) %>%
  summarize(Average_Price = mean(Price, na.rm = TRUE))

# Average price by number of beds
average_price_by_beds <- data_cleaned %>%
  group_by(Beds) %>%
  summarize(Average_Price = mean(Price, na.rm = TRUE))

# Calculate average price by neighbourhood and sort in descending order
average_price_by_neighbourhood <- data_cleaned %>%
  group_by(Neighbourhood) %>%
  summarize(Average_Price = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(Average_Price))
# Select the top 10 neighbourhoods
top_10_neighbourhoods <- head(average_price_by_neighbourhood, 10)
# Print the top 10 neighbourhoods
print(top_10_neighbourhoods)

# Viewing Results

print(average_price_by_type)
print(average_price_by_beds,n=22)
print(average_price_by_neighbourhood)

#3. Analyze Review Scores for "Value" by Price Range
# Create a price range column
data <- data_cleaned %>%
  mutate(Price_Range = cut(Price, 
                           breaks = c(0, 100, 200, 300, 500, 1000, Inf), 
                           labels = c("$0-$100", "$101-$200", "$201-$300", "$301-$500", "$501-$1000", "$1000+")))
# Calculate average review score for "value" by price range
review_scores_by_price_range <- data %>%
  group_by(Price_Range) %>%
  summarize(Average_Review_Score_for_Value = mean(`ReviewScoresValue`, na.rm = TRUE))
# Print the results
print(review_scores_by_price_range)

#4. Is there a relationship between the cleanliness rating and the ""value"" rating for different listings?
# Calculate the correlation between cleanliness rating and value rating
correlation <- cor(data_cleaned$`ReviewScoresCleanliness`, data_cleaned$`ReviewScoresValue`, use = "complete.obs")

# Print the correlation value
print(paste("Correlation between cleanliness rating and value rating:", round(correlation, 2)))
#Correlation between cleanliness rating and value rating: 0.78

# Create a scatter plot to visualize the relationship
plot(data_cleaned$`ReviewScoresCleanliness`, data_cleaned$`ReviewScoresValue`,
     main = "Relationship Between Cleanliness and Value Ratings",
     xlab = "Cleanliness Rating",
     ylab = "Value Rating",
     pch = 19, col = "blue")

#5. Are there particular neighborhoods where pricing seems more correlated with higher review scores or satisfaction?
# Calculate the average price and review scores for each neighborhood
neighborhood_summary <- data %>%
  group_by(Neighbourhood) %>%
  summarize(
    Average_Price = mean(Price, na.rm = TRUE),
    Average_Review_Score = mean(ReviewScoresRating, na.rm = TRUE)
  )
# Check if 'neighborhood_summary' was created successfully
print(head(neighborhood_summary))

# Calculate the correlation between average price and average review score for neighborhoods
correlation <- cor(neighborhood_summary$Average_Price, neighborhood_summary$Average_Review_Score, use = "complete.obs")

# Print the correlation value
print(paste("Correlation between average price and average review score by neighborhood:", round(correlation, 2)))
#Correlation between average price and average review score by neighborhood: 0.24

# Create a scatter plot to visualize the relationship
plot(neighborhood_summary$Average_Price, neighborhood_summary$Average_Review_Score,
     main = "Price vs. Review Score by Neighborhood",
     xlab = "Average Price",
     ylab = "Average Review Score",
     pch = 19, col = "green")


