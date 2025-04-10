#Load Library
library(dplyr)
library(ggplot2)

#Import CSV
dataset <- read.csv("retail_data.csv", header = TRUE)

#Show dataset structure
str(dataset)

#*/
# Cleaning Data
#/*

#Check for na values
colSums(is.na(dataset))

#Handle na and empty string values
dataset <- dataset %>%
  mutate(
    #use across to apply multiple changes
    #Change numeric field na to median
    across(c(Age, Year, Total_Purchases, Amount, Total_Amount), ~ as.numeric(ifelse(is.na(.), median(., na.rm = TRUE), .))),
    
    #Trim white spaces and convert to factor for better memory management
    #Change character field empty string to na
    across(c(Gender, City, Customer_Segment, Product_Category,
             Product_Brand, Product_Type, Feedback, Shipping_Method,
             Payment_Method, Order_Status, Ratings),
           ~ as.factor(trimws(ifelse(. == "", NA_character_, as.character(.))))),
    
  )

#Verify the numeric field contain no na, and the character empty string field is converted to na
colSums(is.na(dataset))

#Show summary
summary(dataset)

#Examples

# Data Aggregation: Average ratings per Product Brand
avg_ratings_brand <- dataset %>%
  group_by(Product_Brand) %>%
  summarise(
            Count = n()) %>%
  arrange(desc(Average_Rating))
print(avg_ratings_brand)

#Explore Distribution(char)
dataset %>%
  group_by(Gender) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Gender, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Gender")

# Explore distribution of Product_Type (categorical)
dataset %>%
  group_by(Product_Type) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Product_Type, y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() + # For better readability if many categories
  labs(title = "Distribution of Product Types")

# Gender vs. Product_Category (categorical vs. categorical)
dataset %>%
  group_by(Gender, Product_Category) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Product_Category, y = count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") + # or "stack"
  labs(title = "Distribution of Product Category by Gender")

